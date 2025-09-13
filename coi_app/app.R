# app.R
# Cost of Inaction (CoI) Explorer — UNICEF COs
# movimentar GmbH

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyvalidate)
  library(tidyverse)
  library(scales)
  library(gt)
  library(targets)
  suppressWarnings(try(library(httr2),      silent = TRUE)) # optional (AI narrative)
  suppressWarnings(try(library(commonmark), silent = TRUE)) # for Markdown -> HTML
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------- tiny helpers to avoid is.character errors ----------
as_chr     <- function(x) toString(x)                           # length-1 character
fmt_dollar <- function(x) as_chr(scales::dollar(x))
fmt_comma  <- function(x) as_chr(scales::comma(x))
fmt_bcr    <- function(x) as_chr(ifelse(is.na(x), "—", sprintf("%.2f", x)))

# ---------- Gemini helpers (REST; header x-goog-api-key) ----------
gemini_generate_rest <- function(prompt,
                                 model = c("gemini-2.5-flash","gemini-2.5-pro","gemini-2.0-flash"),
                                 max_tokens = 400L,
                                 temperature = 0.2,
                                 thinking_budget = NULL,   # 0 disables “thinking” on 2.5 Flash
                                 api_key = Sys.getenv("GEMINI_API_KEY"),
                                 verbose = FALSE) {
  model <- match.arg(model)
  if (!nzchar(api_key)) stop("GEMINI_API_KEY not set.")
  
  endpoint <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent",
    model
  )
  
  body <- list(
    contents = list(list(parts = list(list(text = prompt)))),
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = as.integer(max_tokens)
    )
  )
  if (!is.null(thinking_budget) && grepl("^gemini-2\\.5", model)) {
    body$generationConfig$thinkingConfig <- list(thinkingBudget = as.integer(thinking_budget))
  }
  
  resp <- httr2::request(endpoint) |>
    httr2::req_headers(
      "Content-Type"   = "application/json",
      "X-Goog-Api-Key" = api_key
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()
  
  httr2::resp_check_status(resp)
  out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  
  if (!is.null(out$promptFeedback) && !is.null(out$promptFeedback$blockReason)) {
    br <- out$promptFeedback$blockReason
    return(sprintf("Response blocked by safety system (blockReason: %s).", br))
  }
  
  extract_text_parts <- function(candidate) {
    if (is.null(candidate$content) || is.null(candidate$content$parts)) return(character(0))
    parts <- candidate$content$parts
    texts <- unlist(lapply(parts, function(p) if (!is.null(p$text)) p$text else NULL), use.names = FALSE)
    texts
  }
  texts <- unlist(lapply(out$candidates %||% list(), extract_text_parts), use.names = FALSE)
  
  if (length(texts) == 0) {
    if (verbose) {
      cat("No text parts found; keys:", paste(names(out), collapse = ", "), "\n")
      if (!is.null(out$candidates)) str(out$candidates, max.level = 2)
    }
    return("Gemini returned no text.")
  }
  paste(texts, collapse = "\n\n")
}

gemini_generate_rest_retry <- function(..., max_retries = 5L, base_delay = 1.5) {
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    out <- try(gemini_generate_rest(...), silent = TRUE)
    if (!inherits(out, "try-error") && !grepl("^HTTP\\s*\\d+|failed|no text", out, ignore.case = TRUE)) return(out)
    
    msg <- if (inherits(out, "try-error")) conditionMessage(out) else out
    if (!grepl("429|Too Many Requests|Resource exhausted|503", msg, ignore.case = TRUE) ||
        attempt > max_retries) return(paste("Gemini call failed:", msg))
    
    Sys.sleep(base_delay * (2^(attempt - 1)) * runif(1, 0.8, 1.2))  # backoff + jitter
  }
}

# ---------- narrative prompt ----------
build_coi_prompt <- function(payload) {
  sprintf(paste(
    "You are a Cost of Inaction Assistant for UNICEF, NGOs and government stakeholders. Write a concise narrative (150–220 words) ",
    "in British English, formal tone, followed by 3–5 bullet key messages for advocacy. Base your points ONLY ",
    "on the provided figures.\n\n",
    "Context\n",
    "• Emergency: %s\n",
    "• Planning period: %d year(s)\n",
    "• Valuation for indirect benefits: %s\n",
    "Inputs\n",
    "• PiN children 0–59 months: %s\n",
    "• PiN PLW: %s\n",
    "• Target coverage: %d%%\n",
    "Results (2015 USD where applicable)\n",
    "• Total estimated cost: %s\n",
    "• Indirect economic benefits: %s\n",
    "• Direct benefits (headline count): %s\n",
    "• Benefit–Cost Ratio: %s\n\n",
    "Instructions\n",
    "- Be precise and neutral; avoid hype.\n",
    "- Use short sentences and clear structure.\n",
    "- Translate numbers into planning implications (funding, phasing, priority groups).\n",
    "- Do not invent data. If something is uncertain, say so briefly.\n",
    "- End with 3–5 bullet recommendations for advocacy and planning.",
    "- Analyse the results instead of listing them, including policy implications.",
    "- Do not use the word necessitate.",
    "- Make it clear that these are estimates based on a simulation considering the user inputs. Starting the text with a disclaimar like that could be good.",
    sep = ""
  ),
  payload$emergency,
  as.integer(payload$years),
  payload$valuation,
  scales::comma(round(payload$pin_children)),
  scales::comma(round(payload$pin_plw)),
  as.integer(payload$coverage),
  scales::dollar(round(payload$total_cost)),
  scales::dollar(round(payload$indir_total)),
  scales::comma(round(payload$direct_total)),
  if (is.na(payload$bcr)) "N/A" else sprintf("%.2f", payload$bcr))
}

# ---------- data loading (RDS first, then {targets}) ----------
rds_dir <- file.path(getwd(), "data")

load_from_rds_or_targets <- function(name, use_targets_fallback = TRUE, store = "_targets") {
  rds_path <- file.path(rds_dir, paste0(name, ".rds"))
  if (file.exists(rds_path)) return(readRDS(rds_path))
  
  if (use_targets_fallback) {
    if (!requireNamespace("targets", quietly = TRUE)) {
      stop(sprintf("'%s.rds' not found and {targets} not installed.", name))
    }
    if (!dir.exists(store)) {
      alt <- file.path("..", "_targets")
      if (dir.exists(alt)) store <- alt else stop(sprintf("'%s.rds' not found and targets store '%s' is missing.", name, store))
    }
    targets::tar_config_set(store = store)
    return(targets::tar_read_raw(name))
  }
  stop(sprintf("Data object '%s' not found in ./data and targets fallback disabled.", name))
}

# ---------- load precomputed objects ----------
intervention_list  <- load_from_rds_or_targets("intervention_list")
coi_costs          <- load_from_rds_or_targets("coi_costs")
coi_coverages      <- load_from_rds_or_targets("coi_coverages")
coi_dir_benefits   <- load_from_rds_or_targets("coi_dir_benefits")
coi_indir_benefits <- load_from_rds_or_targets("coi_indir_benefits")

# Optional PV inputs
gni_forecast <- try(load_from_rds_or_targets("gni_forecast"), silent = TRUE)
if (inherits(gni_forecast, "try-error")) gni_forecast <- NULL
income_share <- try(load_from_rds_or_targets("income_share"), silent = TRUE)
if (inherits(income_share, "try-error")) income_share <- NULL

# ---------- tidy anchor tables ----------
direct_tbl <- bind_rows(
  coi_dir_benefits$malnutrition %>%
    mutate(source_tbl = "malnutrition"),
  coi_dir_benefits$breastfeeding %>%
    mutate(source_tbl = "breastfeeding") %>%
    mutate(across(c(saved_implemented, saved_30, saved_95), abs))
) %>%
  select(emergency,
         indicator_category = source_tbl,
         indicator_name = indicator_name_absolute,
         coverage_0,
         `30` = saved_30,
         `95` = saved_95)

indir_tbl_undisc <- coi_indir_benefits %>%
  ungroup() %>%
  mutate(across(contains("Eta-Iota"), abs),
         across(contains("Migration"), abs)) %>%
  select(
    indicator_name = indicator_name_absolute,
    matches("^(Eta-Iota|Migration\\sflows)_(implemented|30|95)$")
  ) %>%
  pivot_longer(
    cols = -indicator_name,
    names_to   = c("emergency", "anchor"),
    names_sep  = "_",
    values_to  = "value"
  )

# ---------- helpers ----------
ANCHORS <- c(0, 30, 95)

interp_3pt <- function(y0, y30, y95, x_star) {
  if (is.na(x_star)) return(NA_real_)
  if (x_star <= 30) {
    y0 + (y30 - y0) * (x_star - 0) / 30
  } else if (x_star <= 95) {
    y30 + (y95 - y30) * (x_star - 30) / (95 - 30)
  } else {
    slope <- (y95 - y30) / (95 - 30)
    y95 + slope * (x_star - 95)
  }
}

annualise <- function(total_over_study, study_years = 3) {
  ifelse(is.finite(study_years) && study_years > 0, total_over_study / study_years, total_over_study)
}

# Baseline PiN proxies per emergency
pin_baseline <- coi_costs %>%
  ungroup() %>%
  group_by(emergency) %>%
  summarise(
    baseline_children = sum(ideal_delivered[target_group %in% c("Children 0–59 months","Children 0-59 months")], na.rm = TRUE),
    baseline_plw      = sum(ideal_delivered[target_group %in% c("PLW","Pregnant and Lactating Women")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    total_need = sum(baseline_children, baseline_plw, na.rm = TRUE),
    baseline_children = ifelse(baseline_children == 0, total_need * 0.6, baseline_children),
    baseline_plw      = ifelse(baseline_plw      == 0, total_need * 0.4, baseline_plw)
  ) %>%
  ungroup() %>%
  select(-total_need)

# PV engine (optional)
EARNINGS_UPLIFT <- 2.62 * 0.01067 # 2.796%
has_pv_inputs <- !(is.null(gni_forecast) || is.null(income_share))

pv_compute_by_em <- function(dir_b_bf, gni_forecast, income_share, rate = 0.03, base_year = 2022) {
  labour_share <- income_share %>%
    filter(ref_area %in% c("GTM","HND","NIC","COL","PER"), time == "2020") %>%
    pull(obs_value) %>%
    mean(na.rm = TRUE)
  
  gni_by_em <- gni_forecast %>%
    filter(year >= 2038, year <= 2080) %>%
    select(emergency, year, real_gni_pc_const_usd) %>%
    group_by(emergency) %>%
    summarise(years  = list(year),
              gni_pc = list(real_gni_pc_const_usd),
              .groups = "drop")
  
  add_ebf <- dir_b_bf %>%
    ungroup() %>%
    filter(indicator_name_absolute == "Exclusively breastfed children") %>%
    mutate(across(c(saved_implemented, saved_30, saved_95), abs)) %>%
    select(emergency, saved_implemented, saved_30, saved_95) %>%
    left_join(gni_by_em, by = "emergency")
  
  pv_one <- function(n_children, gni_pc_vec, years_vec, rate) {
    annual <- gni_pc_vec * (labour_share / 100) * EARNINGS_UPLIFT * n_children
    df     <- (1 + rate)^(years_vec - base_year)
    sum(annual / df, na.rm = TRUE)
  }
  
  add_ebf %>%
    rowwise() %>%
    mutate(
      pv_impl = pv_one(saved_implemented, gni_pc, years, rate),
      pv_30   = pv_one(saved_30,          gni_pc, years, rate),
      pv_95   = pv_one(saved_95,          gni_pc, years, rate)
    ) %>%
    ungroup() %>%
    select(emergency, pv_impl, pv_30, pv_95)
}

pv_tab_3  <- if (has_pv_inputs) pv_compute_by_em(coi_dir_benefits$breastfeeding, gni_forecast, income_share, rate = 0.03) else NULL
pv_tab_5  <- if (has_pv_inputs) pv_compute_by_em(coi_dir_benefits$breastfeeding, gni_forecast, income_share, rate = 0.05) else NULL

# ---------- UI ----------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "CoI Explorer",
  # Narrative styling
  tags$style(HTML("
    .narrative { white-space: normal; font-family: var(--bs-body-font-family); }
    .narrative p { margin-bottom: 0.6rem; }
    .narrative ul { margin-left: 1.2rem; }
    .narrative h4, .narrative h5 { margin-top: 0.2rem; }
  ")),
  layout_columns(
    col_widths = c(4, 8),
    card(
      header = "Inputs",
      selectInput("emergency", "Emergency context",
                  choices = sort(unique(coi_costs$emergency)),
                  selected = sort(unique(coi_costs$emergency))[1]),
      numericInput("pin_children", "PiN — Children 0–59 months", value = 100000, min = 0, step = 1000),
      numericInput("pin_plw", "PiN — PLW", value = 40000, min = 0, step = 1000),
      sliderInput("coverage", "Target coverage (%)", min = 0, max = 100, value = 50, step = 1),
      numericInput("years", "Planning period (years)", value = 3, min = 1, step = 1),
      radioButtons("valuation",
                   "Indirect benefits valuation",
                   choices = c("Undiscounted" = "undisc",
                               "PV (3%)"     = "pv3",
                               "PV (5%)"     = "pv5"),
                   selected = "undisc"),
      accordion(
        accordion_panel("Advanced",
                        numericInput("study_years", "Study duration for annualisation (years)", value = 3, min = 1, step = 1),
                        checkboxInput("override_costs", "Adjust median costs", value = FALSE),
                        conditionalPanel(
                          "input.override_costs == true",
                          sliderInput("cost_adj", "Cost adjustment factor (×)", min = 0.25, max = 2.50, value = 1.00, step = 0.05),
                          helpText("Apply proportional change to total costs (for local price levels or logistics).")
                        ),
                        checkboxInput("enable_ai", "Enable automated narrative (Gemini API key required)", value = FALSE)
        )
      )
    ),
    card(
      header = "Results",
      layout_columns(
        col_widths = c(6,6),
        card(
          card_header("Total response cost"),
          h4(textOutput("total_cost"), class = "mt-2")
        ),
        card(
          card_header("Indirect benefits"),
          h4(textOutput("indir_benefit"), class = "mt-2")
        )
      ),
      layout_columns(
        col_widths = c(6,6),
        card(
          card_header("Direct benefits — headline (count)"),
          h4(textOutput("direct_headline"), class = "mt-2")
        ),
        card(
          card_header("Benefit–Cost Ratio"),
          h4(textOutput("bcr"), class = "mt-2")
        )
      ),
      hr(),
      plotOutput("benefit_plot", height = 320),
      hr(),
      h5("Planning summary"),
      gt_output("summary_gt"),
      hr(),
      h5("Direct benefits by indicator"),
      gt_output("direct_detail_gt"),
      div(class = "mt-2",
          downloadButton("dl_direct_csv", "Download indicators (CSV)")
      ),
      hr(),
      h5("Automated interpretation"),
      uiOutput("narrative"),
      hr(),
      downloadButton("dl_csv", "Download CSV"),
      downloadButton("dl_xlsx", "Download XLSX")
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # Validation
  iv <- InputValidator$new()
  iv$add_rule("pin_children", sv_gte(0))
  iv$add_rule("pin_plw", sv_gte(0))
  iv$add_rule("years", sv_between(1, 50))
  iv$add_rule("study_years", sv_between(1, 50))
  iv$enable()
  
  pin_scale <- reactive({
    req(iv$is_valid())
    base <- pin_baseline %>% filter(emergency == input$emergency)
    if (!nrow(base)) return(list(children = 1, plw = 1))
    list(
      children = ifelse(base$baseline_children > 0, input$pin_children / base$baseline_children, 1),
      plw      = ifelse(base$baseline_plw      > 0, input$pin_plw      / base$baseline_plw, 1)
    )
  })
  
  direct_interp <- reactive({
    req(iv$is_valid())
    df0 <- direct_tbl %>% filter(emergency == input$emergency)
    req(nrow(df0) > 0)
    
    df0 %>%
      mutate(
        value_cov = pmap_dbl(
          list(coverage_0, `30`, `95`),
          ~ interp_3pt(..1, ..2, ..3, input$coverage)
        ),
        scale_group = ifelse(
          grepl("breastfeed|ebf|exclusive", indicator_name, ignore.case = TRUE),
          pin_scale()$children,
          mean(c(pin_scale()$children, pin_scale()$plw), na.rm = TRUE)
        ),
        value_scaled = value_cov * scale_group
      )
  })
  
  # Per-indicator counts (interpolated, PiN-scaled, annualised, extended)
  direct_by_indicator <- reactive({
    req(iv$is_valid())
    df0 <- direct_tbl %>% filter(emergency == input$emergency)
    req(nrow(df0) > 0)
    
    df0 %>%
      mutate(
        value_cov = pmap_dbl(
          list(coverage_0, `30`, `95`),
          ~ interp_3pt(..1, ..2, ..3, input$coverage)
        ),
        scale_group = ifelse(
          grepl("breastfeed|ebf|exclusive", indicator_name, ignore.case = TRUE),
          pin_scale()$children,
          mean(c(pin_scale()$children, pin_scale()$plw), na.rm = TRUE)
        ),
        value_scaled_study = value_cov * scale_group,
        per_year           = annualise(value_scaled_study, input$study_years),
        total_planning     = per_year * input$years
      ) %>%
      select(indicator_category, indicator_name, per_year, total_planning) %>%
      filter(is.finite(total_planning) & abs(total_planning) > 0) %>%
      arrange(indicator_category, desc(total_planning))
  })
  
  indir_interp <- reactive({
    req(iv$is_valid())
    agg_scale <- mean(c(pin_scale()$children, pin_scale()$plw), na.rm = TRUE)
    
    if (input$valuation == "undisc") {
      df0 <- indir_tbl_undisc %>% filter(emergency == input$emergency)
      req(nrow(df0) > 0)
      anchors <- df0 %>%
        pivot_wider(names_from = anchor, values_from = value) %>%
        mutate(`0` = 0)
      
      out <- anchors %>%
        mutate(
          value_cov = pmap_dbl(list(`0`, `30`, `95`),
                               ~ interp_3pt(..1, ..2, ..3, input$coverage)),
          value_scaled = value_cov * agg_scale
        )
      tibble(total = sum(out$value_scaled, na.rm = TRUE), detail = out)
      
    } else {
      if (!has_pv_inputs)
        return(tibble(total = NA_real_, detail = tibble(), note = "PV inputs not available; select Undiscounted."))
      
      pv_tab <- if (input$valuation == "pv3") pv_tab_3 else pv_tab_5
      row <- pv_tab %>% filter(emergency == input$emergency)
      req(nrow(row) > 0)
      
      y0  <- 0
      y30 <- ifelse(is.finite(row$pv_30), row$pv_30, row$pv_impl)
      y95 <- row$pv_95
      val <- interp_3pt(y0, y30, y95, input$coverage)
      
      total_scaled <- as.numeric(val) * agg_scale
      tibble(
        total = total_scaled,
        detail = tibble(
          indicator_name = "PV of cognitive-income benefits",
          emergency = input$emergency,
          value_scaled = total_scaled
        )
      )
    }
  })
  
  cost_interp <- reactive({
    req(iv$is_valid())
    base <- coi_costs %>% filter(emergency == input$emergency)
    req(nrow(base) > 0)
    
    ideal_cost_tot <- sum(base$ideal_cost, na.rm = TRUE)
    impl_cost_tot  <- sum(base$total_cost, na.rm = TRUE)
    impl_cov       <- mean(base$global_coverage, na.rm = TRUE)
    
    slope95 <- ifelse(is.finite(ideal_cost_tot), ideal_cost_tot / 95, NA_real_)
    slopeI  <- ifelse(is.finite(impl_cost_tot) && is.finite(impl_cov) && impl_cov > 0,
                      impl_cost_tot / impl_cov, NA_real_)
    
    slope <- mean(c(slope95, slopeI), na.rm = TRUE)
    if (!is.finite(slope)) slope <- 0
    
    cov_cost   <- slope * input$coverage
    pin_factor <- mean(c(pin_scale()$children, pin_scale()$plw), na.rm = TRUE)
    total <- cov_cost * pin_factor
    
    if (isTRUE(input$override_costs)) total <- total * input$cost_adj
    tibble(cost_total_study = total)
  })
  
  results <- reactive({
    req(nrow(direct_interp()) > 0, nrow(cost_interp()) > 0)
    study_years <- input$study_years
    years       <- input$years
    
    direct_total_study <- sum(direct_interp()$value_scaled, na.rm = TRUE)
    indir_total_study  <- indir_interp()$total
    cost_total_study   <- cost_interp()$cost_total_study[1]
    
    direct_total <- annualise(direct_total_study, study_years) * years
    indir_total  <- annualise(indir_total_study,  study_years) * years
    cost_total   <- annualise(cost_total_study,   study_years) * years
    
    bcr <- ifelse(cost_total > 0, indir_total / cost_total, NA_real_)
    
    list(
      direct_total = direct_total,
      indir_total  = indir_total,
      cost_total   = cost_total,
      bcr          = bcr
    )
  })
  
  # ---------- Outputs ----------
  output$total_cost      <- renderText({ req(results()$cost_total); fmt_dollar(round(results()$cost_total)) })
  output$indir_benefit   <- renderText({ req(results()$indir_total); fmt_dollar(round(results()$indir_total)) })
  output$direct_headline <- renderText({ req(results()$direct_total); fmt_comma(round(results()$direct_total)) })
  output$bcr             <- renderText({ req(!is.null(results()$bcr)); fmt_bcr(results()$bcr) })
  
  output$benefit_plot <- renderPlot({
    req(results())
    df <- tibble(
      metric = factor(c("Indirect benefits", "Cost"),
                      levels = c("Indirect benefits", "Cost")),
      usd = c(results()$indir_total %||% NA_real_,
              results()$cost_total  %||% NA_real_)
    )
    req(all(is.finite(df$usd)))
    
    ggplot(df, aes(metric, usd)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = dollar(usd)), vjust = -0.3) +
      scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0, 0.08))) +
      labs(x = NULL, y = "USD", title = "Costs and indirect benefits over the planning period") +
      theme_minimal(base_size = 12)
  })
  
  output$summary_gt <- render_gt({
    req(results())
    tibble(
      Item = c(
        "Emergency",
        "PiN — Children 0–59m",
        "PiN — PLW",
        "Target coverage (%)",
        "Valuation (indirect benefits)",
        "Planning period (years)",
        "Study duration (years)",
        "Total response cost (USD)",
        "Indirect benefits (USD)",
        "Direct benefits — headline (count)",
        "Benefit–Cost Ratio"
      ),
      Value = c(
        input$emergency,
        comma(input$pin_children),
        comma(input$pin_plw),
        paste0(input$coverage, "%"),
        switch(input$valuation, undisc = "Undiscounted", pv3 = "PV (3%)", pv5 = "PV (5%)"),
        input$years,
        input$study_years,
        dollar(round(results()$cost_total)),
        dollar(round(results()$indir_total)),
        comma(round(results()$direct_total)),
        ifelse(is.na(results()$bcr), "—", round(results()$bcr, 2))
      )
    ) %>%
      gt() %>%
      tab_header(title = "CoI planning summary") %>%
      cols_align("left", columns = Item) %>%
      cols_align("center", columns = Value)
  })
  
  output$direct_detail_gt <- render_gt({
    tb <- direct_by_indicator()
    req(nrow(tb) > 0)
    tb %>%
      mutate(
        per_year       = round(per_year),
        total_planning = round(total_planning)
      ) %>%
      gt(groupname_col = "indicator_category", rowname_col = "indicator_name") %>%
      tab_header(title = "Direct benefits by indicator") %>%
      cols_label(
        per_year       = "Per year (count)",
        total_planning = "Planning period (count)"
      ) %>%
      fmt_number(columns = c(per_year, total_planning), decimals = 0, use_seps = TRUE) %>%
      cols_align("center", columns = c(per_year, total_planning))
  })
  
  output$dl_direct_csv <- downloadHandler(
    filename = function() paste0("coi_direct_indicators_", Sys.Date(), ".csv"),
    content  = function(file) {
      readr::write_csv(
        direct_by_indicator() %>%
          mutate(
            per_year       = round(per_year),
            total_planning = round(total_planning)
          ),
        file
      )
    }
  )
  
  # ---------- Automated narrative (Markdown -> HTML) ----------
  output$narrative <- renderUI({
    if (!isTRUE(input$enable_ai)) {
      return(tags$p(class = "text-muted narrative",
                    "Automated interpretation is disabled. Enable it in Advanced to request a short narrative."))
    }
    if (!requireNamespace("httr2", quietly = TRUE) || Sys.getenv("GEMINI_API_KEY") == "") {
      return(tags$p(class = "text-danger narrative",
                    "No API key found or {httr2} not installed. Set GEMINI_API_KEY and restart."))
    }
    
    p <- list(
      emergency    = input$emergency,
      years        = input$years,
      valuation    = switch(input$valuation, undisc = "Undiscounted", pv3 = "PV (3%)", pv5 = "PV (5%)"),
      pin_children = input$pin_children,
      pin_plw      = input$pin_plw,
      coverage     = input$coverage,
      total_cost   = results()$cost_total,
      indir_total  = results()$indir_total,
      direct_total = results()$direct_total,
      bcr          = results()$bcr
    )
    prompt <- build_coi_prompt(p)
    
    txt <- gemini_generate_rest_retry(
      prompt = prompt,
      model = "gemini-2.5-flash",
      temperature = 0.2,
      max_tokens = 700,
      thinking_budget = 0
    )
    
    if (requireNamespace("commonmark", quietly = TRUE)) {
      html <- commonmark::markdown_html(txt)
      HTML(sprintf('<div class="narrative">%s</div>', html))
    } else {
      tags$div(class = "narrative", style = "white-space:pre-wrap;", txt)
    }
  })
  
  # ---------- Downloads ----------
  summary_df <- reactive({
    tibble(
      emergency              = input$emergency,
      pin_children_u5        = input$pin_children,
      pin_plw                = input$pin_plw,
      coverage_pct           = input$coverage,
      valuation              = input$valuation,
      planning_years         = input$years,
      study_years            = input$study_years,
      total_cost_usd         = round(results()$cost_total),
      indirect_benefit_usd   = round(results()$indir_total),
      direct_benefit_count   = round(results()$direct_total),
      bcr                    = round(results()$bcr, 3)
    )
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() paste0("coi_summary_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(summary_df(), file)
  )
  
  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("coi_summary_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
      writexl::write_xlsx(list("summary" = summary_df()), path = file)
    }
  )
}

shinyApp(ui, server)
