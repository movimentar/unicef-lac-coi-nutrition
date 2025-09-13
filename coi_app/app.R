# app.R
# Cost of Inaction (CoI) Explorer — UNICEF COs
# movimentar GmbH

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(shinyvalidate)
  library(tidyverse); library(scales); library(gt); library(targets)
  suppressWarnings(try(library(httr2),      silent = TRUE)) # Gemini
  suppressWarnings(try(library(commonmark), silent = TRUE)) # markdown->HTML
  suppressWarnings(try(library(shinycssloaders), silent = TRUE)) # spinners
})

`%||%` <- function(x, y) if (is.null(x)) y else x
as_chr     <- function(x) toString(x)
fmt_dollar <- function(x) as_chr(scales::dollar(x))
fmt_comma  <- function(x) as_chr(scales::comma(x))
fmt_bcr    <- function(x) as_chr(ifelse(is.na(x), "—", sprintf("%.2f", x)))

# If shinycssloaders is available, wrap an output in a spinner
with_spinner_maybe <- function(x) {
  if ("shinycssloaders" %in% loadedNamespaces())
    shinycssloaders::withSpinner(x, type = 4)
  else
    x
}

# ---------- Gemini helpers (REST; header X-Goog-Api-Key) ----------
gemini_generate_rest <- function(prompt,
                                 model = c("gemini-2.5-flash","gemini-2.5-pro","gemini-2.0-flash"),
                                 max_tokens = 400L, temperature = 0.2,
                                 thinking_budget = NULL,
                                 api_key = Sys.getenv("GEMINI_API_KEY"),
                                 verbose = FALSE) {
  model <- match.arg(model)
  if (!nzchar(api_key)) stop("GEMINI_API_KEY not set.")
  endpoint <- sprintf("https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent", model)
  body <- list(
    contents = list(list(parts = list(list(text = prompt)))),
    generationConfig = list(temperature = temperature, maxOutputTokens = as.integer(max_tokens))
  )
  if (!is.null(thinking_budget) && grepl("^gemini-2\\.5", model))
    body$generationConfig$thinkingConfig <- list(thinkingBudget = as.integer(thinking_budget))
  resp <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type"="application/json","X-Goog-Api-Key"=api_key) |>
    httr2::req_body_json(body) |> httr2::req_timeout(30) |> httr2::req_perform()
  httr2::resp_check_status(resp)
  out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (!is.null(out$promptFeedback$blockReason))
    return(sprintf("Response blocked (blockReason: %s).", out$promptFeedback$blockReason))
  texts <- unlist(lapply(out$candidates %||% list(), \(c){
    if (is.null(c$content$parts)) character(0)
    else unlist(lapply(c$content$parts, \(p) p$text %||% NULL), use.names = FALSE)
  }), use.names = FALSE)
  if (!length(texts)) { if (verbose) str(out, max.level = 2); return("Gemini returned no text.") }
  paste(texts, collapse = "\n\n")
}
gemini_generate_rest_retry <- function(..., max_retries = 5L, base_delay = 1.5){
  i <- 0L
  repeat {
    i <- i + 1L
    res <- try(gemini_generate_rest(...), silent = TRUE)
    ok  <- !inherits(res,"try-error") && !grepl("^HTTP\\s*\\d+|failed|no text", res, TRUE)
    if (ok) return(res)
    msg <- if (inherits(res,"try-error")) conditionMessage(res) else res
    if (!grepl("429|Too Many Requests|Resource exhausted|503", msg, TRUE) || i>=max_retries)
      return(paste("Gemini call failed:", msg))
    Sys.sleep(base_delay * (2^(i-1)) * runif(1, .8, 1.2))
  }
}

# ---------- narrative prompt (includes per-indicator lines) ----------
build_coi_prompt <- function(p, direct_lines = character()){
  direct_block <-
    if (length(direct_lines)) {
      paste0("Direct indicators (counts)\n• ", paste(direct_lines, collapse = "\n• "), "\n")
    } else {
      "Direct indicators (counts)\n• None above zero at this setting.\n"
    }
  sprintf(paste(
    "You are a Cost of Inaction Assistant for UNICEF, NGOs and government stakeholders. ",
    "Write a concise narrative (150–220 words) in British English, formal tone, followed by ",
    "3–5 bullet key messages for advocacy. Use ONLY the numbers supplied. Begin with a brief ",
    "disclaimer that these are estimates from a simulation based on the inputs.\n\n",
    "Context\n• Emergency: %s\n• Planning period: %d year(s)\n• Valuation for indirect benefits: %s\n",
    "Inputs\n• PiN children 0–59 months: %s\n• PiN PLW: %s\n• Target coverage: %d%%\n",
    "Results (2015 USD where applicable)\n• Total estimated cost: %s\n• Indirect economic benefits: %s\n",
    "• Direct benefits (headline count): %s\n• Benefit–Cost Ratio: %s\n\n%s",
    "Instructions\n- Be precise and neutral; avoid hype.\n- Translate the numbers into planning implications.\n",
    "- Do not invent data; flag uncertainty briefly.\n- End with 3–5 bullet recommendations.",
    sep = ""),
    p$emergency, as.integer(p$years), p$valuation,
    comma(round(p$pin_children)), comma(round(p$pin_plw)), as.integer(p$coverage),
    dollar(round(p$total_cost)),  dollar(round(p$indir_total)),
    comma(round(p$direct_total)), if (is.na(p$bcr)) "N/A" else sprintf("%.2f", p$bcr),
    direct_block
  )
}

# ---------- data loading (RDS first, then {targets}) ----------
rds_dir <- file.path(getwd(), "data")
load_from_rds_or_targets <- function(name, use_targets_fallback = TRUE, store = "_targets"){
  rds_path <- file.path(rds_dir, paste0(name, ".rds"))
  if (file.exists(rds_path)) return(readRDS(rds_path))
  if (use_targets_fallback){
    if (!requireNamespace("targets", quietly = TRUE)) stop("{targets} not installed.")
    if (!dir.exists(store)) { alt <- file.path("..","_targets"); if (dir.exists(alt)) store <- alt
    else stop(sprintf("Targets store '%s' missing.", store)) }
    targets::tar_config_set(store = store)
    return(targets::tar_read_raw(name))
  }
  stop(sprintf("Data object '%s' not found and no fallback.", name))
}

# ---------- load study objects ----------
intervention_list  <- load_from_rds_or_targets("intervention_list")
coi_costs          <- load_from_rds_or_targets("coi_costs")
coi_coverages      <- load_from_rds_or_targets("coi_coverages")
coi_dir_benefits   <- load_from_rds_or_targets("coi_dir_benefits")
coi_indir_benefits <- load_from_rds_or_targets("coi_indir_benefits")
gni_forecast <- try(load_from_rds_or_targets("gni_forecast"), silent = TRUE); if (inherits(gni_forecast,"try-error")) gni_forecast <- NULL
income_share <- try(load_from_rds_or_targets("income_share"), silent = TRUE); if (inherits(income_share,"try-error")) income_share <- NULL

# ---------- PiN baselines (for scaling) ----------
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
  ) %>% ungroup() %>% select(-total_need)

# ---------- helper: anchors per indicator (clean) ----------
build_direct_anchors <- function(maln_tbl, bf_tbl, emergency, cost_tbl){
  maln <- maln_tbl %>%
    filter(emergency == !!emergency) %>%
    transmute(
      emergency,
      indicator_category = "Malnutrition",
      indicator_name = indicator_name_absolute,
      `30` = as.numeric(saved_30),
      `95` = as.numeric(saved_95)
    )
  bf <- bf_tbl %>%
    filter(emergency == !!emergency) %>%
    transmute(
      emergency,
      indicator_category = "Breastfeeding",
      indicator_name = indicator_name_absolute,
      `30` = as.numeric(abs(saved_30)),
      `95` = as.numeric(abs(saved_95))
    )
  bind_rows(maln, bf) %>%
    mutate(`30` = ifelse(is.na(`30`), 0, `30`),
           `95` = ifelse(is.na(`95`), 0, `95`))
}

# ---------- indirect (undiscounted) tidy for cost-benefit ----------
indir_tbl_undisc <- coi_indir_benefits %>%
  ungroup() %>% mutate(across(contains("Eta-Iota"), abs),
                       across(contains("Migration"), abs)) %>%
  select(indicator_name = indicator_name_absolute,
         matches("^(Eta-Iota|Migration\\sflows)_(implemented|30|95)$")) %>%
  pivot_longer(cols = -indicator_name,
               names_to = c("emergency","anchor"), names_sep = "_",
               values_to = "value")

# ---------- utilities ----------
interp_3pt <- function(y0, y30, y95, x){
  if (is.na(x)) return(NA_real_)
  if (x <= 30)  return(y0 + (y30 - y0) * x/30)
  if (x <= 95)  return(y30 + (y95 - y30) * (x-30)/(95-30))
  slope <- (y95 - y30) / (95 - 30)
  y95 + slope * (x - 95)
}
annualise <- function(total_over_study, study_years = 3){
  ifelse(is.finite(study_years) && study_years > 0, total_over_study / study_years, total_over_study)
}

# ---------- PV engine (optional) ----------
EARNINGS_UPLIFT <- 2.62 * 0.01067
has_pv_inputs <- !(is.null(gni_forecast) || is.null(income_share))
pv_compute_by_em <- function(bf_tbl, gni, ishare, rate = 0.03, base_year = 2022){
  labour_share <- ishare %>% filter(ref_area %in% c("GTM","HND","NIC","COL","PER"),
                                    time=="2020") %>% pull(obs_value) %>% mean(na.rm=TRUE)
  gni_by <- gni %>% filter(year>=2038, year<=2080) %>%
    select(emergency, year, real_gni_pc_const_usd) %>% group_by(emergency) %>%
    summarise(years=list(year), gni_pc=list(real_gni_pc_const_usd), .groups="drop")
  add_ebf <- bf_tbl %>% ungroup() %>%
    filter(indicator_name_absolute=="Exclusively breastfed children") %>%
    mutate(across(c(saved_implemented,saved_30,saved_95), abs)) %>%
    select(emergency, saved_implemented, saved_30, saved_95) %>%
    left_join(gni_by, by="emergency")
  pv_one <- function(n, g, yrs, r){ sum((g*(labour_share/100)*EARNINGS_UPLIFT*n) / (1+r)^(yrs - base_year), na.rm=TRUE) }
  add_ebf %>% rowwise() %>%
    mutate(pv_impl=pv_one(saved_implemented,gni_pc,years,rate),
           pv_30  =pv_one(saved_30,         gni_pc,years,rate),
           pv_95  =pv_one(saved_95,         gni_pc,years,rate)) %>%
    ungroup() %>% select(emergency, pv_impl, pv_30, pv_95)
}
pv_tab_3 <- if (has_pv_inputs) pv_compute_by_em(coi_dir_benefits$breastfeeding, gni_forecast, income_share, 0.03) else NULL
pv_tab_5 <- if (has_pv_inputs) pv_compute_by_em(coi_dir_benefits$breastfeeding, gni_forecast, income_share, 0.05) else NULL

# ---------- UI ----------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "CoI Explorer",
  tags$style(HTML("
    .narrative{white-space:normal}
    .narrative p{margin-bottom:.6rem}
    .narrative ul{margin-left:1.2rem}
  ")),
  layout_columns(
    col_widths = c(4, 8),
    # ---- Inputs ----
    card(
      header = "Inputs",
      selectInput("emergency","Emergency context", choices = sort(unique(coi_costs$emergency))),
      numericInput("pin_children","PiN — Children 0–59 months", 100000, min=0, step=1000),
      numericInput("pin_plw","PiN — PLW", 40000, min=0, step=1000),
      sliderInput("coverage","Target coverage (%)", min=0,max=100,value=50, step=1),
      numericInput("years","Planning period (years)", 3, min=1, step=1),
      radioButtons("valuation","Indirect benefits valuation",
                   c("Undiscounted"="undisc","PV (3%)"="pv3","PV (5%)"="pv5"), selected="undisc"),
      checkboxInput("show_dbg","Show anchors/debug", FALSE),
      accordion(
        accordion_panel("Advanced",
                        numericInput("study_years","Study duration for annualisation (years)", 3, min=1, step=1),
                        checkboxInput("override_costs","Adjust median costs", FALSE),
                        conditionalPanel("input.override_costs",
                                         sliderInput("cost_adj","Cost adjustment factor (×)", .25, 2.5, 1.0, .05),
                                         helpText("Proportional change to total costs (local prices/logistics).")
                        ),
                        checkboxInput("enable_ai","Enable automated narrative (Gemini API key required)", FALSE)
        )
      )
    ),
    
    # ---- Results ----
    card(
      header = "Results",
      navset_card_tab(
        # Dashboard tab
        nav_panel("Dashboard", icon = icon("dashboard"),
                  layout_columns(
                    col_widths = c(6,6),
                    value_box(title = "Total response cost",  value = textOutput("total_cost"),  showcase = icon("sack-dollar")),
                    value_box(title = "Indirect benefits",    value = textOutput("indir_benefit"), showcase = icon("arrow-trend-up"))
                  ),
                  layout_columns(
                    col_widths = c(6,6),
                    value_box(title = "Direct benefits (headline)", value = textOutput("direct_headline"), showcase = icon("users")),
                    value_box(title = "Benefit–Cost Ratio",         value = textOutput("bcr"),             showcase = icon("scale-balanced"))
                  ),
                  hr(),
                  with_spinner_maybe(plotOutput("benefit_plot", height = 320))
        ),
        
        # Detailed tables tab
        nav_panel("Detailed tables", icon = icon("table-list"),
                  h5("Planning summary"),
                  with_spinner_maybe(gt_output("summary_gt")),
                  conditionalPanel("input.show_dbg", hr(), h5("Debug: per-indicator anchors (30% / 95%)"),
                                   with_spinner_maybe(gt_output("dbg_gt"))),
                  hr(),
                  h5("Direct benefits by indicator"),
                  with_spinner_maybe(gt_output("direct_detail_gt")),
                  div(class="mt-2", downloadButton("dl_direct_csv","Download indicators (CSV)"))
        ),
        
        # AI Interpretation tab
        nav_panel("Automated interpretation", icon = icon("robot"),
                  br(),
                  actionButton("generate_narrative", "Generate interpretation",
                               class="btn-primary", icon = icon("wand-magic-sparkles")),
                  hr(),
                  with_spinner_maybe(uiOutput("narrative"))
        )
      ),
      hr(),
      downloadButton("dl_csv","Download CSV"),
      downloadButton("dl_xlsx","Download XLSX")
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session){
  # Validation
  iv <- InputValidator$new()
  iv$add_rule("pin_children", sv_gte(0)); iv$add_rule("pin_plw", sv_gte(0))
  iv$add_rule("years", sv_between(1, 50)); iv$add_rule("study_years", sv_between(1, 50))
  iv$enable()
  
  # PiN scale (fully reactive)
  pin_scale <- reactive({
    req(iv$is_valid())
    base <- pin_baseline %>% filter(emergency == input$emergency)
    if (!nrow(base)) return(list(children=1, plw=1))
    list(
      children = ifelse(base$baseline_children > 0, input$pin_children / base$baseline_children, 1),
      plw      = ifelse(base$baseline_plw      > 0, input$pin_plw      / base$baseline_plw, 1)
    )
  })
  
  # Clean anchors table per emergency
  direct_anchors <- reactive({
    build_direct_anchors(coi_dir_benefits$malnutrition, coi_dir_benefits$breastfeeding, input$emergency, coi_costs)
  })
  
  output$dbg_gt <- render_gt({
    req(input$show_dbg)
    direct_anchors() %>%
      arrange(indicator_category, desc(`95`)) %>%
      mutate(`30` = round(`30`), `95` = round(`95`)) %>%
      gt(groupname_col = "indicator_category", rowname_col = "indicator_name") %>%
      tab_header(title = "Anchors (counts at 30% and 95% coverage)")
  })
  
  # Per-indicator results (reactive)
  direct_by_indicator <- reactive({
    da <- direct_anchors(); req(nrow(da) > 0)
    nm <- tolower(da$indicator_name)
    is_plw    <- grepl("maternal|stillbirth", nm)
    scale_vec <- ifelse(is_plw, pin_scale()$plw, pin_scale()$children)
    cov <- as.numeric(input$coverage)
    y30 <- as.numeric(da$`30`); y95 <- as.numeric(da$`95`); y0 <- 0
    y_cov <- if (cov <= 30) y0 + (y30 - y0) * (cov / 30) else y30 + (y95 - y30) * ((cov - 30) / (95 - 30))
    study_total <- as.numeric(y_cov) * as.numeric(scale_vec)
    yrs_study <- as.numeric(input$study_years); yrs_plan <- as.numeric(input$years)
    yrs_study <- ifelse(is.finite(yrs_study) && yrs_study > 0, yrs_study, 3)
    yrs_plan  <- ifelse(is.finite(yrs_plan)  && yrs_plan  > 0, yrs_plan, 1)
    per_year       <- study_total / yrs_study
    total_planning <- per_year * yrs_plan
    tibble(
      indicator_category = da$indicator_category,
      indicator_name     = da$indicator_name,
      study_total        = study_total,
      per_year           = round(per_year),
      total_planning     = round(total_planning)
    )
  })
  
  # Short lines for AI prompt (top 6 indicators)
  direct_prompt_lines <- reactive({
    tb <- direct_by_indicator()
    if (!nrow(tb)) return(character(0))
    tb %>%
      arrange(desc(total_planning)) %>%
      filter(is.finite(total_planning), total_planning > 0) %>%
      mutate(line = sprintf("%s — %s over planning period (%s/yr)",
                            indicator_name, comma(total_planning), comma(per_year))) %>%
      slice_head(n = 6) %>%
      pull(line)
  })
  
  # Headline = sum of planning-period totals
  direct_total_planning <- reactive({
    sum(direct_by_indicator()$total_planning, na.rm = TRUE)
  })
  
  # Indirects
  indir_interp <- reactive({
    agg_scale <- mean(c(pin_scale()$children, pin_scale()$plw), na.rm = TRUE)
    if (input$valuation == "undisc"){
      anchors <- indir_tbl_undisc %>% filter(emergency == input$emergency) %>%
        pivot_wider(names_from = anchor, values_from = value) %>% mutate(`0` = 0)
      val <- anchors %>% mutate(
        value_cov    = pmap_dbl(list(`0`,`30`,`95`), ~ interp_3pt(..1, ..2, ..3, input$coverage)),
        value_scaled = value_cov * agg_scale
      )
      tibble(total = sum(val$value_scaled, na.rm = TRUE))
    } else {
      if (!has_pv_inputs) return(tibble(total = NA_real_))
      pv_tab <- if (input$valuation=="pv3") pv_tab_3 else pv_tab_5
      row <- pv_tab %>% filter(emergency == input$emergency); req(nrow(row)>0)
      y0 <- 0; y30 <- ifelse(is.finite(row$pv_30), row$pv_30, row$pv_impl); y95 <- row$pv_95
      tibble(total = as.numeric(interp_3pt(y0, y30, y95, input$coverage)) * agg_scale)
    }
  })
  
  # Costs
  cost_interp <- reactive({
    base <- coi_costs %>% filter(emergency == input$emergency); req(nrow(base) > 0)
    ideal_cost_tot <- sum(base$ideal_cost, na.rm = TRUE)
    impl_cost_tot  <- sum(base$total_cost, na.rm = TRUE)
    impl_cov       <- mean(base$global_coverage, na.rm = TRUE)
    slope95 <- ifelse(is.finite(ideal_cost_tot), ideal_cost_tot/95, NA_real_)
    slopeI  <- ifelse(is.finite(impl_cost_tot) && is.finite(impl_cov) && impl_cov>0, impl_cost_tot/impl_cov, NA_real_)
    slope <- mean(c(slope95, slopeI), na.rm = TRUE); if (!is.finite(slope)) slope <- 0
    pin_factor <- mean(c(pin_scale()$children, pin_scale()$plw), na.rm = TRUE)
    tot <- slope * input$coverage * pin_factor
    if (isTRUE(input$override_costs)) tot <- tot * input$cost_adj
    tibble(cost_total_study = tot)
  })
  
  # Aggregate (per planning period)
  results <- reactive({
    study_years <- input$study_years; years <- input$years
    indir_total_study <- indir_interp()$total; cost_total_study <- cost_interp()$cost_total_study[1]
    indir_total <- annualise(indir_total_study, study_years) * years
    cost_total  <- annualise(cost_total_study,  study_years) * years
    bcr <- ifelse(cost_total>0, indir_total/cost_total, NA_real_)
    list(
      direct_total = direct_total_planning(),
      indir_total  = indir_total,
      cost_total   = cost_total,
      bcr          = bcr
    )
  })
  
  # ---- Dashboard outputs ----
  output$total_cost      <- renderText({ req(results()); fmt_dollar(round(results()$cost_total)) })
  output$indir_benefit   <- renderText({ req(results()); fmt_dollar(round(results()$indir_total)) })
  output$direct_headline <- renderText({ req(results()); fmt_comma(round(results()$direct_total)) })
  output$bcr             <- renderText({ req(results()); fmt_bcr(results()$bcr) })
  
  output$benefit_plot <- renderPlot({
    req(results())
    df <- tibble(metric=factor(c("Indirect benefits","Cost"), levels=c("Indirect benefits","Cost")),
                 usd=c(results()$indir_total, results()$cost_total))
    ggplot(df, aes(metric, usd)) + geom_col(width=.6) +
      geom_text(aes(label = dollar(usd)), vjust = -0.3) +
      scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0,.08))) +
      labs(x=NULL, y="USD", title="Costs and indirect benefits over the planning period") +
      theme_minimal(base_size = 12)
  })
  
  # ---- Tables ----
  output$summary_gt <- render_gt({
    req(results())
    tibble(
      Item = c("Emergency","PiN — Children 0–59m","PiN — PLW","Target coverage (%)",
               "Valuation (indirect benefits)","Planning period (years)","Study duration (years)",
               "Total response cost (USD)","Indirect benefits (USD)","Direct benefits — headline (count)","Benefit–Cost Ratio"),
      Value = c(
        input$emergency, comma(input$pin_children), comma(input$pin_plw), paste0(input$coverage,"%"),
        switch(input$valuation, undisc="Undiscounted", pv3="PV (3%)", pv5="PV (5%)"),
        input$years, input$study_years,
        dollar(round(results()$cost_total)), dollar(round(results()$indir_total)),
        comma(round(results()$direct_total)), ifelse(is.na(results()$bcr),"—",round(results()$bcr,2))
      )
    ) %>% gt() %>% tab_header(title="CoI planning summary") %>%
      cols_align("left", columns = Item) %>% cols_align("center", columns = Value)
  })
  
  output$direct_detail_gt <- render_gt({
    tb <- direct_by_indicator(); req(nrow(tb) > 0)
    tb %>%
      gt(groupname_col = "indicator_category", rowname_col = "indicator_name") %>%
      tab_header(title = "Direct benefits by indicator") %>%
      cols_hide(columns = c(study_total)) %>%
      cols_label(
        per_year       = "Per year (count)",
        total_planning = "Planning period (count)"
      ) %>%
      fmt_number(columns = c(per_year, total_planning), decimals = 0, use_seps = TRUE) %>%
      cols_align("center", columns = c(per_year, total_planning))
  })
  
  output$dl_direct_csv <- downloadHandler(
    filename = function() paste0("coi_direct_indicators_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(direct_by_indicator(), file)
  )
  
  # ---- AI narrative (decoupled; runs only on click) ----
  output$narrative <- renderUI({
    tags$p(class="text-muted narrative",
           "Click “Generate interpretation” to request a short AI narrative for the current settings.")
  })
  
  observeEvent(input$generate_narrative, {
    if (!isTRUE(input$enable_ai)) {
      output$narrative <- renderUI(tags$p(class="text-muted narrative",
                                          "Enable the toggle in Advanced to allow AI interpretation."))
      return()
    }
    if (!requireNamespace("httr2", quietly = TRUE) || Sys.getenv("GEMINI_API_KEY")=="") {
      output$narrative <- renderUI(tags$p(class="text-danger narrative",
                                          "No API key found or {httr2} not installed. Set GEMINI_API_KEY and restart."))
      return()
    }
    
    showNotification("Requesting narrative from Gemini…", type = "message", duration = 5)
    
    # Freeze current inputs/results so this doesn't keep re-running
    res <- isolate(results())
    p <- list(
      emergency=isolate(input$emergency), years=isolate(input$years),
      valuation=switch(isolate(input$valuation), undisc="Undiscounted", pv3="PV (3%)", pv5="PV (5%)"),
      pin_children=isolate(input$pin_children), pin_plw=isolate(input$pin_plw),
      coverage=isolate(input$coverage),
      total_cost=res$cost_total, indir_total=res$indir_total,
      direct_total=res$direct_total, bcr=res$bcr
    )
    
    txt <- gemini_generate_rest_retry(
      prompt = build_coi_prompt(p, direct_lines = isolate(direct_prompt_lines())),
      model  = "gemini-2.5-flash", temperature = 0.2, max_tokens = 700, thinking_budget = 0
    )
    
    if (requireNamespace("commonmark", quietly = TRUE)) {
      output$narrative <- renderUI(HTML(sprintf('<div class="narrative">%s</div>', commonmark::markdown_html(txt))))
    } else {
      output$narrative <- renderUI(tags$div(class="narrative", style="white-space:pre-wrap;", txt))
    }
  })
  
  # ---- Downloads (summary) ----
  summary_df <- reactive({
    tibble(
      emergency = input$emergency,
      pin_children_u5 = input$pin_children,
      pin_plw = input$pin_plw,
      coverage_pct = input$coverage,
      valuation = input$valuation,
      planning_years = input$years,
      study_years = input$study_years,
      total_cost_usd = round(results()$cost_total),
      indirect_benefit_usd = round(results()$indir_total),
      direct_benefit_count = round(results()$direct_total),
      bcr = round(results()$bcr, 3)
    )
  })
  output$dl_csv  <- downloadHandler(filename=function() paste0("coi_summary_", Sys.Date(), ".csv"),
                                    content=function(file) readr::write_csv(summary_df(), file))
  output$dl_xlsx <- downloadHandler(filename=function() paste0("coi_summary_", Sys.Date(), ".xlsx"),
                                    content=function(file){
                                      if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
                                      writexl::write_xlsx(list("summary" = summary_df()), path = file)
                                    })
}

shinyApp(ui, server)
