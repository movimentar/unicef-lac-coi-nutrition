# app.R
# Cost of Inaction (CoI) Explorer — UNICEF COs
# movimentar GmbH

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(shinyvalidate); library(shinyjs)
  library(tidyverse); library(scales); library(gt); library(targets)
  suppressWarnings(try(library(httr2),      silent = TRUE))   # Gemini API
  suppressWarnings(try(library(commonmark), silent = TRUE))   # Markdown -> HTML
  suppressWarnings(try(library(shinycssloaders), silent = TRUE)) # spinners
  suppressWarnings(try(library(rmarkdown),  silent = TRUE))   # HTML report
  suppressWarnings(try(library(writexl),    silent = TRUE))   # XLSX
})

# ---------- Small helpers ----------
`%||%` <- function(x, y) if (is.null(x)) y else x
as_chr     <- function(x) toString(x)
fmt_dollar <- function(x) as_chr(scales::dollar(round(x, 2))) # show cents
fmt_comma  <- function(x) as_chr(scales::comma(round(x, 0)))
fmt_bcr    <- function(x) as_chr(ifelse(is.na(x), "—", sprintf("%.2f", x)))
with_spinner_maybe <- function(x) {
  if (requireNamespace("shinycssloaders", quietly = TRUE))
    shinycssloaders::withSpinner(x, type = 6, color = "#1CABE2")
  else x
}

# Natural sort for IDs like "5-b", "10", "12-a" etc.
mixed_order <- function(ids_chr){
  num <- suppressWarnings(as.integer(stringr::str_extract(ids_chr, "^[0-9]+")))
  suf <- stringr::str_to_lower(stringr::str_replace_na(stringr::str_extract(ids_chr, "(?<=-)[A-Za-z]+"), ""))
  # Empty suffix first, then a, b, c...
  order(num, nchar(suf) > 0, suf)
}

# ---------- Gemini (optional) ----------
gemini_generate_rest <- function(prompt,
                                 model = c("gemini-2.5-flash","gemini-2.5-pro","gemini-2.0-flash"),
                                 max_tokens = 700L, temperature = 0.2,
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
    httr2::req_body_json(body) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()
  
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

safe_condition_message <- function(x){
  if (inherits(x, "try-error")) {
    cond <- attr(x, "condition")
    if (inherits(cond, "condition")) conditionMessage(cond) else as.character(x)
  } else if (inherits(x, "condition")) {
    conditionMessage(x)
  } else if (is.character(x)) {
    x
  } else {
    "Unknown error."
  }
}

gemini_generate_rest_retry <- function(..., max_retries = 5L, base_delay = 1.5){
  i <- 0L
  repeat {
    i <- i + 1L
    res <- try(gemini_generate_rest(...), silent = TRUE)
    ok <- !inherits(res, "try-error") &&
      !grepl("^(HTTP\\s*\\d+|Gemini call failed:|Response blocked|Gemini returned no text)", res, ignore.case = TRUE)
    if (ok) return(res)
    msg <- safe_condition_message(res)
    if (!grepl("429|Too Many Requests|Resource exhausted|503|temporar", msg, ignore.case = TRUE) || i >= max_retries) {
      return(paste("Gemini call failed:", msg))
    }
    Sys.sleep(base_delay * (2^(i-1)) * runif(1, .8, 1.2))
  }
}

build_coi_prompt <- function(p, indicator_lines = NULL){
  sprintf(paste(
    "You are a Cost of Inaction Assistant. Write a concise narrative (150–220 words) in British English,",
    "then 3–5 bullet key messages. Use ONLY the numbers supplied. Begin with a brief disclaimer.\n\n",
    "Context\n• Emergency: %s\n• Planning period: %d year(s)\n• Valuation: %s\n",
    "Inputs\n• PiN children 0–59m: %s\n• PiN PLW: %s\n• Coverage: %d%%\n",
    "Results (2015 USD)\n• Total cost: %s\n• Indirect economic benefits: %s\n",
    "• Direct benefits (headline): %s\n• Benefit–Cost Ratio: %s\n",
    if (length(indicator_lines)) "\nKey direct indicators:\n" else "",
    paste(indicator_lines, collapse = "\n"),
    "\n\nInstructions\n- Be precise, neutral; avoid hype.\n- Translate numbers to planning implications.\n",
    "- Do not invent data; flag uncertainty briefly.\n- Finish with 3–5 bullet recommendations.",
    sep = ""),
    p$emergency, as.integer(p$years), p$valuation,
    comma(round(p$pin_children)), comma(round(p$pin_plw)), as.integer(p$coverage),
    dollar(round(p$total_cost)),  dollar(round(p$indir_total)),
    comma(round(p$direct_total)), if (is.na(p$bcr)) "N/A" else sprintf("%.2f", p$bcr)
  )
}

# ---------- Load data (RDS first; fallback to {targets}) ----------
rds_dir <- file.path(getwd(), "data")
load_from_rds_or_targets <- function(name, use_targets_fallback = TRUE, store = "_targets"){
  rds_path <- file.path(rds_dir, paste0(name, ".rds"))
  if (file.exists(rds_path)) return(readRDS(rds_path))
  if (use_targets_fallback){
    if (!requireNamespace("targets", quietly = TRUE)) stop("{targets} not installed.")
    if (!dir.exists(store)) {
      alt <- file.path("..","_targets")
      if (dir.exists(alt)) store <- alt
      else stop(sprintf("Targets store '%s' missing.", store))
    }
    targets::tar_config_set(store = store)
    return(targets::tar_read_raw(name))
  }
  stop(sprintf("Data object '%s' not found and no fallback.", name))
}

# ---------- Study objects ----------
intervention_list      <- load_from_rds_or_targets("intervention_list")
coi_costs              <- load_from_rds_or_targets("coi_costs")
coverage_costs         <- load_from_rds_or_targets("coverage_costs")     # for ideal_delivered baseline
median_costs_cleaned   <- load_from_rds_or_targets("median_costs_cleaned")
coi_dir_benefits       <- load_from_rds_or_targets("coi_dir_benefits")
coi_indir_benefits     <- load_from_rds_or_targets("coi_indir_benefits")
gni_forecast <- try(load_from_rds_or_targets("gni_forecast"), silent = TRUE); if (inherits(gni_forecast,"try-error")) gni_forecast <- NULL
income_share <- try(load_from_rds_or_targets("income_share"), silent = TRUE); if (inherits(income_share,"try-error")) income_share <- NULL

# ---------- PiN baselines (for scaling) ----------
children_aliases <- c(
  "Children 0–59 months", "Children 0-59 months",
  "Children under 5", "Children under five", "Children u5", "Under five children", "U5 children"
)
plw_aliases <- c("PLW","Pregnant and Lactating Women","Pregnant & lactating women","Pregnant and lactating women")

pin_baseline <- coi_costs %>%
  mutate(tg_norm = tolower(gsub("\\s+", " ", chartr("–—", "--", trimws(target_group))))) %>%
  group_by(emergency) %>%
  summarise(
    base_total = sum(ideal_delivered, na.rm = TRUE),
    baseline_children_raw = sum(ideal_delivered[tg_norm %in% tolower(gsub("\\s+", " ", children_aliases))], na.rm = TRUE),
    baseline_plw_raw      = sum(ideal_delivered[tg_norm %in% tolower(gsub("\\s+", " ", plw_aliases))], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    baseline_children = dplyr::coalesce(na_if(baseline_children_raw, 0), ifelse(base_total > 0, base_total * 0.6, 1)),
    baseline_plw      = dplyr::coalesce(na_if(baseline_plw_raw, 0),      ifelse(base_total > 0, base_total * 0.4, 1))
  ) %>% select(emergency, baseline_children, baseline_plw)

# ---------- Direct benefits anchors (per emergency) ----------
build_direct_anchors <- function(maln_tbl, bf_tbl, em){
  maln <- maln_tbl %>%
    filter(emergency == !!em) %>%
    transmute(emergency, indicator_category = "Malnutrition",
              indicator_name = indicator_name_absolute,
              `30` = as.numeric(saved_30), `95` = as.numeric(saved_95))
  bf <- bf_tbl %>%
    filter(emergency == !!em) %>%
    transmute(emergency, indicator_category = "Breastfeeding",
              indicator_name = indicator_name_absolute,
              `30` = as.numeric(abs(saved_30)), `95` = as.numeric(abs(saved_95)))
  bind_rows(maln, bf) %>% mutate(`30` = replace_na(`30`, 0), `95` = replace_na(`95`, 0))
}

# ---------- Indirect benefits (undiscounted tidy) ----------
indir_tbl_undisc <- coi_indir_benefits %>%
  ungroup() %>%
  mutate(across(contains("Eta-Iota"), abs),
         across(contains("Migration"), abs)) %>%
  select(indicator_name = indicator_name_absolute,
         matches("^(Eta-Iota|Migration\\sflows)_(implemented|30|95)$")) %>%
  pivot_longer(cols = -indicator_name,
               names_to = c("emergency","anchor"), names_sep = "_",
               values_to = "value")

# ---------- PV (optional) ----------
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
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1CABE2",
                   "font-family-sans-serif" = "'Lato', sans-serif"),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Lato:wght@400;700;800&display=swap"),
    tags$script(src = "script.js"),
    # Keep only sidebar scrollable (prevents stretching of main cards)
    tags$style(HTML("
      /* Make only the left sidebar scroll; keep main content steady */
      .bslib-sidebar-layout > .sidebar { 
        position: sticky; 
        top: 0;
        height: calc(100vh - 0px); 
        overflow-y: auto; 
        padding-right: .25rem;
      }
      /* Nicer checkbox list for long names */
      .nie-list .form-check { margin-bottom: .35rem; }
      .nie-list label { white-space: normal; }
    "))
  ),
  div(class = "logo-container",
      tags$img(src = "united-nations-childrens-fund-unicef-vector-logo.png", height = "40"), 
      tags$div(class="title", "Cost of Inaction Explorer - Nutrition in Emergencies")
  ),
  layout_sidebar(
    sidebar = sidebar(
      id = "app_sidebar",
      title = "Scenario Inputs",
      tooltip(
        selectInput("emergency","Emergency context", choices = sort(unique(coi_costs$emergency))),
        "Select the emergency archetype to set baseline conditions."
      ),
      tooltip(
        numericInput("pin_children","PiN — Children 0–59 months", 100000, min=0, step=1000),
        "People in Need (PiN): estimated number of children requiring nutrition assistance."
      ),
      tooltip(
        numericInput("pin_plw","PiN — PLW", 40000, min=0, step=1000),
        "People in Need (PiN): estimated number of pregnant & lactating women requiring assistance."
      ),
      tooltip(
        sliderInput("coverage","Target coverage (%)", min=0,max=100,value=50, step=1),
        "Percentage of the PiN the planned intervention aims to reach."
      ),
      tooltip(
        numericInput("years","Planning period (years)", 3, min=1, step=1),
        "Number of years the response is planned to last. Results scale to this duration."
      ),
      tooltip(
        radioButtons("valuation","Indirect benefits valuation",
                     c("Undiscounted"="undisc","PV (3%)"="pv3","PV (5%)"="pv5"), selected="undisc"),
        "How long-term economic benefits are valued."
      ),
      
      accordion(
        open = FALSE,
        accordion_panel("Advanced settings: NiE package & unit costs", icon = icon("sliders"),
                        
                        # Package selection
                        div(class="nie-list",
                            tooltip(
                              checkboxGroupInput("intervention_package", "NiE package (included items)", choices = NULL),
                              "Select interventions to include in the scenario. Uncheck to exclude from costs/benefits."
                            )
                        ),
                        hr(),
                        h5("Unit-cost override (per intervention)"),
                        helpText(
                          "Edit the unit cost used for calculations. If empty, the default ",
                          tags$em("median per-person cost"),
                          " from the study is used."
                        ),
                        # Pick an intervention (from the currently selected package) and override its unit cost
                        selectInput("uc_sel", "Intervention", choices = NULL),
                        numericInput("uc_val", "Unit cost (USD / beneficiary)", value = NA, min = 0, step = 0.01),
                        fluidRow(
                          column(6, actionButton("uc_apply", "Apply", class="btn-primary")),
                          column(6, actionButton("uc_reset_all", "Reset all", class="btn-secondary"))
                        ),
                        div(id="uc_default_note", class="text-muted small", style="margin-top:.5rem;")
        )
      ),
      div(class = "footer",
          tags$a(href = "https://www.movimentar.eu/", target = "_blank",
                 tags$img(src = "https://movimentar.eu/wp-content/uploads/2019/08/movimentar_logo_transparent.png",
                          class = "footer-logo", alt = "movimentar logo")),
          tags$a(href = "https://github.com/movimentar/unicef-lac-coi-nutrition", target = "_blank",
                 shiny::icon("github"), "Study Repository")
      )
    ),
    
    navset_card_tab(
      title = "Results",
      nav_panel("Dashboard", icon = icon("dashboard"),
                layout_columns(
                  col_widths = c(6,6),
                  tooltip(
                    value_box(
                      title = "Total Response Cost",
                      value = textOutput("total_cost"),
                      showcase = icon("sack-dollar"),
                      color = "secondary"
                    ),
                    "All programme costs over the selected planning period, after applying coverage, PiN scaling and any unit-cost overrides."
                  ),
                  tooltip(
                    value_box(
                      title = "Indirect Benefits",
                      value = textOutput("indir_benefit"),
                      showcase = icon("arrow-trend-up"),
                      color = "primary"
                    ),
                    "Monetised long-term economic benefits (undiscounted or PV at the chosen rate) attributable to the interventions."
                  )
                ),
                layout_columns(
                  col_widths = c(6,6),
                  tooltip(
                    value_box(
                      title = "Direct Benefits (Cases Averted / Improved)",
                      value = textOutput("direct_headline"),
                      showcase = icon("users")
                    ),
                    "Total count of direct outcomes (e.g., cases averted or improved) over the planning period, aggregated across indicators."
                  ),
                  tooltip(
                    value_box(
                      title = "Benefit–Cost Ratio",
                      value = textOutput("bcr"),
                      showcase = icon("scale-balanced")
                    ),
                    "Indirect Benefits divided by Total Response Cost. Values > 1 mean benefits exceed costs."
                  )
                ),
                hr(),
                tooltip(
                  card(full_screen = TRUE,
                       card_header("Cost vs. Benefit Analysis"),
                       with_spinner_maybe(plotOutput("benefit_plot", height = "320px"))
                  ),
                  "Comparison of total indirect benefits versus total programme cost over the planning period (USD)."
                )
      ),
      
      nav_panel("Detailed Tables", icon = icon("table-list"),
                h5("Planning Summary"),
                gt_output("summary_gt"),
                hr(),
                h5("Direct benefits by indicator"),
                gt_output("direct_detail_gt")
      ),
      
      nav_panel("AI Interpretation", icon = icon("robot"),
                actionButton("generate_narrative", "Generate Interpretation",
                             class="btn-primary", icon = icon("wand-magic-sparkles")),
                helpText("Click to generate a narrative summary for the current scenario (via Gemini)."),
                hr(),
                with_spinner_maybe(uiOutput("narrative"))
      ),
      
      nav_panel("Downloads & Report", icon = icon("download"),
                br(), p("Download the raw summary data for the current scenario."),
                downloadButton("dl_xlsx","Summary (XLSX)"),
                downloadButton("dl_direct_xlsx","Indicators (XLSX)"),
                hr(),
                p("Generate a full report for the current inputs. The report opens in a new tab; you can print or save as PDF from your browser."),
                actionButton("generate_report", "Generate HTML Report", icon = icon("file-invoice"))
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session){
  # --- Validation
  iv <- InputValidator$new()
  iv$add_rule("pin_children", sv_gte(0)); iv$add_rule("pin_plw", sv_gte(0))
  iv$add_rule("years", sv_between(1, 50))
  iv$enable()
  
  # --- PiN scaling
  pin_scale <- reactive({
    base <- pin_baseline %>% filter(emergency == input$emergency)
    if (!nrow(base)) return(list(children=1, plw=1))
    list(
      children = ifelse(base$baseline_children > 0, input$pin_children / base$baseline_children, 1),
      plw      = ifelse(base$baseline_plw      > 0, input$pin_plw      / base$baseline_plw, 1)
    )
  })
  
  # --- Direct anchors for selected emergency
  direct_anchors <- reactive({
    build_direct_anchors(coi_dir_benefits$malnutrition, coi_dir_benefits$breastfeeding, input$emergency)
  })
  
  # --- Available interventions (names + IDs; single row per intervention)
  interventions_tbl <- reactive({
    # Take a representative 'ideal_delivered' per intervention (ignore country)
    base <- coverage_costs %>%
      filter(emergency == input$emergency) %>%
      mutate(intervention_id = as.character(intervention_id)) %>%
      group_by(intervention_id) %>%
      summarise(
        intervention_name = dplyr::first(na.omit(intervention_name)),
        ideal_delivered   = dplyr::first(na.omit(ideal_delivered)),
        .groups = "drop"
      )
    # join the study medians for this emergency
    med <- median_costs_cleaned %>%
      filter(emergency == input$emergency) %>%
      mutate(intervention_id = as.character(intervention_id)) %>%
      select(intervention_id, median_cost_person)
    out <- base %>% left_join(med, by = "intervention_id")
    # natural order
    out[mixed_order(out$intervention_id), , drop = FALSE]
  })
  
  # Populate the package checkbox (labels = names only; values = IDs)
  observeEvent(interventions_tbl(), {
    it <- interventions_tbl()
    choices <- setNames(it$intervention_id, it$intervention_name)  # labels: names only
    updateCheckboxGroupInput(session, "intervention_package",
                             choices = choices,
                             selected = it$intervention_id   # all checked by default
    )
    # Also populate the override dropdown with the same order
    updateSelectInput(session, "uc_sel",
                      choices = setNames(it$intervention_id, it$intervention_name),
                      selected = it$intervention_id[1]
    )
    # Seed default-note
    def <- it %>% slice(1)
    txt <- sprintf("Default (study) unit cost for this intervention: %s", fmt_dollar(def$median_cost_person))
    shinyjs::html(id = "uc_default_note", html = txt)
    # Prefill numeric with default (rounded 2 d.p.)
    updateNumericInput(session, "uc_val", value = round(def$median_cost_person, 2))
  }, ignoreInit = FALSE)
  
  # When override dropdown changes, refresh the default note & numeric value
  observeEvent(input$uc_sel, {
    it <- interventions_tbl()
    row <- it %>% filter(intervention_id == input$uc_sel) %>% slice(1)
    if (nrow(row)) {
      shinyjs::html("uc_default_note", sprintf("Default (study) unit cost for this intervention: %s", fmt_dollar(row$median_cost_person)))
      # If there's an existing user override, show it; else default
      val <- overrides()[[row$intervention_id]] %||% row$median_cost_person
      updateNumericInput(session, "uc_val", value = round(val, 2))
    }
  })
  
  # Store overrides as a named list
  .overrides <- reactiveVal(list())
  overrides <- reactive(.overrides())
  
  observeEvent(input$uc_apply, {
    req(input$uc_sel, is.finite(input$uc_val))
    cur <- overrides()
    cur[[input$uc_sel]] <- as.numeric(input$uc_val)
    .overrides(cur)
    showNotification("Override saved for selected intervention.", type = "message", duration = 2)
  })
  observeEvent(input$uc_reset_all, {
    .overrides(list())
    # reset numeric to default of current selection
    it <- interventions_tbl()
    row <- it %>% filter(intervention_id == input$uc_sel) %>% slice(1)
    if (nrow(row)) updateNumericInput(session, "uc_val", value = round(row$median_cost_person, 2))
    showNotification("All overrides cleared.", type = "message", duration = 2)
  })
  
  # --- Per-indicator results (interpolated, PiN-scaled, annualised, planning period)
  direct_by_indicator <- reactive({
    da <- direct_anchors()
    req(nrow(da) > 0)
    nm <- tolower(da$indicator_name)
    is_plw <- grepl("maternal|stillbirth", nm)   # PLW-oriented indicators
    scale_vec <- ifelse(is_plw, pin_scale()$plw, pin_scale()$children)
    cov <- as.numeric(input$coverage)
    y30 <- as.numeric(da$`30`); y95 <- as.numeric(da$`95`); y0 <- 0
    y_cov <- if (cov <= 30) { y0 + (y30 - y0) * (cov / 30) } else { y30 + (y95 - y30) * ((cov - 30) / (95 - 30)) }
    study_total <- as.numeric(y_cov) * as.numeric(scale_vec)
    yrs_study <- 3; yrs_plan <- ifelse(is.finite(input$years) && input$years>0, input$years, 1)
    per_year       <- study_total / yrs_study
    total_planning <- per_year * yrs_plan
    tibble(
      indicator_category = da$indicator_category,
      indicator_name     = da$indicator_name,
      study_total        = study_total,
      per_year           = round(per_year),
      total_planning     = round(total_planning)
    ) %>% arrange(indicator_category, desc(total_planning))
  })
  
  direct_total_planning <- reactive(sum(direct_by_indicator()$total_planning, na.rm = TRUE))
  
  # --- Indirect benefits
  interp_3pt <- function(y0, y30, y95, x){
    if (is.na(x)) return(NA_real_)
    if (x <= 30)  return(y0 + (y30 - y0) * x/30)
    if (x <= 95)  return(y30 + (y95 - y30) * (x-30)/(95-30))
    slope <- (y95 - y30) / (95 - 30); y95 + slope * (x - 95)
  }
  annualise <- function(total_over_study, study_years = 3){
    ifelse(is.finite(study_years) && study_years > 0, total_over_study / study_years, total_over_study)
  }
  
  indir_interp <- reactive({
    agg_scale <- mean(c(pin_scale()$children, pin_scale()$plw), na.rm = TRUE)
    if (input$valuation == "undisc"){
      anchors <- indir_tbl_undisc %>% filter(emergency == input$emergency) %>%
        pivot_wider(names_from = anchor, values_from = value) %>% mutate(`0` = 0)
      val <- anchors %>% mutate(
        value_cov   = pmap_dbl(list(`0`,`30`,`95`), ~ interp_3pt(..1, ..2, ..3, input$coverage)),
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
  
  # --- Cost engine using unit-costs (median_costs_cleaned) + overrides
  # Build the effective unit price table for selected package
  unit_prices_tbl <- reactive({
    it <- interventions_tbl()
    sel <- input$intervention_package %||% it$intervention_id
    it <- it %>% filter(intervention_id %in% sel)
    ov <- overrides()
    it %>%
      mutate(
        override = map_dbl(intervention_id, ~ as.numeric(ov[[.x]] %||% NA_real_)),
        final_unit_cost = ifelse(is.finite(override), override, median_cost_person)
      )
  })
  
  # Costs proportional to coverage, scaled by PiN; using ideal_delivered baseline per intervention
  cost_interp <- reactive({
    tbl <- unit_prices_tbl()
    req(nrow(tbl) > 0)
    # Total ideal cost for 100% (sum of unit * ideal_delivered over selected items)
    ideal_total_100 <- sum(tbl$final_unit_cost * tbl$ideal_delivered, na.rm = TRUE)
    cost_at_95 <- ideal_total_100 * 0.95
    cost_per_pct <- if (cost_at_95 > 0) cost_at_95 / 95 else 0
    coverage_cost <- cost_per_pct * as.numeric(input$coverage)
    pin_factor <- mean(c(pin_scale()$children, pin_scale()$plw), na.rm = TRUE)
    tibble(cost_total_study = coverage_cost * pin_factor)
  })
  
  # --- Aggregate results (per planning period)
  results <- reactive({
    indir_total_study <- indir_interp()$total
    cost_total_study  <- cost_interp()$cost_total_study[1]
    indir_total <- annualise(indir_total_study, 3) * input$years
    cost_total  <- annualise(cost_total_study,  3) * input$years
    bcr <- ifelse(cost_total>0, indir_total/cost_total, NA_real_)
    list(
      direct_total = direct_total_planning(),
      indir_total  = indir_total,
      cost_total   = cost_total,
      bcr          = bcr
    )
  })
  
  # --- Dashboard outputs
  output$total_cost      <- renderText({ req(results()); fmt_dollar(results()$cost_total) })
  output$indir_benefit   <- renderText({ req(results()); fmt_dollar(results()$indir_total) })
  output$direct_headline <- renderText({ req(results()); fmt_comma(results()$direct_total) })
  output$bcr             <- renderText({ req(results()); fmt_bcr(results()$bcr) })
  
  UNICEF_CYAN <- "#1CABE2"
  output$benefit_plot <- renderPlot({
    req(results())
    df <- tibble(
      metric = factor(c("Indirect benefits","Cost"), levels = c("Indirect benefits","Cost")),
      usd = c(results()$indir_total, results()$cost_total)
    )
    ggplot(df, aes(metric, usd, fill = metric)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = dollar(usd)), vjust = -0.5, size = 3.2) +
      scale_fill_manual(values = c("Indirect benefits" = UNICEF_CYAN, "Cost" = "#6C757D")) +
      scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "USD", title = "Costs and indirect benefits over the planning period") +
      theme_minimal(base_size = 16) +
      theme(plot.title=element_text(size=13, face="bold"), axis.title.y=element_text(size=8), axis.text=element_text(size=8))
  }, res = 144)
  
  # --- Detailed tables
  summary_gt_df <- reactive({
    req(results())
    tibble(
      Item = c("Emergency","PiN — Children 0–59m","PiN — PLW","Target coverage (%)",
               "Valuation (indirect benefits)","Planning period (years)",
               "Total response cost (USD)","Indirect benefits (USD)","Direct benefits — headline (count)","Benefit–Cost Ratio"),
      Value = c(
        input$emergency, fmt_comma(input$pin_children), fmt_comma(input$pin_plw), paste0(input$coverage,"%"),
        switch(input$valuation, undisc="Undiscounted", pv3="PV (3%)", pv5="PV (5%)"),
        input$years, 
        fmt_dollar(results()$cost_total), fmt_dollar(results()$indir_total),
        fmt_comma(results()$direct_total), fmt_bcr(results()$bcr)
      )
    )
  })
  output$summary_gt <- render_gt({
    summary_gt_df() %>%
      gt() %>% tab_header(title="CoI Planning Summary") %>%
      cols_align("left", columns = Item) %>% cols_align("center", columns = Value)
  })
  output$direct_detail_gt <- render_gt({
    tb <- direct_by_indicator(); req(nrow(tb) > 0)
    tb %>%
      gt(groupname_col = "indicator_category", rowname_col = "indicator_name") %>%
      tab_header(title = "Direct benefits by indicator") %>%
      cols_hide(columns = c(study_total)) %>%
      cols_label(per_year = "Per year (count)", total_planning = "Planning period (count)") %>%
      fmt_number(columns = c(per_year, total_planning), decimals = 0, use_seps = TRUE) %>%
      cols_align("center", columns = c(per_year, total_planning))
  })
  
  # --- AI interpretation (button)
  narrative_html <- reactiveVal(NULL)
  
  # Reset the AI interpretation whenever any user-controlled parameter changes
  observeEvent(
    list(
      input$emergency,
      input$pin_children,
      input$pin_plw,
      input$coverage,
      input$years,
      input$valuation,
      input$intervention_package,  # NiE package items
      input$uc_val,                # number typed in the override box
      input$uc_apply,              # override applied
      input$uc_reset_all           # overrides cleared
    ),
    {
      narrative_html(NULL)  # go back to the default "Click to generate…" message
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$generate_narrative, {
    if (!requireNamespace("httr2", quietly = TRUE) || Sys.getenv("GEMINI_API_KEY")=="") {
      narrative_html(tags$p(class="text-danger narrative",
                            "No API key found or {httr2} not installed. Set GEMINI_API_KEY and try again."))
      return(invisible())
    }
    id <- showNotification("Requesting narrative from Gemini API…", type = "message", duration = NULL)
    on.exit(removeNotification(id), add = TRUE)
    res   <- isolate(results())
    dir_df<- isolate(direct_by_indicator())
    inputs<- isolate(reactiveValuesToList(input))
    top3 <- dir_df %>% arrange(desc(total_planning)) %>% slice_head(n = 3) %>%
      transmute(line = sprintf("• %s: %s", indicator_name, scales::comma(total_planning))) %>% pull(line)
    p <- list(
      emergency = inputs$emergency, years = inputs$years,
      valuation = switch(inputs$valuation, undisc="Undiscounted", pv3="PV (3%)", pv5="PV (5%)"),
      pin_children = inputs$pin_children, pin_plw = inputs$pin_plw, coverage = inputs$coverage,
      total_cost = res$cost_total, indir_total = res$indir_total,
      direct_total = res$direct_total, bcr = res$bcr
    )
    txt <- gemini_generate_rest_retry(
      prompt = build_coi_prompt(p, indicator_lines = top3),
      model = "gemini-2.5-flash", temperature = 0.2, max_tokens = 700, thinking_budget = 0
    )
    if (grepl("^(Gemini call failed:|Response blocked|Gemini returned no text)", txt, ignore.case = TRUE)) {
      showNotification(txt, type = "error", duration = 6)
      narrative_html(tags$p(class="text-danger narrative", txt))
      return(invisible())
    }
    if (requireNamespace("commonmark", quietly = TRUE)) {
      narrative_html(HTML(sprintf('<div class="narrative">%s</div>', commonmark::markdown_html(txt))))
    } else {
      narrative_html(tags$div(class="narrative", style="white-space:pre-wrap;", txt))
    }
  })
  output$narrative <- renderUI({
    narrative_html() %||% tags$p(class="text-muted narrative",
                                 "Click “Generate Interpretation” to obtain a short narrative.")
  })
  
  # --- Downloads
  output$dl_direct_xlsx <- downloadHandler(
    filename = function() paste0("coi_direct_indicators_", Sys.Date(), ".xlsx"),
    content  = function(file) writexl::write_xlsx(direct_by_indicator(), file)
  )
  summary_df_export <- reactive({
    tibble(
      emergency              = input$emergency,
      pin_children_u5        = input$pin_children,
      pin_plw                = input$pin_plw,
      coverage_pct           = input$coverage,
      valuation              = input$valuation,
      planning_years         = input$years,
      total_cost_usd         = round(results()$cost_total, 2),
      indirect_benefit_usd   = round(results()$indir_total, 2),
      direct_benefit_count   = round(results()$direct_total),
      bcr                    = round(results()$bcr, 3)
    )
  })
  output$dl_xlsx <- downloadHandler(
    filename=function() paste0("coi_summary_", Sys.Date(), ".xlsx"),
    content=function(file) writexl::write_xlsx(list("summary" = summary_df_export()), path = file)
  )
  
  # --- HTML report (opens in new tab)
  observeEvent(input$generate_report, {
    req(results())
    narrative_str <- tryCatch(as.character(isolate(narrative_html())), error = function(e) "")
    withProgress(message = "Rendering HTML report…", value = 0, {
      incProgress(0.2, detail = "Preparing data")
      prefix <- paste0("reports_", session$token)
      reports_dir <- file.path(tempdir(), prefix)
      dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
      addResourcePath(prefix, reports_dir)
      report_filename <- paste0("CoI_Report_", gsub("\\s+", "_", input$emergency),
                                "_", round(as.numeric(Sys.time())), ".html")
      output_file <- file.path(reports_dir, report_filename)
      params <- list(
        summary_df     = summary_df_export(),
        results        = results(),
        direct_df      = direct_by_indicator(),
        narrative_html = narrative_str
      )
      incProgress(0.5, detail = "Knitting report")
      rmarkdown::render(
        input = "report.Rmd",          # <- keep your improved report_html.Rmd name here if different
        output_file = output_file,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      incProgress(0.3, detail = "Opening in a new tab")
      url <- paste0("./", prefix, "/", report_filename)
      shinyjs::runjs(sprintf("openReport('%s')", url))
    })
  })
}

shinyApp(ui, server)
