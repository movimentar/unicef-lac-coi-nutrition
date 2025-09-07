# # function calculates indirect benefits (baby formula costs and costs from
# # cognitive loses) from the estimated number of children exclusively breastfed
# # found in the coi_dir_benefits. Output is a data frame with the indirect costs
# # by emergency and coverage.
# calculate_indir_benefits <- function(coi_dir_benefits,
#                                      mean_earnings,
#                                      income_share,
#                                      gni_forecast,
#                                      mean_formula_price,
#                                      formula_packages) {
#   # filter relevant labour incomes contries for study
# labour_income_filtered <- income_share %>%
#   # filter by countries of study
#   filter(ref_area %in% c("GTM", "HND", "NIC", "COL", "PER")) %>%
#   # filter year 2020 only
#   filter(time == "2020") %>%
#   # create new cols to join with other tables
#   mutate(country = case_when(
#     ref_area == "COL" ~ "Colombia",
#     ref_area == "GTM" ~ "Guatemala",
#     ref_area == "HND" ~ "Honduras",
#     ref_area == "NIC" ~ "Nicaragua",
#     ref_area == "PER" ~ "Peru"
#   ),
#   indicator_name = "Labour income share as a percent of GDP")
# 
# # calculate mean labour income of countries filtered
# countries_labour_income <- mean(pull(labour_income_filtered, obs_value))
# 
# 
# # gni for working years only
# gni_working_years <- gni_forecast %>%
# filter(year >= 2038 & year <= 2080)
# 
# # function for calculating formula costs
# calculate_formula_cost <- function(dependent_children, formula_units_needed, formula_unit_cost){
#   return(dependent_children * formula_units_needed * formula_unit_cost)
# }
# 
# # function to calculate potential future income lost due to cognitive losses
# # See : https://www.aliveandthrive.org/sites/default/files/1360-the-cost-of-not-breastfeeding-faq-v4-.pdf
# 
# # calculate_cognitive_costs <-
# #   function(children_not_breastfed,
# #            gni,
# #            iq_increase = 0.0262,
# #            labour_share) {
# #
# #     map_dbl(gni, \(x) x * children_not_breastfed * iq_increase * (labour_share / 100)) %>%
# #       sum() %>%
# #       return()
# #   }
# 
# # function to calculate potential future income lost due to cognitive losses
# # See : https://www.aliveandthrive.org/sites/default/files/1360-the-cost-of-not-breastfeeding-faq-v4-.pdf
# # earnings gain per EBF child (2.62 IQ points * 1.067% earnings per IQ point ≈ 0.02796)
# EARNINGS_UPLIFT <- 0.0262 * 0.01067  # ≈ 0.02796
# 
# calculate_cognitive_costs <- function(children_not_breastfed,
#                                       gni,            # vector of real GNI per capita (2010 USD)
#                                       years,          # matching vector of years (e.g., 2038:2080)
#                                       labour_share,   # % of GDP
#                                       base_year = 2022,
#                                       discount_rate = NULL,
#                                       uplift = EARNINGS_UPLIFT) {
# 
#   # annual benefit stream (real, undiscounted)
#   annual <- gni * (labour_share / 100) * uplift * children_not_breastfed
# 
#   # discount if requested
#   if (!is.null(discount_rate)) {
#     df <- (1 + discount_rate)^(years - base_year)
#     annual <- annual / df
#   }
#   sum(annual)
# }
# 
# # calculate indirect benefits
# coi_indir_benefits <- coi_dir_benefits[["breastfeeding"]] %>%
#   filter(indicator_name_absolute == "Exclusively breastfed children") %>%
#   select(-coverage_0) %>%
#   pivot_longer(
#     cols = saved_implemented:saved_95,
#     names_to = "coverage_type",
#     names_prefix = "saved_",
#     values_to = "value"
#   ) %>%
#   mutate(
#     formula_cost = calculate_formula_cost(
#       dependent_children = value,
#       formula_units_needed = formula_packages,
#       formula_unit_cost = mean_formula_price
#     ),
#     cognitive_cost = map_dbl(
#       value,
#       \(x) calculate_cognitive_costs(
#         children_not_breastfed = x,
#         gni = gni_working_years[["real_gni"]],
#         labour_share = countries_labour_income
#       )
#     )
#   )
# 
# # wrangle data for presentation
# coi_indir_benefits <- coi_indir_benefits %>%
#   ungroup() %>%
#   select(-value,-indicator_category) %>%
#   pivot_longer(
#     cols = c("formula_cost", "cognitive_cost"),
#     names_to = "indicator_name",
#     values_to = "value"
#   ) %>%
#   mutate(
#     indicator_name_absolute = if_else(
#       indicator_name == "formula_cost",
#       "Cost of feeding a child with formula for the first 2 years",
#       "Potential future income lost due to cognitive losses"
#     )
#   ) %>%
#   pivot_wider(names_from = c("emergency", "coverage_type"),
#               values_from = value)
# 
# return(coi_indir_benefits)
# }

# calculate_indir_benefits.R
# Indirect benefits from additional exclusively breastfed (EBF) children.

# CODE 2
# calculate_indir_benefits <- function(coi_dir_benefits,
#                                      mean_earnings,      # kept for compatibility (unused)
#                                      income_share,       # ILO labour income share (% of GDP)
#                                      gni_forecast,       # cols: year, real_gni; optional: pop
#                                      mean_formula_price, # mean price per 900g tin
#                                      formula_packages) { # tins needed over 24 months
#   
#   # --- Labour income share: mean across study countries (2020) ---
#   labour_income_filtered <- income_share %>%
#     dplyr::filter(ref_area %in% c("GTM","HND","NIC","COL","PER"),
#                   time == "2020")
#   countries_labour_income <- mean(dplyr::pull(labour_income_filtered, obs_value), na.rm = TRUE)
#   
#   # --- Real GNI *per capita* (2010 USD) for working years 2038–2080 ---
#   gni_working_years <- gni_forecast %>%
#     dplyr::filter(year >= 2038 & year <= 2080) %>%
#     dplyr::mutate(
#       # If a population column exists, assume real_gni is an aggregate in *millions*
#       # and pop is in *thousands*; convert to per-capita. Otherwise, treat as per-capita already.
#       real_gni_pc_2010usd = dplyr::case_when(
#         "pop" %in% names(.) ~ real_gni * 1000 / pop,
#         TRUE                ~ real_gni
#       )
#     )
#   gni_pc_vec <- gni_working_years$real_gni_pc_2010usd
#   years_vec  <- gni_working_years$year
#   
#   # --- Household BMS outlays avoided (two years) ---
#   calculate_formula_cost <- function(dependent_children, formula_units_needed, formula_unit_cost) {
#     dependent_children * formula_units_needed * formula_unit_cost
#   }
#   
#   # --- Cognitive-income preserved (undiscounted) ---
#   # Earnings uplift per additional EBF child:
#   # 2.62 IQ points * 1.067% earnings per IQ point  =  0.0262 * 1.067  ≈ 0.02796 (2.796)
#   EARNINGS_UPLIFT <- 2.62 * 0.01067
#   
#   calculate_cognitive_costs <- function(children_not_breastfed,
#                                         gni_pc, years, labour_share,
#                                         base_year = 2022,
#                                         discount_rate = NULL,
#                                         uplift = EARNINGS_UPLIFT) {
#     annual <- gni_pc * (labour_share / 100) * uplift * children_not_breastfed
#     if (!is.null(discount_rate)) {
#       df <- (1 + discount_rate)^(years - base_year)
#       annual <- annual / df
#     }
#     sum(annual, na.rm = TRUE)
#   }
#   
#   # --- Build indirect benefits from EBF improvements ---
#   indir <- coi_dir_benefits[["breastfeeding"]] %>%
#     dplyr::filter(indicator_name_absolute == "Exclusively breastfed children") %>%
#     dplyr::select(-coverage_0) %>%
#     tidyr::pivot_longer(
#       cols = saved_implemented:saved_95,
#       names_to = "coverage_type",
#       names_prefix = "saved_",
#       values_to = "value"   # additional EBF children vs 0% baseline
#     ) %>%
#     dplyr::mutate(
#       formula_cost = calculate_formula_cost(value, formula_packages, mean_formula_price),
#       cognitive_cost = purrr::map_dbl(
#         value,
#         ~ calculate_cognitive_costs(.x, gni_pc = gni_pc_vec, years = years_vec,
#                                     labour_share = countries_labour_income)
#       )
#     )
#   
#   # --- Reshape for presentation ---
#   out <- indir %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-value, -indicator_category) %>%
#     tidyr::pivot_longer(
#       cols = c("formula_cost", "cognitive_cost"),
#       names_to = "indicator_name",
#       values_to = "value"
#     ) %>%
#     dplyr::mutate(
#       indicator_name_absolute = dplyr::case_when(
#         indicator_name == "formula_cost"  ~ "Cost of feeding a child with formula for the first 2 years",
#         TRUE                               ~ "Potential future income lost due to cognitive losses (undiscounted)"
#       )
#     ) %>%
#     tidyr::pivot_wider(names_from = c("emergency", "coverage_type"),
#                        values_from = value)
#   
#   return(out)
# }
# 

# calculate_indir_benefits.R
# ------------------------------------------------------------
# Computes indirect benefits from additional exclusively breastfed (EBF) children:
#   (a) Household outlays on breast-milk substitutes (BMS) avoided over 24 months
#   (b) Cognitive-income preserved (undiscounted), using per-capita real GNI
#
# Inputs:
# - coi_dir_benefits: list with $breastfeeding table incl. "Exclusively breastfed children"
# - income_share: ILO labour income share dataset (% of GDP), used to scale GNI to labour earnings
# - gni_forecast: emergency-specific per-capita GNI in constant 2015 US$
#     cols = emergency, year, real_gni_pc_const_usd, (optional) pop_group
# - mean_formula_price: mean price per 900g tin (lowest economy brand)
# - formula_packages: tins needed per child for 0–24 months (total)
#
# Notes:
# - Cognitive-income uses the working-life window 2038–2080 (consistent with your study).
# - Earnings uplift per additional EBF child = 2.62 IQ points × 1.067% per IQ point ≈ 0.02796 (2.796%).
# - All values are in constant 2015 US$ (no rebasing here).
# - Returns a wide table with two rows:
#     • "Cost of feeding a child with formula for the first 2 years"
#     • "Potential future income lost due to cognitive losses (undiscounted)"
#   and, for each row, columns by emergency × scenario: _implemented, _30, _95.
# ------------------------------------------------------------

# calculate_indir_benefits.R
# ------------------------------------------------------------
# Computes indirect benefits from additional exclusively breastfed (EBF) children:
#   (a) Household outlays on breast-milk substitutes (BMS) avoided over 24 months
#   (b) Cognitive-income preserved (UNDISCOUNTED), using per-capita real GNI
#
# Inputs
# - coi_dir_benefits: list with $breastfeeding table incl. "Exclusively breastfed children"
# - income_share: ILO labour income share (% of GDP), used to scale GNI to labour earnings
# - gni: emergency-specific per-capita GNI, constant 2015 US$
#        required cols = emergency, year, real_gni_pc_const_usd
#        (optional) pop_group is ignored here
# - mean_formula_price: mean price per 900g tin (lowest economy brand)
# - formula_packages: tins needed per child for 0–24 months (total)
#
# Notes
# - Working-life window = 2038–2080 (consistent with your study specification).
# - Earnings uplift per additional EBF child = 2.62 IQ points × 1.067% per IQ point
#     ≈ 0.02796 (2.796%).
# - All values are constant 2015 US$ (no rebasing here).
# - Output: wide table with rows
#     • "Cost of feeding a child with formula for the first 2 years"
#     • "Potential future income lost due to cognitive losses (undiscounted)"
#   and, for each, columns by emergency × scenario: _implemented, _30, _95.
# ------------------------------------------------------------

calculate_indir_benefits <- function(coi_dir_benefits,
                                     mean_earnings,      # kept for signature compatibility (unused)
                                     income_share,       # ILO labour income share (% of GDP)
                                     gni,                # cols: emergency, year, real_gni_pc_const_usd
                                     mean_formula_price, # mean price per 900g tin
                                     formula_packages) { # tins per child (0–24 months)
  # ---- Basic input checks (fail early with clear messages) ----
  req_gni_cols <- c("emergency", "year", "real_gni_pc_const_usd")
  if (!all(req_gni_cols %in% names(gni))) {
    stop("`gni` must contain columns: ", paste(req_gni_cols, collapse = ", "), call. = FALSE)
  }
  if (!("breastfeeding" %in% names(coi_dir_benefits))) {
    stop("`coi_dir_benefits` must contain a $breastfeeding element.", call. = FALSE)
  }
  
  # ---- Labour income share (mean across study countries, 2020) ----
  labour_income_filtered <- income_share %>%
    dplyr::filter(ref_area %in% c("GTM", "HND", "NIC", "COL", "PER"),
                  time == "2020")
  
  countries_labour_income <- mean(
    dplyr::pull(labour_income_filtered, obs_value),
    na.rm = TRUE
  )
  
  # ---- GNI per capita (constant 2015 US$) for working years 2038–2080, by emergency ----
  # Build a compact look-up with list-columns (years, gni_pc) per emergency.
  gni_by_em <- gni %>%
    dplyr::filter(year >= 2038, year <= 2080) %>%
    dplyr::select(emergency, year, real_gni_pc_const_usd) %>%
    dplyr::group_by(emergency) %>%
    dplyr::summarise(
      years  = list(year),
      gni_pc = list(real_gni_pc_const_usd),
      .groups = "drop"
    )
  
  # ---- Helper: household BMS outlays avoided over 24 months ----
  calculate_formula_cost <- function(n_children, tins_per_child, tin_price) {
    n_children * tins_per_child * tin_price
  }
  
  # ---- Cognitive-income preserved (undiscounted) ----
  # 2.62 IQ points × 1.067% earnings per IQ point  ->  2.62 * 0.01067 ≈ 0.02796 (2.796%)
  EARNINGS_UPLIFT <- 2.62 * 0.01067
  
  calculate_cognitive_costs <- function(n_children,
                                        gni_pc_vec, years_vec,
                                        labour_share_pct,
                                        base_year = 2022,       # unused when undiscounted
                                        discount_rate = NULL,   # hook kept for possible PV sensitivity
                                        uplift = EARNINGS_UPLIFT) {
    # Annual (real) earnings increment per person from EBF-induced IQ uplift
    annual <- gni_pc_vec * (labour_share_pct / 100) * uplift * n_children
    if (!is.null(discount_rate)) {
      df <- (1 + discount_rate)^(years_vec - base_year)
      annual <- annual / df
    }
    sum(annual, na.rm = TRUE)
  }
  
  # ---- Build indirect benefits from EBF improvements, by emergency & scenario ----
  indir <- coi_dir_benefits[["breastfeeding"]] %>%
    dplyr::filter(indicator_name_absolute == "Exclusively breastfed children") %>%
    dplyr::select(-coverage_0) %>%
    tidyr::pivot_longer(
      cols        = saved_implemented:saved_95,
      names_to    = "coverage_type",
      names_prefix= "saved_",
      values_to   = "value"     # additional EBF children vs 0% baseline
    ) %>%
    # Attach the emergency-specific GNI stream (years, gni_pc) via list-columns
    dplyr::left_join(gni_by_em, by = "emergency") %>%
    dplyr::mutate(
      # (a) Formula purchases avoided
      formula_cost = calculate_formula_cost(
        n_children     = value,
        tins_per_child = formula_packages,
        tin_price      = mean_formula_price
      ),
      # (b) Cognitive-income preserved (UNDISCOUNTED), using the correct GNI stream per emergency
      cognitive_cost = purrr::pmap_dbl(
        list(n_children = value, gni_pc_vec = gni_pc, years_vec = years),
        function(n_children, gni_pc_vec, years_vec) {
          calculate_cognitive_costs(
            n_children        = n_children,
            gni_pc_vec        = gni_pc_vec,
            years_vec         = years_vec,
            labour_share_pct  = countries_labour_income,
            discount_rate     = NULL  # undiscounted for the main table
          )
        }
      )
    )
  
  # ---- Reshape for presentation ----
  out <- indir %>%
    dplyr::ungroup() %>%
    dplyr::select(-value, -indicator_category, -years, -gni_pc) %>%
    tidyr::pivot_longer(
      cols = c("formula_cost", "cognitive_cost"),
      names_to  = "indicator_name",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      indicator_name_absolute = dplyr::case_when(
        indicator_name == "formula_cost" ~
          "Cost of feeding a child with formula for the first 2 years",
        TRUE ~
          "Potential future income lost due to cognitive losses (undiscounted)"
      )
    ) %>%
    tidyr::pivot_wider(
      names_from  = c("emergency", "coverage_type"),
      values_from = value
    )
  
  return(out)
}
