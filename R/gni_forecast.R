# gni_forecast.R
# -------------------------------------------------------------------
# Context-specific forecasts of REAL GNI PER CAPITA for:
#   - Eta–Iota: Guatemala, Honduras, Nicaragua
#   - Migration flows: Colombia, Honduras, Peru
#
# Uses World Bank constant-dollar per-capita GNI (NY.GNP.PCAP.KD)
# and UN WPP 2022 population to compute population-weighted means.
# Output unit: constant 2015 US$ per capita.
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(WDI)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

options(scipen = 999)

eta_iota_ctys  <- c("Guatemala", "Honduras", "Nicaragua")
migration_ctys <- c("Colombia", "Honduras", "Peru")

# ---------------------------
# 1) Load WPP 2022 population
# ---------------------------
get_wpp_pop <- function() {
  # Load datasets into the workspace (they are not exported)
  utils::data("pop1dt", package = "wpp2022")
  utils::data("popproj1dt", package = "wpp2022")
  
  # pop1dt & popproj1dt have columns: name, year, pop (in thousands)
  hist <- tibble::as_tibble(get("pop1dt")) |>
    dplyr::select(country = name, year, pop)
  proj <- tibble::as_tibble(get("popproj1dt")) |>
    dplyr::select(country = name, year, pop)
  
  dplyr::bind_rows(hist, proj) |>
    dplyr::mutate(pop_persons = as.numeric(pop) * 1000) |>
    dplyr::select(country, year, pop_persons)
}

# Group population (persons) by year for a set of countries
get_group_pop <- function(countries) {
  get_wpp_pop() |>
    dplyr::filter(country %in% countries) |>
    dplyr::group_by(year) |>
    dplyr::summarize(pop_group = sum(pop_persons, na.rm = TRUE), .groups = "drop")
}

# ---------------------------------------
# 2) Real GNI per capita (constant USD)
# ---------------------------------------
# NY.GNP.PCAP.KD = GNI per capita, constant 2015 US$
get_wdi_gni_pc_const <- function(countries, start_year = 1980, end_year = 2022) {
  WDI(country = "all", indicator = "NY.GNP.PCAP.KD",
      start = start_year, end = end_year, extra = FALSE) |>
    tibble::as_tibble() |>
    dplyr::rename(gni_pc_const = NY.GNP.PCAP.KD) |>
    dplyr::filter(country %in% countries)
}

# Build the population-weighted per-capita series and forecast to end_forecast
build_group_series <- function(countries, end_forecast = 2080, use_log = TRUE) {
  
  wdi_cty  <- get_wdi_gni_pc_const(countries)
  pop_cty  <- get_wpp_pop() |> dplyr::filter(country %in% countries)
  
  # Join at country-year level to compute per-year weighted average
  df <- wdi_cty |>
    dplyr::left_join(pop_cty, by = c("country", "year")) |>
    dplyr::filter(year >= 1980, !is.na(gni_pc_const), !is.na(pop_persons))
  
  # Population-weighted mean across countries for each year
  grp <- df |>
    dplyr::group_by(year) |>
    dplyr::summarize(
      gni_pc_const = sum(gni_pc_const * pop_persons, na.rm = TRUE) /
        sum(pop_persons, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Last observed year (avoid any NA years)
  last_obs_year <- max(grp$year[is.finite(grp$gni_pc_const)], na.rm = TRUE)
  
  # Forecast gni_pc_const with log-linear model to keep forecasts positive
  if (use_log) {
    fit <- lm(log(gni_pc_const) ~ year, data = dplyr::filter(grp, year <= last_obs_year))
    pred_years <- tibble::tibble(year = seq.int(last_obs_year + 1, end_forecast))
    pred_vals  <- exp(predict(fit, newdata = pred_years))
  } else {
    fit <- lm(gni_pc_const ~ year, data = dplyr::filter(grp, year <= last_obs_year))
    pred_years <- tibble::tibble(year = seq.int(last_obs_year + 1, end_forecast))
    pred_vals  <- as.numeric(predict(fit, newdata = pred_years))
    pred_vals  <- pmax(pred_vals, 1)  # floor at $1 in constant 2015 US$
  }
  
  forecast <- pred_years |> dplyr::mutate(gni_pc_const = pred_vals)
  
  # Attach group population (persons) for all years 1980..end_forecast
  pops_group <- get_group_pop(countries) |>
    dplyr::filter(year >= 1980, year <= end_forecast)
  
  dplyr::bind_rows(grp, forecast) |>
    dplyr::left_join(pops_group, by = "year") |>
    dplyr::arrange(year)
}

# ----------------------------------------------------
# 3) Build forecasts for both contexts and combine
# ----------------------------------------------------
gni_eta <- build_group_series(eta_iota_ctys,   end_forecast = 2080, use_log = TRUE) |>
  dplyr::mutate(emergency = "Eta-Iota")

gni_mig <- build_group_series(migration_ctys, end_forecast = 2080, use_log = TRUE) |>
  dplyr::mutate(emergency = "Migration flows")

gni_forecast_tbl <- dplyr::bind_rows(gni_eta, gni_mig) |>
  dplyr::select(
    emergency,
    year,
    real_gni_pc_const_usd = gni_pc_const,
    pop_group
  )

# If you prefer 2010 US$ instead of 2015 US$, set a scalar rebase factor:
# Example (illustrative): rebase 2015->2010 by CPI ratio 2010/2015
rebasing_factor_2015_to_2010 <- NULL  # e.g., 100/107.3

if (!is.null(rebasing_factor_2015_to_2010)) {
  gni_forecast <- gni_forecast |>
    dplyr::mutate(real_gni_pc_2010usd = real_gni_pc_const_usd * rebasing_factor_2015_to_2010)
}

readr::write_csv(gni_forecast_tbl, "raw_data/gni_forecast.csv")
