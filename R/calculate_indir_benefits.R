# function calculates indirect benefits (baby formula costs and costs from
# cognitive loses) from the estimated number of children exclusively breastfed
# found in the coi_dir_benefits. Output is a data frame with the indirect costs
# by emergency and coverage.
calculate_indir_benefits <- function(coi_dir_benefits,
                                     mean_earnings,
                                     income_share,
                                     gni_forecast,
                                     formula_price,
                                     formula_packages) {
  # filter relevant labour incomes contries for study
labour_income_filtered <- income_share %>% 
  # filter by countries of study
  filter(ref_area %in% c("GTM", "HND", "NIC", "COL", "PER")) %>% 
  # filter year 2020 only
  filter(time == "2020") %>%
  # create new cols to join with other tables
  mutate(country = case_when(
    ref_area == "COL" ~ "Colombia",
    ref_area == "GTM" ~ "Guatemala",
    ref_area == "HND" ~ "Honduras",
    ref_area == "NIC" ~ "Nicaragua",
    ref_area == "PER" ~ "Peru"
  ),
  indicator_name = "Labour income share as a percent of GDP")

# calculate mean labour income of countries filtered
countries_labour_income <- mean(pull(labour_income_filtered, obs_value))

# prepare formula price data
mean_formula_price <- formula_price %>%
  rename(country = Country,
         formula_unit_cost = `Unit cost for price of lowest economy brand of formula per 900-gram container (US$)`) %>% 
  # replace "Not Available by NA"
  mutate(
    formula_unit_cost = 
      if_else(formula_unit_cost == "Not Available",
              NA,
              formula_unit_cost)) %>% 
  mutate(formula_unit_cost = as.double(formula_unit_cost)) %>% 
  # filter countries of study
  filter(country %in% c(
    "Guatemala",
    "Honduras",
    "Nicaragua",
    "Colombia",
    "Peru"
  )) %>% 
  pull(formula_unit_cost) %>% 
  mean(na.rm = TRUE)

# gni for working years only
gni_working_years <- gni_forecast %>% 
filter(year >= 2038 & year <= 2080)

# function for calculating formula costs
calculate_formula_cost <- function(dependent_children, formula_units_needed, formula_unit_cost){
  return(dependent_children * formula_units_needed * formula_unit_cost)
}

# function to calculate potential future income lost due to cognitive losses
# See : https://www.aliveandthrive.org/sites/default/files/1360-the-cost-of-not-breastfeeding-faq-v4-.pdf

calculate_cognitive_costs <-
  function(children_not_breastfed,
           gni,
           iq_increase = 0.0262,
           labour_share) {
    
    map_dbl(gni, \(x) x * children_not_breastfed * iq_increase * (labour_share / 100)) %>% 
      sum() %>% 
      return()
  }

# calculate indirect benefits
coi_indir_benefits <- coi_dir_benefits[["breastfeeding"]] %>%
  filter(indicator_name_absolute == "Exclusively breastfed children") %>%
  select(-coverage_0) %>%
  pivot_longer(
    cols = saved_implemented:saved_95,
    names_to = "coverage_type",
    names_prefix = "saved_",
    values_to = "value"
  ) %>%
  mutate(
    formula_cost = calculate_formula_cost(
      dependent_children = value,
      formula_units_needed = formula_packages,
      formula_unit_cost = mean_formula_price
    ),
    cognitive_cost = map_dbl(
      value,
      \(x) calculate_cognitive_costs(
        children_not_breastfed = x,
        gni = gni_working_years[["real_gni"]],
        labour_share = countries_labour_income
      )
    )
  )

# wrangle data for presentation
coi_indir_benefits <- coi_indir_benefits %>%
  ungroup() %>%
  select(-value,-indicator_category) %>%
  pivot_longer(
    cols = c("formula_cost", "cognitive_cost"),
    names_to = "indicator_name",
    values_to = "value"
  ) %>%
  mutate(
    indicator_name_absolute = if_else(
      indicator_name == "formula_cost",
      "Cost of feeding a child with formula for the first 2 years",
      "Potential future income lost due to cognitive losses"
    )
  ) %>%
  pivot_wider(names_from = c("emergency", "coverage_type"),
              values_from = value)

return(coi_indir_benefits)
}
