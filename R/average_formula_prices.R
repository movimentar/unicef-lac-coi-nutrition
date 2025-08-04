# function estimates mean formula price for LAC countries available in data set.
average_formula_prices <- function(formula_price){
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
    mean(na.rm = TRUE) %>% 
    return()
}

