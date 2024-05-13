# function takes coi_df and curative_rates df, adds the curative rates and
# calculates an estimation of the people in need of each individual intervention
# (ideal_reached). Using this estimation coverage and ideal costs columns are
# calculated.
calc_coverage_costs <- function(coi_df, curative_rates){
  
  # function adds a column that describes the percentage of the population that
  # needs to receive the curative intervention based on the values in the
  # curative_rates df. If the value is NA the intervention is not curative and
  # needs to be applied to 100% of the population, if it is 0 this means that the
  # intervention is curative but we didn't have data so we are using a value based
  # on our data in the curative_rates df. If there is a value for the intervention
  # we use the value in curative_rates df.
  add_curative_rates <- function(coi_df, curative_rates){
    # get median of medians from the curative rates
    highest_curative_rate <- curative_rates %>% 
      pull(median_curative_rate) %>% 
      max()
    
    coi_df %>% 
      left_join(curative_rates,
                by = c("emergency",
                       "intervention_id")) %>% 
      mutate(median_curative_rate = case_when(
        is.na(median_curative_rate) ~ 1,
        median_curative_rate == 0 ~ highest_curative_rate,
        median_curative_rate > 0 ~ median_curative_rate
      ))
  }
  
  # add curative_rates column ready to use in calculations
  coi_df <- coi_df %>% 
    add_curative_rates(curative_rates)
  
  # calculate coverages and costs for delivering to all people that need the
  # interventions
  coi_df <- coi_df %>% 
    mutate(ideal_delivered = total_pin * ages_relative_pin * median_curative_rate) %>% 
    mutate(coverage = total_beneficiaries / ideal_delivered) %>% 
    mutate(ideal_cost = ideal_delivered * median_cost_person)
  
  return(coi_df)
}





