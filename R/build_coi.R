build_coi<- function(cost_beneficiary_clean,
                     pin_percentages,
                     pin_data_clean,
                     intervention_list,
                     median_costs_cleaned){
  
  # convert intervention_id col to factor using levels from intervention_list
  cost_beneficiary_clean <-  cost_beneficiary_clean %>%
    mutate(intervention_id = factor(intervention_id,
                                    levels = intervention_list$intervention_id))
  
  # add target_group column
  cost_beneficiary_clean <- cost_beneficiary_clean %>%
    mutate(intervention_id = as_factor(intervention_id)) %>%
    left_join(select(intervention_list, intervention_id, target_group),
              by = "intervention_id") %>% 
    relocate(target_group, .after = emergency)
  
  # sum costs and beneficiaries per year to obtain totals for whole three year
  # period
  three_year_cost_beneficiaries <- cost_beneficiary_clean %>%
    group_by(emergency,
             country,
             target_group,
             intervention_id,
             intervention_name) %>% 
    summarise(total_cost = sum(total_cost, na.rm = TRUE),
              total_beneficiaries = sum(total_beneficiaries, na.rm = TRUE))
  
  # add pin_percentages(demography), pin data and median_costs columns. Remove
  # category columns/interventions by filtering out NAs from ages_relative_pin col
  coi_df <- three_year_cost_beneficiaries %>%
    left_join(
      pin_percentages,
      by = c("intervention_id", "target_group", "country")) %>% 
    select(-int_age_range) %>% 
    left_join(pin_data_clean, by = c("emergency", "country", "target_group")) %>% 
    filter(!is.na(ages_relative_pin)) %>% 
    left_join(median_costs_cleaned, by = c("intervention_id", "emergency")) %>% 
    mutate(intervention_id = factor(intervention_id,
                                    levels = intervention_list$intervention_id))
  return(coi_df)
}

