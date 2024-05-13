clean_cost_beneficiary <- function(df){
  
  # function calculates intervention_id medians per emergency and adds them as a
  # new column.
  obtain_median_values <- function(df, var){
    median_df <- df %>% 
      filter({{var}} != 0) %>% 
      filter(!(is.na({{var}}) | is.nan({{var}}) | is.infinite({{var}}))) %>% 
      group_by(emergency, intervention_id) %>% 
      summarise("{{var}}_median" := median({{var}}))
    
    df %>% 
      left_join(median_df, by = c("emergency", "intervention_id")) %>% 
      return()
  }
  
  # create df with values to replace flagged data
  replace_df <- df %>% 
    obtain_median_values(total_cost) %>% 
    obtain_median_values(total_beneficiaries) %>% 
    obtain_median_values(imp_cost_person)
  
  # replace flagged values by respective medians
  # and remove complementary columns
  replace_df %>% 
    mutate(
      total_cost = if_else(total_cost_replace_flag, total_cost_median, total_cost),
      total_beneficiaries = if_else(total_beneficiaries_replace_flag, total_beneficiaries_median, total_beneficiaries),
      imp_cost_person = if_else(imp_cost_person_replace_flag, imp_cost_person_median, imp_cost_person)) %>% 
    select(-total_cost_replace_flag, 
           -total_beneficiaries_replace_flag,
           -imp_cost_person_replace_flag) %>% 
    select(-ends_with("median")) %>% 
    return()
  
}

