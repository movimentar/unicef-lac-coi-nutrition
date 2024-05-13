
calculate_cost_medians <- function(df){
  median_df <- df %>% 
    group_by(emergency, intervention_id) %>% 
    summarise(median_cost_person = median(imp_cost_person, na.rm = TRUE)) %>% 
    return()
}




