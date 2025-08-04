# function makes a df containing the coverage data to be used in the LiST model.
# Coverage data is contained in coverage_costs, metadata from intervention_list
# df is added and then the df is filtered to retain interventions in LiST only.
build_list_input <- function(coverage_costs, intervention_list){
  coverage_costs %>% 
    left_join(select(intervention_list, intervention_id, list_name, in_list), by = "intervention_id") %>% 
    filter(in_list == TRUE) %>% 
    select(emergency, country, intervention_name, coverage, list_name) %>%
    filter(!is.na(list_name)) %>% 
    mutate(coverage = coverage * 100) %>% 
    filter(!is.na(coverage)) %>% 
    relocate(target_group, .after = country) %>% 
    mutate(coverage = if_else(coverage > 95, # make 95 the max coverage
                              95,
                              coverage)) %>% 
    return()
}

