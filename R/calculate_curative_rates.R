# this script estimates the percentage of children and PLW that would need
# treatment for malnutrition or anemia based on the CO data received, namely how
# many curative interventions where delivered compared with screenings on the
# same target group.

# the output is a data frame with such percentages (medians of populations),
# including those that resulted in 0.

calculate_curative_rates <- function(coi_df){
  # identify curative and screening interventions from intervention list
  screening_ints <- c("8-a", "13-a")
  curative_ints <- c("9-a", "9-b", "14", "15")
  
  
  curative_rates <- coi_df %>%
    # filter and tag curative and screening interventions
    filter(intervention_id %in% c(screening_ints, curative_ints)) %>% 
    group_by(emergency, country, target_group, intervention_id, intervention_name) %>% 
    summarise(total_beneficiaries = sum(total_beneficiaries, na.rm = TRUE)) %>%
    mutate(intervention_type = case_when(
      intervention_id %in% screening_ints ~ "screening",
      intervention_id %in% curative_ints ~ "curative"
    )) %>% 
    select(-intervention_name) %>% 
    # separate curative and screening intervention summaries to do calculations
    pivot_wider(names_from = c("intervention_id", "intervention_type"),
                values_from = total_beneficiaries) %>%
    mutate(screenings = case_when(
      !is.na(`8-a_screening`) ~ `8-a_screening`,
      !is.na(`13-a_screening`) ~ `13-a_screening`,
    )) %>% 
    pivot_longer(cols = 4:9,
                 names_sep = "_",
                 names_to = c("intervention_id", "intervention_type"),
                 values_to = "total_beneficiaries"
    ) %>% 
    filter(!is.na(total_beneficiaries)) %>% 
    filter(intervention_type == "curative") %>% 
    mutate(curative_rate = total_beneficiaries / screenings) %>% 
    mutate(curative_rate = if_else(is.nan(curative_rate), 0, curative_rate)) %>%
    group_by(emergency, intervention_id) %>% 
    summarise(median_curative_rate = median(curative_rate)) %>% 
    # convert intervention_id column in factor based on coi_df
    mutate(intervention_id = factor(intervention_id, 
                                    levels = levels(coi_df$intervention_id))) %>% 
    arrange(intervention_id) %>% 
    return()
}

