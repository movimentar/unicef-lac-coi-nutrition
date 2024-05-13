
build_cost_beneficiary <-
  function(cost_benef_list,
           intervention_list,
           emergency_list) {
    # store cost data as a separate variable
    costs_df <- cost_benef_list[["costs"]] %>%
      # calculate costs total
      group_by(id, emergency_id, year, intervention_id) %>%
      summarise(total_cost = sum(value))
    
    # store beneficiary data as a separate variable
    beneficiaries_df <- cost_benef_list[["beneficiaries"]] %>%
      # filter people_reached only
      filter(benef_category == "people_reached") %>%
      # calculate beneficiary totals ignoring sub categories (i.e. sex)
      group_by(id, emergency_id, year, intervention_id, benef_category) %>%
      summarise(total_benef = sum(value))
    
    # store comment data as a separate variable
    comments_df <- cost_benef_list[["costs"]] %>%
      filter(is.na(comments) == FALSE) %>%
      group_by(id, comments) %>%
      summarise()
    
    # build cost_benef df by joining cost, beneficiaries and comments data frames
    cost_benef_df <- costs_df %>%
      left_join(beneficiaries_df,
                by = c("id", "emergency_id", "year", "intervention_id")) %>%
      left_join(comments_df, by = "id")
    
    # add metadata from intervention list and emergency list
    cost_benef_df <- cost_benef_df %>%
      left_join(emergency_list, by = "emergency_id") %>%
      left_join(select(intervention_list, intervention_id, intervention_name),
                by = "intervention_id") %>%
      # select final colums for product df
      ungroup() %>%
      select(
        emergency_id,
        country,
        emergency,
        year,
        intervention_id,
        intervention_name,
        total_cost,
        total_beneficiaries = total_benef,
        comments
      )
    
    # calculate intervention implementation cost per person
    cost_benef_df <- cost_benef_df %>% 
      mutate(imp_cost_person = total_cost / total_beneficiaries) %>% 
      relocate(imp_cost_person, .before = comments)
    
    return(cost_benef_df)
  }
