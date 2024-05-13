
explore_cost_beneficiary <- function(df){
  
  # remove observations with no data if df is cost_beneficiary_cleaned
  if("no_data_flag" %in% names(df)){
    df <- df %>% 
      filter(!no_data_flag)
  }
  
  
  # define function that plots a specific numeric variable from the df into a
  # box plot. Distributions are disaggregated by intervention_id and emergency.
  # Observations with values 0 in giben variable get filtered out.
  explore_distributions <- function(df, var){
    
    # filter out observations with value 0
    df <- df %>% 
      filter({{var}} != 0)
    
    # plot filtered variable
    df %>%
      ggplot(aes(x = {{var}}, fill = intervention_id)) +
      geom_boxplot() +
      facet_wrap(~emergency) +
      scale_x_continuous(labels = label_number())
  }
  
  
  initial_exploration_plots <- list(
    # plot cost distributions
    costs = explore_distributions(df, total_cost),
    # plot beneficiaries distributions
    beneficiaries = explore_distributions(df, total_beneficiaries),
    # plot imp_cost_person distributions
    cost_person = explore_distributions(df, imp_cost_person)
  )
  
  return(initial_exploration_plots)
}


