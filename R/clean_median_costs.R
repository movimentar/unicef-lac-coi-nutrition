clean_median_costs <- function(df){
  
  # calculate median of medians
  median_medians <- df %>% 
    pull(median_cost_person) %>% 
    median(na.rm = TRUE)
  
  # replace NAs from median df by median of medians
  df <- df %>% 
    mutate(median_cost_person = if_else(is.na(median_cost_person),
                                        median_medians,
                                        median_cost_person))
  
  # replace values higher than 100 by median
  
  df <- df %>% 
    mutate(median_cost_person = if_else(median_cost_person > 100,
                                        median_medians,
                                        median_cost_person))
  
  return(df)
}


