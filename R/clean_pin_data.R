# function wrangles pin_data df into a tidy data set and cleans it by removing
# columns and countries with no PiN data.
clean_pin_data <- function(pin_data){
  tidy_pin_data <- pin_data %>% 
    select(-pin_type) %>% 
    pivot_longer(
      cols = starts_with("people"),
      names_to = c("indicator_type", "target_group", "year"),
      names_pattern = "(.*)\\.(.*)\\.(.*)",
      values_to = "values") %>% 
    relocate(comments, .after = last_col())
  
  # removing others category from PiN and countires with 0 observations
  clean_pin_data <- tidy_pin_data %>% 
    filter(indicator_type == "people_need" & target_group != "Others") %>%
    group_by(emergency, country, target_group) %>%
    summarise(total_pin = sum(values, na.rm = TRUE)) %>% 
    filter(total_pin != 0)
  
  return(clean_pin_data)
}


