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
  
  # filtering people in need and targeted
  # removing others category from PiN and countries with 0 observations
  clean_pin_data <- tidy_pin_data %>% 
    filter(indicator_type %in% c("people_need", "people_targeted") & target_group != "Others") %>%
    group_by(emergency, country, target_group, indicator_type) %>%
    summarise(total_pin = sum(values, na.rm = TRUE)) %>% 
    filter(total_pin != 0) %>% 
    pivot_wider(names_from = indicator_type,
                values_from = total_pin)
  
  # replace people in need by people targeted for three emergencies due to high
  # PIN values and comparatively low delivery values.
  clean_pin_data <- clean_pin_data %>%
    mutate(
      total_pin = case_when(
        emergency == "Eta-Iota" & country == "Honduras" ~ people_targeted,
        emergency == "Migration flows" &
          country %in% c("Colombia", "Peru") ~ people_targeted,
        .default = people_need
      )
    ) %>%
    select(-people_need,-people_targeted)
  
  return(clean_pin_data)
}


