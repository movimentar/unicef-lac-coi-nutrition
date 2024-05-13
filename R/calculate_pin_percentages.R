# This function uses the wpp2022 package which can be found here:
# https://github.com/PPgp/wpp2022
# The package contains demographic data from the UN population projections. The
# data is made easily accessible through this package in tidy data sets.

calculate_pin_percentages <- function(intervention_list, emergency_list){
  
  # load historical age estimates from wpp2022 package
  data("popAge1dt")
  # load Percent age-specific fertility rate from wpp2022 package
  data("percentASFR1dt")
  
  # get vector with countries involved in the study
  countries <- emergency_list %>% 
    pull(country) %>% 
    unique()
  
  # retrieve pop data of countries from wpp2022 data set
  country_pop_2020 <- popAge1dt %>%
    filter(year == 2020 & name %in% countries)
  
  # retrieve age-specific fertility data of countries from wpp2022 data set
  age_fertility <- percentASFR1dt %>% 
    filter(year == 2020 & name %in% countries) %>% 
    select(name, age, pasfr) %>% 
    mutate(age_fertility = pasfr/100) #convert percentages to decimals
  
  # join age-specific fertility to pop data
  country_pop_2020 <- country_pop_2020 %>% 
    left_join(age_fertility, by = c("name", "age"))
  
  # label age groups according to target group and calculate populations for each
  # target group from the population data
  target_pop <- country_pop_2020 %>% 
    mutate(target_group = case_when(
      age < 5 ~ "Children under five",
      age >= 15 & age <= 49 ~ "Pregnant and lactating women"
    )) %>% 
    mutate(target_pop = case_when(
      target_group == "Children under five" ~ pop,
      target_group == "Pregnant and lactating women" ~ popF * age_fertility
    ))
  
  
  # calculate total population of children U5 and PLW per country
  target_pop_totals <- target_pop %>% 
    filter(!is.na(target_pop)) %>% 
    group_by(name, target_group) %>% 
    summarise(total_target_pop = sum(target_pop))
  
  # calculate the percentage of each cohort relative to the children under five
  # total population. i.e. children age 0 make x% of the total children under five
  # population in Mexico.
  target_pop <- target_pop %>% 
    filter(!is.na(target_pop)) %>% 
    left_join(target_pop_totals, by = c("name", "target_group")) %>% 
    mutate(relative_target_pop = target_pop / total_target_pop) %>% 
    select(name, target_group, age, relative_target_pop)
  
  # setup a table to catch all the relevant countries and age ranges to later
  # return
  age_ranges <- intervention_list %>% 
    select(intervention_id, target_group, int_age_range) %>% 
    filter(!is.na(int_age_range))
  
  age_ranges <- map2(countries, list((age_ranges)), \(c, df) mutate(df, country = c)) %>% 
    list_rbind()
  
  # Extract age ranges from return table to process later
  
  age_ranges <- age_ranges %>%
    mutate(low_end = as.integer(str_extract(
      int_age_range, "(\\d+)\\s-\\s(\\d+)", group = 1
    )),
    upp_end = as.integer(str_extract(
      int_age_range, "(\\d+)\\s-\\s(\\d+)", group = 2
    )))
  
  # function that sums relative populations from a lookup table (target_pop df)
  # depending on the age ranges each intervention in the df calls for.
  calc_relative_pop <- function(df, age_lookup){
    # convert months to years
    params <- df %>% 
      mutate(low_end = trunc(low_end / 12),
             upp_end = trunc(upp_end / 12)) %>% 
      select(country, low_end, upp_end)
    
    # look up age values an sum them
    look_up_age <- function(low_end, upp_end, country, age_lookup){
      age_lookup %>% 
        filter(name == {{country}} & (age >= {{low_end}} & age <= {{upp_end}})) %>% 
        pull(relative_target_pop) %>% 
        sum() %>% 
        return()
    }
    
    # get relative ages in vector form to add to df with mutate
    relative_ages <- pmap_dbl(params, look_up_age, age_lookup)
    
    df %>% 
      mutate(ages_relative_pin = relative_ages) %>% 
      return()
    
  }
  
  # Convert age ranges to percentages relative to PiN populations.
  ages_relative_pin <- age_ranges %>% 
    calc_relative_pop(target_pop) %>% 
    select(-low_end, -upp_end) %>% 
    mutate(ages_relative_pin = if_else( # replace PLW NAs by 1
      target_group == "Pregnant and lactating women",
      1,
      ages_relative_pin
    ))
  
  return(ages_relative_pin)
}






  