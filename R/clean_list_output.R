# function to clean list_output file to a shape that the analysis accepts
clean_list_output <- function(list_output) {
  # clean df
  list_output_clean <- list_output %>%
    mutate(
      emergency = if_else(
        str_detect(`File name`, "Eta-Iota"),
        "Eta-Iota",
        "Migration flows"
      ),
      coverage_type = str_extract(`File name`, "_(.*).PJNZ", group = 1)
    ) %>%
    filter(
      is.na(Configuration) | Configuration %in% c(
        "Total (0-59 months)",
        "<1 month and 1-5 months",
        "<1 month",
        "Pregnant women with anemia",
        "Pregnant women with iron-deficiency anemia",
        "Women of reproductive age with anemia",
        "Women of reproductive age with iron-deficiency anemia"
      )
    ) %>%
    filter(!(str_detect(Indicator, "Global") &
               Configuration == "<1 month")) %>%
    mutate(
      indicator_name = if_else(str_detect(Indicator, "anemia"),
                               Configuration,
                               Indicator),
      indicator_category = case_when(
        str_detect(indicator_name, "mortality") |
          str_detect(indicator_name, "stillbirths") ~ "mortality_rates",
        str_detect(indicator_name, "\\b(stunting|wasting)\\b") ~ "nutrition",
        str_detect(indicator_name, "breastfe") ~ "breastfeeding",
        str_detect(indicator_name, "anemia") ~ "anemia"
      )
    ) %>%
    mutate(
      indicator = case_when(
        Indicator == "Neonatal mortality rate (deaths per 1,000 live births)" ~ "neonatal_rate",
        Indicator == "Infant mortality rate (deaths per 1,000 live births)" ~ "infant_rate",
        Indicator == "Under five mortality rate (deaths per 1,000 live births)" ~ "u5_rate",
        Indicator == "Maternal mortality rate (deaths per 100,000 women aged 15-49)" ~ "maternal_rate",
        Indicator == "Stillbirth rate (stillbirths per 1,000 total births (live and stillbirths))" ~ "stillbirth_rate",
        Indicator == "Global stunting (<-2 SD) rate" ~ "stunting_rate",
        Indicator == "Global wasting (<-2 SD) rate" ~ "wasting_rate",
        Indicator == "Percent of children (<6 mo) exclusively breastfed" ~ "exclusive_breastfeeding_rate",
        Indicator == "Prevalence of early initiation of breastfeeding" ~ "breastfeeding_initiation_rate",
        Configuration == "Pregnant women with anemia" ~ "pregnant_anemia",
        Configuration == "Pregnant women with iron-deficiency anemia" ~ "pregnant_iron_def",
        Configuration == "Women of reproductive age with anemia" ~ "women_anemia",
        Configuration == "Women of reproductive age with iron-deficiency anemia" ~ "women_iron_def"
      )
    ) %>%
    mutate(
      Country = if_else(
        Country == "Venezuela" & coverage_type == "implemented",
        str_to_title(str_extract(
          `File name`, "Migration\\\\(.*)_", group = 1
        )),
        Country
      )
    ) %>%
    select(
      country = Country,
      emergency,
      coverage_type,
      indicator_category,
      indicator,
      indicator_name,
      baseline_value = `2020`,
      after_delivery_value = `2021`
    )
  
  # replace Venezuela obs by corresponding migration countries.
  
  venezuela_obs <- list_output_clean %>%
    filter(country == "Venezuela")
  
  migration_countries <- list_output_clean %>%
    filter(country != "Venezuela") %>%
    filter(emergency == "Migration flows") %>%
    pull(country) %>%
    unique()
  
  # function to replace all values from the column country by a different country
  replace_country <- function(df, country) {
    df %>%
      mutate(country = {{country}}) %>%
      return()
  }
  
  venezuela_obs_repalced <-
    map2(list(venezuela_obs), migration_countries, replace_country) %>%
    list_rbind()
  
  list_output_clean <- list_output_clean %>%
    filter(country != "Venezuela") %>%
    rbind(venezuela_obs_repalced)
  
  return(list_output_clean)
  
}

