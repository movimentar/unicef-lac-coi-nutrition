# from trimmed_coverage_costs, calculate absolute values of health outcomes
# avoided or improved using LiST output and LiST metadata file. Returns a listt
# with two data frames: one containing indicators related to malnutrition and
# mortality and a second one with breastfeeding indicators.
estimate_dir_benefits <-
  function(trimmed_coverage_costs,
           list_output,
           list_metadata) {
    # prepare list output
    # filter indicators that are not used anymore.
    # convert after delivery values to rates
        benef_indicators <- list_output %>%
      filter(!(
        indicator %in% c(
          "maternal_deaths",
          "neonatal_deaths",
          "stillbirths",
          "u5_deaths"
        )
      )) %>%
      left_join(list_metadata,
                by = c("indicator", "indicator_category", "indicator_name")) %>%
      mutate(
        final_reduced_rates = case_when(
          indicator == "maternal_rate" ~ after_delivery_value / 100000,
          indicator == "neonatal_rate" ~ after_delivery_value / 1000,
          indicator == "u5_rate" ~ after_delivery_value / 1000,
          indicator == "stillbirth_rate" ~ after_delivery_value / 1000,
          indicator_category != "mortality_rates" ~ after_delivery_value / 100
        )
      )
    
    # calculate benefits using LiST outputs.
    # neonates estimation: 1) Estimate number of children of 0 age (children under
    # five / 5); 2) Estimate number of children at one day of age (children age 0 /
    # 365); 3) Estimate number of children within their first 28 days of life
    # (children 1 day of life * 28)
    coi_benefits <- trimmed_coverage_costs %>%
      group_by(emergency, country, target_group) %>%
      summarise(total_pin = unique(total_pin)) %>%
      filter(!is.na(total_pin)) %>%
      left_join(
        benef_indicators,
        by = c("country", "emergency", "target_group" = "related_target_group")
      ) %>%
      mutate(
        absolute_values = case_when(
          indicator == "maternal_rate" ~ final_reduced_rates * total_pin,
          indicator == "u5_rate" ~ final_reduced_rates * total_pin,
          indicator == "neonatal_rate" ~ final_reduced_rates * (((total_pin / 5) / 365) * 28), # neonates estimation
          indicator == "stillbirth_rate" ~ final_reduced_rates * total_pin,
          indicator == "stunting_rate" ~ final_reduced_rates * total_pin,
          indicator == "wasting_rate" ~ final_reduced_rates * total_pin,
          indicator == "exclusive_breastfeeding_rate" ~ final_reduced_rates * (total_pin / 5),
          indicator == "breastfeeding_initiation_rate" ~ final_reduced_rates * (((total_pin / 5) / 365) * 28), # neonates estimation
          indicator == "pregnant_anemia" ~ final_reduced_rates * total_pin,
          indicator == "pregnant_iron_def" ~ final_reduced_rates * total_pin,
          indicator == "women_anemia" ~ final_reduced_rates * total_pin,
          indicator == "women_iron_def" ~ final_reduced_rates * total_pin
        )
      )
    
    # wrangle benefit table for presentation
    # coverages to columns, and cases avoided respective to 0 estimated.
    coi_avoided <- coi_benefits %>%
      group_by(emergency,
               coverage_type,
               indicator_category,
               indicator_name_absolute) %>%
      summarise(absolute_values = sum(absolute_values, na.rm = TRUE)) %>%
      pivot_wider(
        names_from = coverage_type,
        names_prefix = "coverage_",
        values_from = absolute_values
      ) %>%
      mutate(
        saved_implemented = coverage_0 - coverage_implemented,
        saved_30 = coverage_0 - coverage_30,
        saved_95 = coverage_0 - coverage_95
      ) %>%
      select(-coverage_30,-coverage_95,-coverage_implemented)
    
    # separate main tables in two according to avoided value sign (mm:
    # malnutrition and mortality; bf: breastfeeding)
    coi_avoided_mm <-  coi_avoided %>%
      filter(indicator_category != "breastfeeding")
    
    coi_avoided_bf <- coi_avoided %>%
      filter(indicator_category == "breastfeeding")
    
    # return list with result dfs
    dir_benefits <- list(
      malnutrition = coi_avoided_mm,
      breastfeeding = coi_avoided_bf
    )
    
    return(dir_benefits)
  }
