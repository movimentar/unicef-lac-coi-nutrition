tar_load(c("list_output", "list_metadata", "coverage_costs", "list_metadata", "list_output"))


# trim exess delivered (over 100% delivery) to ideal_delivered
coverage_costs <- coverage_costs %>% 
  mutate(trimmed_beneficiaries = if_else(total_beneficiaries > ideal_delivered,
                                         ideal_delivered,
                                         total_beneficiaries
                                         ))

# coverages
coi_coverages <- coverage_costs %>% 
  group_by(emergency, intervention_name) %>% 
  summarise(total_beneficiaries = sum(trimmed_beneficiaries, na.rm = TRUE),
            ideal_delivered = sum(ideal_delivered, na.rm = TRUE)) %>% 
  mutate(coverage = total_beneficiaries / ideal_delivered) %>% 
  pivot_wider(names_from = emergency,
              values_from = c("total_beneficiaries", "ideal_delivered", "coverage")) %>% 
  select(intervention_name,
         `total_beneficiaries_Eta-Iota`,
         `ideal_delivered_Eta-Iota`,
         `coverage_Eta-Iota`,
         `total_beneficiaries_Migration flows`,
         `ideal_delivered_Migration flows`,
         `coverage_Migration flows`
         )

# costs
# should summarize table to drop country and summarize by emergency as in
# current table in report
coi_costs <- coverage_costs %>% 
  group_by(emergency, target_group) %>% 
  summarise(total_pin = sum(total_pin, na.rm = TRUE),
            total_beneficiaries = sum(trimmed_beneficiaries, na.rm = TRUE),
            ideal_delivered = sum(ideal_delivered, na.rm = TRUE),
            total_cost = sum(total_cost, na.rm = TRUE),
            ideal_cost = sum(ideal_cost, na.rm = TRUE)) %>% 
  mutate(funding_gap = ideal_cost - total_cost) %>% 
  mutate(global_coverage = (total_beneficiaries / ideal_delivered) * 100)

# benefits

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
    ))) %>% 
  left_join(list_metadata, by = c("indicator", "indicator_category", "indicator_name")) %>% 
  mutate(final_reduced_rates = case_when(
    indicator == "maternal_rate" ~ after_delivery_value / 100000,
    indicator == "neonatal_rate" ~ after_delivery_value / 1000,
    indicator == "u5_rate" ~ after_delivery_value / 1000,
    indicator == "stillbirth_rate" ~ after_delivery_value / 1000,
    indicator_category != "mortality_rates" ~ after_delivery_value / 100
  ))

# calculate benefits using LiST outputs.
# neonates estimation: 1) Estimate number of children of 0 age (children under
# five / 5); 2) Estimate number of children at one day of age (children age 0 /
# 365); 3) Estimate number of children within their first 28 days of life
# (children 1 day of life * 28)
coi_benefits <- coverage_costs %>% 
  group_by(emergency, country, target_group) %>% 
  summarise(total_pin = unique(total_pin)) %>% 
  filter(!is.na(total_pin)) %>% 
  left_join(benef_indicators, by = c("country", "emergency", "target_group" = "related_target_group")) %>% 
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
    ))

# wrangle benefit table for presentation
# coverages to columns, and cases avoided respective to 0 estimated.
coi_avoided <- coi_benefits %>% 
  group_by(emergency,
           coverage_type,
           indicator_category,
           indicator_name_absolute) %>%
  summarise(absolute_values = sum(absolute_values, na.rm = TRUE)) %>% 
  pivot_wider(names_from = coverage_type,
              names_prefix = "coverage_",
              values_from = absolute_values) %>% 
  mutate(saved_implemented = coverage_0 - coverage_implemented,
         saved_30 = coverage_0 - coverage_30,
         saved_95 = coverage_0 - coverage_95) %>% 
  select(-coverage_30, -coverage_95, -coverage_implemented)

# separate main tables in two according to avoided value sign
coi_avoided_mm <-  coi_avoided %>% 
  filter(indicator_category != "breastfeeding")

coi_avoided_bf <- coi_avoided %>% 
  filter(indicator_category == "breastfeeding")
  
  
