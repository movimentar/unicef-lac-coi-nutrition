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
coi_benefits <- coverage_costs %>% 
  group_by(emergency, country, target_group) %>% 
  summarise(total_pin = unique(total_pin)) %>% 
  filter(!is.na(total_pin)) %>% 
  left_join(benef_indicators, by = c("country", "emergency", "target_group" = "related_target_group"))
  

