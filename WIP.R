tar_load(c("list_output", "list_metadata", "coverage_costs"))


# trim exess delivered (over 100% delivery) to ideal_delivered
coverage_costs <- coverage_costs %>% 
  mutate(trimmed_beneficiaries = if_else(total_beneficiaries > ideal_delivered,
                                         ideal_delivered,
                                         total_beneficiaries
                                         ))

# coverages
coi_coverages <- coverage_costs %>% 
  group_by(emergency, intervention_name) %>% 
  summarise(total_beneficiaries = sum(total_beneficiaries, na.rm = TRUE),
            ideal_delivered = sum(ideal_delivered, na.rm = TRUE)) %>% 
  mutate(coverage = total_beneficiaries / ideal_delivered) %>% 
  pivot_wider(names_from = emergency,
              values_from = c("total_beneficiaries", "ideal_delivered", "coverage"))

# costs
# should summarize table to drop country and summarize by emergency as in
# current table in report
coi_costs <- coverage_costs %>% 
  group_by(emergency, intervention_name) %>% 
  summarise(total_beneficiaries = sum(total_beneficiaries, na.rm = TRUE),
            ideal_delivered = sum(ideal_delivered, na.rm = TRUE)) %>% 
  mutate(coverage = total_beneficiaries / ideal_delivered)

# benefits
# should calculate benefits using LiST outputs. See line 1006
coi_benefits <- coverage_costs %>% 
  mutate(implemented_)


