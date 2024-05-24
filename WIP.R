tar_load(c("list_output", "list_metadata", "coverage_costs"))


# trim exess delivered (over 100% delivery) to ideal_delivered
coverage_costs <- coverage_costs %>% 
  mutate(trimmed_beneficiaries = if_else(total_beneficiaries > ideal_delivered,
                                         ideal_delivered,
                                         total_beneficiaries
                                         ))

# costs
# should summarize table to drop country and summarize by emergency as in
# current table in report
coi_costs <- coverage_costs %>% 
  group_by(emergency, target_group, intervention_id, intervention_name) %>% 
  summarise()

# benefits
# should calculate benefits using LiST outputs. See line 1006
coi_benefits <- coverage_costs %>% 
  mutate(implemented_)


