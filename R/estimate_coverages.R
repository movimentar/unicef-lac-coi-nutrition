# from trimmed_coverage_costs, summarizes by emergency and intervention and
# estimates coverages.
  estimate_coverages <- function(trimmed_coverage_costs){
    trimmed_coverage_costs %>% 
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
      ) %>% 
      return()
  }
