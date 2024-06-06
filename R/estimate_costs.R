# from trimmed_cost_coverage, summarizes by emergency and target group and
# calculate total and ideal costs of delivering interventions. Returns a summary
# table.
estimate_costs <- function(trimmed_coverage_costs){
  trimmed_coverage_costs %>% 
    group_by(emergency, target_group) %>% 
    summarise(total_pin = sum(total_pin, na.rm = TRUE),
              total_beneficiaries = sum(trimmed_beneficiaries, na.rm = TRUE),
              ideal_delivered = sum(ideal_delivered, na.rm = TRUE),
              total_cost = sum(total_cost, na.rm = TRUE),
              ideal_cost = sum(ideal_cost, na.rm = TRUE)) %>% 
    mutate(funding_gap = ideal_cost - total_cost) %>% 
    mutate(global_coverage = (total_beneficiaries / ideal_delivered) * 100) %>% 
    return()
}
