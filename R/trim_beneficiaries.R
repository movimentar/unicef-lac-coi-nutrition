# from the coverage_costs df, trims the total beneficiaries col to not exceed
# the ideal_delivered values. A new column (trimmed beneficiaries) is creates
# with the generated data.
trim_beneficiaries <- function(coverage_costs) {
  coverage_costs %>%
    mutate(
      trimmed_beneficiaries = if_else(
        total_beneficiaries > ideal_delivered,
        ideal_delivered,
        total_beneficiaries
      )
    ) %>% 
    return()
}
