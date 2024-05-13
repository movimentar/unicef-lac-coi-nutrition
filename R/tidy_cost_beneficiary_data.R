
tidy_cost_beneficiary_data <- function(file) {
  #read csv file
  raw_cost_beneficiary_data <-
    read_csv(file)
  
  #pivot table to long format
  cost_beneficiary_long <- raw_cost_beneficiary_data %>%
    pivot_longer(c(4:42), names_to = "headers")
  #split cost data
  co_costs <- cost_beneficiary_long %>%
    filter(str_detect(headers, "year")) %>%
    mutate(year = str_extract(headers, "202.")) %>%
    mutate(cost_type = str_extract(headers, "[^_]*$")) %>%
    filter(cost_type != "total") %>%
    select(id,
           emergency_id,
           intervention_id,
           year,
           cost_type,
           value,
           comments)
  
  # split beneficiary data
  co_benef <-  cost_beneficiary_long %>%
    filter(str_detect(headers, "beneficiaries")) %>%
    mutate(year = str_extract(headers, "202.")) %>%
    mutate(benef_category = str_extract(headers, "people_.*d")) %>%
    mutate(benef_gender = str_extract(headers, "[^_]*$")) %>%
    select(
      id,
      emergency_id,
      intervention_id,
      year,
      benef_category,
      benef_gender,
      value,
      comments
    )
  
  # package cost and beneficiary data in a list
  cost_beneficiary_data <-
    list(costs = co_costs, beneficiaries = co_benef)
  
  return(cost_beneficiary_data)
}
