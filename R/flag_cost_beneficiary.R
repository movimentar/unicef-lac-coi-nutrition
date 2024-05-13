
flag_cost_beneficiary <- function(df){
  
    # function identifies values that mean no data for us.
  contains_no_data <- function(value){
    is.na(value) | is.nan(value) | is.infinite(value) | value == 0
  }
  
  # function flags a specific variable from the df by creating a new logical
  # column. TRUE means that the value should be replaced by the overall median.
  flag_var <- function(df, var) {
    
    # function calculates intervention_id IQRs per emergency
    obtain_IQR_values <- function(df, var){
      df %>% 
        filter({{var}} != 0) %>% 
        filter(!(is.na({{var}}) | is.nan({{var}}) | is.infinite({{var}}))) %>% 
        group_by(emergency, intervention_id) %>% 
        summarise(IQR := IQR({{var}}),
                  median := median({{var}})) %>% 
        return()
    }
    
    # function identifies values higher than their respective IQR
    surpasses_IQR <- function(value, IQR, median){
      if_else(
        value > IQR + median,
        TRUE,
        FALSE
      ) %>% 
        return()
    }
    
    # flag values to be replaced in new column
    df %>%
      left_join(obtain_IQR_values(df, {{var}}),
                by = (c("emergency", "intervention_id"))) %>%
      mutate("{{var}}_replace_flag" := if_else(
        contains_no_data({{var}}) |
          surpasses_IQR({{var}}, IQR, median),
        TRUE,
        FALSE
      )) %>% 
      select(-IQR, -median) %>% 
      return()
  }
  
  # function flags observations based on the content of total_cost and
  # total_beneficiaries. If both contain no data (NAs or 0), the observation gets
  # flagged with TRUE.
  flag_no_data <- function(df){
    
    df %>% 
      mutate(no_data_flag = if_else(
        contains_no_data(total_cost) & contains_no_data(total_beneficiaries),
        TRUE,
        FALSE
      )) %>% 
      return()
  } 
  
  
  flagged_cost_benef <- df %>% 
    flag_var(total_cost) %>% 
    flag_var(total_beneficiaries) %>% 
    flag_var(imp_cost_person) %>% 
    flag_no_data() %>% 
    return()
}
