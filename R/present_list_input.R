# function transforms df into a gt object which is a table whose purpose is to
# present data to a human.
present_list_input <- function(list_input){
  list_input %>% 
    # prepare data for gt package
    ungroup() %>% 
    # create gt object
    gt() %>% 
    tab_header(title = "Input coverages used in LiST") %>% 
    cols_hide(intervention_id) %>% 
    cols_label( # rename colums
      emergency = "Emergency",
      country = "Country",
      target_group = "Target group",
      intervention_name = "Intervention name",
      coverage = "Coverage",
      list_name = "Intervention name in LiST"
    ) %>% 
    fmt_number( # format column
      columns = coverage,
      decimals = 2
    ) %>% 
    return()
}


  
