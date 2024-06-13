# read intervention list and make intervention_id col factor type
read_file <- function(file) {
  if (file == "raw_data/intervention_list.csv") {
    read_csv(file,
             col_types = list(intervention_id = readr::col_factor())) %>%
      return()
    
  } else if (file == "raw_data/list_output.csv") {
    read_csv(file, skip = 1)
      
    
  } else {
    read_csv(file) %>%
      return()
  }
}
