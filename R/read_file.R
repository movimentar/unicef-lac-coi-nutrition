# read intervention list and make intervention_id col factor type
read_file <- function(file) {
  if (file == "raw_data/intervention_list.csv") {
    read_csv(file,
             col_types = list(intervention_id = readr::col_factor())) %>%
      return()
  } else{
    read_csv(file) %>%
      return()
  }
}
