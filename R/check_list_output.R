# function aids checking the list_output df by producing plots comparing the
# different indicator values. The main objective is to identify typing mistakes
# while transfering data from the spectrum software to the receiving
# spreadsheet. Outputs plots corresponding to the different emergencies and
# countries assessed.
check_list_output <- function(list_output) {
  # wrangle data before plotting
  list_output <- list_output %>%
    mutate(emergency = str_replace(emergency, "\\s|-", "_")) %>%
    mutate(model_name = paste0(country, "_", emergency)) %>%
    pivot_longer(
      cols = c("baseline_value", "after_delivery_value"),
      names_to = "value_timing",
      values_to = "values"
    )
  
  # generalize plotting through function
  plot_output <- function(list_output, model_name) {
    list_output %>%
      filter(model_name == {{model_name}}) %>%
      ggplot(aes(x = indicator, y = values, fill = value_timing)) +
      geom_col(position = "dodge") +
      facet_wrap(~ coverage_type) +
      scale_y_continuous(trans = 'log2') +
      coord_flip() +
      labs(title = {{model_name}})
  }
  
  # plot based on model names
  list_output_plots <-
    map2(list(list_output),
         unique(list_output$model_name),
         plot_output)
  
  # return list
  return(list_output_plots)
}





