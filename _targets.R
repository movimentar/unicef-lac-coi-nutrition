# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(packages = c(
  "tidyverse",
  "scales",
  "wpp2022",
  "gt",
  "FactoMineR",
  "factoextra"
) # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    cost_beneficiary_file,
    "raw_data/co_cost_beneficiary_data.csv",
    format = "file"
  ),
  tar_target(
    cost_beneficiary_data,
    tidy_cost_beneficiary_data(cost_beneficiary_file)
  ),
  tar_target(
    intervention_list_file,
    "raw_data/intervention_list.csv",
    format = "file"
  ),
  tar_target(emergency_list_file,
             "raw_data/emergency_list.csv",
             format = "file"),
  tar_target(intervention_list,
             read_file(intervention_list_file)),
  tar_target(emergency_list,
             read_file(emergency_list_file)),
  tar_target(
    cost_beneficiary_df,
    build_cost_beneficiary(cost_beneficiary_data, intervention_list, emergency_list)),
  tar_target(
    exploration_cost_beneficiary,
    explore_cost_beneficiary(cost_beneficiary_df)
  ),
  tar_target(
    cost_beneficiary_flagged,
    flag_cost_beneficiary(cost_beneficiary_df)
  ),
  tar_target(
    cost_beneficiary_clean,
    clean_cost_beneficiary(cost_beneficiary_flagged)
  ),
  tar_target(
    exploration_cleaned,
    explore_cost_beneficiary(cost_beneficiary_clean)
  ),
  tar_target(
    median_costs,
    calculate_cost_medians(cost_beneficiary_clean)
  ),
  tar_target(median_costs_cleaned,
             clean_median_costs(median_costs)),
  tar_target(
    pin_percentages,
    calculate_pin_percentages(intervention_list, emergency_list)
  ),
  tar_target(
    pin_data_file, 
    "raw_data/pin_data.csv",
    format = "file"
  ),
  tar_target(
    pin_data,
    read_file(pin_data_file)
  ),
  tar_target(
    pin_data_clean,
    clean_pin_data(pin_data)
  ),
  tar_target(
    coi_df,
    build_coi(cost_beneficiary_clean,
              pin_percentages,
              pin_data_clean,
              intervention_list,
              median_costs_cleaned)
  ),
  tar_target(curative_rates,
             calculate_curative_rates(coi_df)
  ),
  tar_target(coverage_costs,
             calc_coverage_costs(coi_df, curative_rates)
             ),
  tar_target(list_input, 
             build_list_input(coverage_costs, intervention_list)
             ),
  tar_target(list_input_gt,
             present_list_input(list_input)
             ),
  tar_target(list_output_file,
             "raw_data/list_output_outdated.csv",
             format = "file"
             ),
  tar_target(list_indicator_file,
             "raw_data/list_indicator_metadata.csv",
             format = "file"),
  tar_target(list_output,
             read_file(list_output_file)
             ),
  tar_target(list_metadata,
             read_file(list_indicator_file)
  ),
  tar_target(trimmed_coverage_costs,
             trim_beneficiaries(coverage_costs)
             ),
  tar_target(coi_coverages,
             estimate_coverages(trimmed_coverage_costs)
             ),
  tar_target(coi_costs,
             estimate_costs(trimmed_coverage_costs)
             ),
  tar_target(coi_dir_benefits,
             estimate_dir_benefits(trimmed_coverage_costs,
                                   list_output,
                                   list_metadata)
             ),
  tar_target(coi_pca,
             run_pca(coverage_costs, emergency_list)
             ),
  tar_target(list_output_exploration,
             check_list_output(list_output)),
  tar_target(formula_price_file,
             "raw_data/formula_price.csv",
             format = "file"),
  tar_target(gni_forecast_file,
             "raw_data/gni_forecast.csv",
             format = "file"),
  tar_target(income_share_file,
             "raw_data/income_share.csv",
             format = "file"),
  tar_target(mean_earnings_file,
             "raw_data/mean_earnings.csv",
             format = "file"),
  tar_target(formula_price,
             read_file(formula_price_file)
             ),
  tar_target(gni_forecast,
             read_file(gni_forecast_file)
             ),
  tar_target(income_share,
             read_file(income_share_file)
             ),
  tar_target(mean_earnings,
             read_file(mean_earnings_file)
             ),
  tar_target(formula_packages,
             calculate_formula_packages()),
  tar_target(coi_indir_benefits,
             calculate_indir_benefits(coi_dir_benefits,
                                      mean_earnings,
                                      income_share,
                                      gni_forecast,
                                      formula_price,
                                      formula_packages)
             )
)
