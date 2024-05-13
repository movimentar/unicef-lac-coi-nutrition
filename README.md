# Study - The cost of inaction on nutrition in emergencies in Latin America and the Caribbean
Repository in support of the Study - The cost of inaction on nutrition in emergencies in Latin America and the Caribbean

The Latin America and the Caribbean (LAC) region is highly susceptible to natural disasters, due to structural poverty, inequality, and pervasive violence. This affects millions of vulnerable people every year, especially the nutritional status of children under five years of age and pregnant and lactating women (PLW), with humanitarian needs. Additionally, the region experiences significant migration flows, making LAC a region with a high per capita number of migrants, including refugees and displaced persons. Disasters and humanitarian crises severely impact nutritional status, especially among children under five and PLW, leading to increased risks of infectious diseases, malnutrition, and long-term cognitive and physical impairments. Despite the critical importance of nutrition responses in emergencies, global funding often prioritises other sectors over direct nutritional interventions, overlooking the nuanced needs of the population groups most vulnerable to malnutrition  and the long-term benefits of nutritional support. UNICEF emphasises the importance of prioritising, funding and implementing nutrition interventions in response to an emergency to prevent malnutrition and its severe outcomes, highlighting the need for increased political and financial support for nutrition-specific interventions to address the challenges faced by the most vulnerable in LAC. The primary objective of this multi-country study was to assess the human and economic implications of failing to fund and therefore implement a nutrition-focused response in specific emergencies in the Latin American and Caribbean (LAC) region targeting children under five years old, with an additional emphasis on those under two, as well as pregnant and lactating women.

See more at: https://movimentar.eu.teamwork.com/app/projects/536205/

## Instructions for running the scripts
The projects uses the package "targets" to define a reproducible and organized pipeline, for more info on its use please consult [their user manual](https://books.ropensci.org/targets/).

The current pipeline has been built and run with R version 4.2.3 (2023-03-15 ucrt) -- "Shortstop Beagle"

The pipeline is run from the _targets.R file located on the root of the project. To run the pipeline, first install the packages indicated in the tar_option_set() function in the _targets.R file, from those packages, ["wpp2022"](https://github.com/PPgp/wpp2022) needs to be [installed from github](https://cran.r-project.org/web/packages/githubinstall/vignettes/githubinstall.html).

After the necessary packages have been installed, you can check the pipeline and then run it following the instructions in the [sections 2.4 and 2.5 of the user manual](https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline).

A target is equivalent to an intermediate step in the pipeline, these can be data frames, lists, individual values, plots or any other R object. To retrieve them and inspect them, check the target names with the tar_visnetwork() function and load the target you want to check to the environment using the tar_load() function.

The main target in the project is the "coi_df" target which contains a data frame with the necesary information for calculating nutrition intervention coverages and estimated monetary costs of reaching the people in need indicated by the data collection. All targets preceding the coi_df can be considered data cleaning and calibration steps, while all steps following the same target can be considered steps to obtain output tables and results.

There is a portion of the pipeline corresponding to the LiST model that has to be run manually. This paragrgraph will be updated as that section is finished.








