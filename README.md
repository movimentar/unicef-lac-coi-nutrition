# Study – The cost of inaction on nutrition in emergencies in Latin America and the Caribbean

This repository supports the study *“The Cost of Inaction on Nutrition in Emergencies in Latin America and the Caribbean (LAC)”*, implemented by movimentar GmbH in collaboration with UNICEF.

The LAC region faces high exposure to natural disasters, compounded by structural poverty, inequality, and pervasive violence. These challenges affect millions, particularly children under five and pregnant and lactating women (PLW), increasing the risk of malnutrition, infectious diseases, and long-term developmental impairments.

The region also experiences substantial migration flows, with a high per capita number of migrants, refugees, and displaced persons. Despite the urgency of addressing nutrition in such emergencies, global humanitarian funding tends to prioritise other sectors, often overlooking the specific nutritional needs of vulnerable populations.

UNICEF advocates for prioritising, funding, and implementing nutrition-specific interventions to prevent severe outcomes during crises. This study aims to assess the human and economic consequences of failing to fund and implement adequate nutrition responses in selected emergencies across the LAC region, with a focus on children under five—particularly those under two—and PLW.

For more background and project documentation, visit the [project workspace](https://movimentar.eu.teamwork.com/app/projects/536205/).

---

## Reproducibility and pipeline structure

This project uses the [`targets`](https://books.ropensci.org/targets/) package to define a structured and reproducible pipeline in R.

* **R version**: 4.2.3 (2023-03-15 ucrt) – "Shortstop Beagle"
* **Main script**: `_targets.R` at the project root
* **Main output**: `coi_df`, a data frame containing intervention coverage estimates and associated cost calculations

To run the pipeline:

1. **Install dependencies**
   Packages required are listed in the `tar_option_set()` call in `_targets.R`.
   One package, [`wpp2022`](https://github.com/PPgp/wpp2022), must be [installed from GitHub](https://cran.r-project.org/web/packages/githubinstall/vignettes/githubinstall.html).

2. **Run the pipeline**
   Follow sections [2.4](https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline) and [2.5](https://books.ropensci.org/targets/walkthrough.html#run-the-pipeline) of the `targets` manual to:

   * Inspect the pipeline with `tar_visnetwork()`
   * Execute the full workflow using `tar_make()`

3. **Review and inspect results**
   Each **target** represents an intermediate object (e.g. data frame, list, value, plot). Use `tar_load("target_name")` to load a specific object into your environment. The `coi_df` target is the core output; targets upstream of it prepare and clean data, while downstream targets generate tables and summaries.

4. **LiST model integration**
   A portion of the analysis using the Lives Saved Tool (LiST) is run externally and integrated into the pipeline manually. 





