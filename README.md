# The cost of inaction on nutrition in emergencies in Latin America and the Caribbean

This repository supports the study *“The Cost of Inaction on Nutrition in Emergencies in Latin America and the Caribbean (LAC)”*, implemented by **movimentar GmbH** in collaboration with **UNICEF**.

The LAC region faces a high risk of natural disasters, structural poverty, inequality, and widespread violence. These factors place vulnerable populations—particularly children under five and pregnant and lactating women (PLW)—at heightened risk of malnutrition, infectious disease, and long-term developmental challenges.

Migration flows, particularly from Venezuela and Central America, have added complexity to humanitarian needs. Despite the urgency, nutrition-specific interventions often receive limited funding in emergencies. This study quantifies the human and economic consequences of failing to adequately fund nutrition responses during selected emergencies in the region.

For project documentation, see the [project workspace](https://movimentar.eu.teamwork.com/app/projects/536205/).

The web application Cost of Inaction Explorer - Nutrition in Emergencies is available at [https://movimentar.shinyapps.io/nutrivalue/](https://movimentar.shinyapps.io/nutrivalue/).

---

## Methodological framework

The analysis applies the **Cost of Inaction (CoI)** framework developed by *Anand et al. (2012)*. The study estimates both **direct benefits** (e.g. lives saved, morbidity avoided) and **indirect benefits** (e.g. cognitive gains, economic productivity) that would be realised through full implementation of nutrition-specific interventions.

Key methodological tools and assumptions include:

* **Lives Saved Tool (LiST)**: Used to estimate changes in mortality and health outcomes.
* **Cost of Not Breastfeeding Tool**: Used to estimate indirect economic benefits of exclusive breastfeeding.
* **Median costing**: Due to gaps in intervention-level cost reporting, we applied medians across countries for each intervention.
* **Proxy baselines for migrants**: Due to data limitations, national Venezuelan baseline indicators were used to model migrant populations.

---

## Reproducible analysis pipeline

This repository is built using the [`targets`](https://books.ropenspec.org/targets/) R package to ensure a structured, reproducible data pipeline. This system allows researchers to rerun, trace, and validate each step of the analysis.

### The Data Pipeline Visualised

The following diagram illustrates the flow of data from raw sources to final report outputs. This visual provides a high-level overview of the entire analytical process, showing how raw data is cleaned, modelled, and aggregated to produce key findings and tables. The purple block highlights the **modelling stage**, which uses the Lives Saved Tool (LiST).



<div align="center">   
 <img src="images/data_pipeline.png" alt="Data Pipeline" width="30%">
</div>

### System requirements

* **R version**: 4.2.3 (2023-03-15 ucrt)
* **Primary script**: `_targets.R`
* **Main output**: `coi_df` — a data frame with intervention coverage and cost estimates

### Required packages

All required packages are listed in `tar_option_set()` in `_targets.R`. One package must be installed manually:

```r
remotes::install_github("PPgp/wpp2022")
````

-----

## How to run the analysis

### 1\. Load the project

Open the R Project file `unicef-lac-coi-nutrition.Rproj` in RStudio. This sets the working directory and environment.

### 2\. Visualise the pipeline (optional)

To explore the structure of the analysis visually:

```r
targets::tar_visnetwork()
```

To zoom in on the output path for the main dataset (`coi_df`):

```r
targets::tar_visnetwork(names = "coi_df")
```

This diagram can be useful for explaining the workflow or reviewing dependencies between processing steps. It is not required to run the pipeline.

### 3\. Run the full workflow

This executes all data loading, wrangling, aggregation, and result generation:

```r
targets::tar_make()
```

### 4\. Access outputs

To retrieve a result into your environment (e.g. the main data frame):

```r
targets::tar_load("coi_df")
```

Other intermediate objects are defined as targets and can be loaded similarly. These include cleaned data sets, cost tables, mortality estimates, and more.

-----

## Repository structure

| Folder/File                      | Description                                                    |
| -------------------------------- | -------------------------------------------------------------- |
| `_targets.R`                     | Main pipeline script defining target structure                 |
| `R/`                             | Custom R functions used in the pipeline                        |
| `raw_data/`                      | Raw input data files (see below)                               |
| `coI_results.qmd`                | Quarto file generating tables and figures for the final report |
| `col_results_files/`             | Folder containing rendered tables and HTML output              |
| `tar_visnetwork.html`            | Snapshot of the workflow visualisation                         |
| `README.md`                      | This file                                                      |
| `unicef-lac-coi-nutrition.Rproj` | RStudio project file                                           |

-----

### Raw data files

All data used in the analysis is stored in the `raw_data/` folder. Key files include:

  * `co_cost_beneficiary_data.csv`: Reported beneficiaries and intervention costs
  * `pin_data.csv`: People in Need (PiN) estimates
  * `emergency_list.csv`: Metadata on studied emergencies
  * `intervention_list.csv`: Descriptions of all modelled interventions
  * `formula_price.csv`: Unit cost of infant formula
  * `mean_earnings.csv`: National income data for economic modelling
  * `income_share.csv`, `gni_forecast.csv`: Data for productivity loss estimates
  * `list_indicator_metadata.csv`, `list_output.csv`: LiST-generated mortality and morbidity outputs
  * `funding_data_2.csv`: Humanitarian nutrition funding data (reported vs. needed)

These inputs are read and validated during pipeline execution. Any changes to the data must be reflected by re-running `targets::tar_make()`.

-----

## Report outputs

After running the pipeline, report outputs can be found in:

  * `col_results.qmd`: Source code for tables and summaries
  * `col_results.docx`, `col_results.html`, `Col_results_summary.xlsx`: Exported result files

-----

## Notes for non-technical users

The **`targets`** pipeline ensures that all analysis steps are transparent, reproducible, and modular. It automatically updates only what needs to be re-run when data or code changes, saving time and reducing error.

  * Think of each step as a building block. A *target* is a named object like a table or result.
  * The final results in the Word and Excel files are built from the target `coi_df`, which combines all calculations.
  * You can explore or edit results using `tar_load()` without manually running every step again.

-----

If you have questions or need technical support, please contact [support@movimentar.eu](mailto:support@movimentar.eu).

