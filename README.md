# ripa2tan_epi

This repository contains the data and code for the work "Epidemiological landscape of communicable and non-communicable diseases in urban and rural districts in the Kilimanjaro region and their potential environmental drivers".

## Project structure

```bash
.
├── data
│   ├── netra_figs_data
│   ├── processed
│   └── raw
├── results
│   ├── plots
│   ├── R_output
│   └── tables
└── scripts
    ├── 00_source.R
    ├── 01_data-processing.R
    ├── 02_initial-description.R
    ├── 03_data-post-processing-and-imputation.R
    ├── 04_final-description.R
    ├── 05_descriptive-modeling.R
    ├── 06_association-modeling_fit-models.R
    ├── 07_association-modeling_generated-quantities.R
    ├── 08_make-figures-and-tables.R
    ├── figure1_b.R
    ├── figure1.R
    ├── renv
    └── stan_models
        ├── association_exposure-disease-incidence.stan
        ├── descriptive_time-series_choose-likelihood_imputation.stan
        └── descriptive_time-series_choose-likelihood.stan
```

## Installation

All analyses were performed in R version 4.4.1.

This project relies on the `renv` package to manage package dependencies. The list of packages and their versions is recorded in the `renv.lock` file inside the `scripts` folder. Opening an R session in the `scripts` folder will automatically initialize the `renv` virtual environment. You can install the specific package versions with the command:

```bash
R -e "renv::restore()"
```

## Scripts

### 00_source.R

This script is loaded by every other script. It loads basic R packages, sets ggplot2 options, sets path variables, sets environment variables relevant to the analysis (subsets of exposures and diseases) and defines different utility functions.

### 01_data-processing.R

It processes the raw data of population, environment and diseases held in `/data/raw`.

It creates an initial version of the dataset in `/data/processed/initial`, composed by files:
- `population.csv`
- `environmental.csv`
- `disease.csv`

Data sources:
- Population:
    - `population/pop_millions.xlsx`
- Environment:
    - `environmental/main/avgpm25_monthly.csv`
    - `environmental/siha_correction/avg_pm25_monthly_siha_np.csv`; Siha PM2.5 data computed without considering National Park area.
    - `environmental/main/2012_pm2_5.csv`
    - `environmental/main/2013_pm2_5.csv`
    - `environmental/main/monthwise_greenness.csv`
    - `environmental/main/rc_month_rain_satellite.csv`
    - `environmental/temperature_correction/environmental_v2.xlsx`; temperature values provided by collaborators
    - `environmental/main/moshi_utci_2012_oct_2022.csv`
    - `environmental/main/siha_utci_2012_oct_2022.csv`
- Disease:
    - `disease_v2/Moshi_monthlydata_cleaned_june_2024.csv`
    - `disease_v2/Siha_monthlydata_cleaned_june_2024_august_updated.csv`

### 02_initial-description.R

It creates descriptive plots of the initial dataset in `/results/plots/initial-description`.

Creates visualizations of different classifications of the diseases, and saves the final disease groups in `/data/processed/initial/disease-grouping.csv`.

<!-- The plots are:
- `population.pdf`; population from 2012 to 2021
- `environmental.pdf`; environmental time series, all available data (2012 to 2022)
- `environmental_missing.pdf`;  environmental missingness pattern
- `disease_missing.pdf`; disease missingness pattern
- `disease_original-groups.pdf`; disease cases heatmap, with original proposed groups
- `disease_original-groups_minus-missings.pdf`; same plot, removing diseases with many missings
- `disease_skevaki-groups.pdf`; disease cases heatmap, proposed grouping 1
- `disease_renz-groups.pdf`; disease cases heatmap, proposed grouping 2
- `disease_final-groups.pdf`; disease cases heatmap, final decided disease groups
- `disease.pdf`; time series of number of cases for all diseases (2014 to 2022) -->

### 03_data-post-processing-and-imputation.R

It creates the final dataset for the study period (2015 to 2021) and saves it in `/data/processed/final`. The resulting files are:
- `population_final.csv`
- `environmental_final.csv`
- `disease_final.csv`

Population data is kept as is, disease data is post-processed and the environmental data of 4 variables is imputed.

In more detail:

#### Population

For visualization purposes, the population of 2022 is imputed with a linear prediction and saved in `/data/processed/interim/population_2022-imputed.csv`.

#### Disease post-processing

Only diseases without missing observations during the study period are kept for the analyses.

The following diseases are removed:
- Other vector-borne diseases (yellow fever, viral hemorrhagic fevers, trypanosomiasis, relapsing fever \[louse-borne typhus])
- Meningitis
- Acute flaccid paralysis
- Cholera
- Rabies
- Measles
- Influenza

"Gynecological diseases" are removed due to unclear definition.

The diseases "diarrhea" and "malnutrition" are created by pooling the cases of different diseases. These are:
- Diarrhea:
	- Diarrhea with severe dehydration
	- Diarrhea with no dehydration
- Malnutrition:
	- Other nutritional disorders,
	- Moderate malnutrition
	- Marasmus
	- Marasmic kwashiorkor
	- Kwashiorkor

The 46 initial diseases are trimmed down to 30 diseases.

The post-processed disease dataset (all time, 2014 to 2022) is saved in `/data/processed/interim/disease_final_complete.csv`.

#### Environmental data imputation

Variables total_rainfall and n_raindays were missing for Siha in 2021.
Variables utci and greenness had a few missing observations from 2015 to 2021 (2 for utci, 11 for greenness).

These were imputed with the median predictions from the model **`descriptive_time-series_choose-likelihood_imputation.stan`**.

Variables utci and greenness were modeled with a Normal likelihood, total_rainfall with a Gamma likelihood, and n_raindays with a Negative Binomial likelihood.

The imputations can be visualized in `/results/plots/final-description/environmental_imputed_2015-2021.pdf`

### 04_final-description.R

It creates descriptive plots of the final dataset in `/results/plots/final-description`.

<!-- The plots are:
- `population_2015-2021.pdf`
- `environmental_2015-2021.pdf`
- `disease_final-groups_post-processing_all-time.pdf`; incidence rate (per 100,000 people) heatmap, 2014 to 2022
- `disease_incidence-rate-1e5_all-time.pdf`; incidence rate time series, 2014 to 2022
- `disease_final-groups_post-processing_2015-2021.pdf`
- `disease_incidence-rate-1e5_2015-2021.pdf` -->

### 05_descriptive-modeling.R

It performs the trend analyses for the environmental and disease time series.

For each time series, the model **`descriptive_time-series_choose-likelihood.stan`** is fitted and saved in `/results/R_output/models/{model_name}.Rds`, `model_name` being `stanfit_descriptive_{exposure / disease}`.

Generated quantities are obtained from the model fit and saved in `/results/R_output/generated-quantities`. As the generated quantities are `rvar` objects (samples from the posterior distribution) which can be quite heavy, they are saved separately as R datasets (Rds).

These are:
- Predictions for the latent trend: `{model_name}_pred-trend.Rds`
- Summary statistics of the trend: `{model_name}_pred-trend-summ.Rds`

Reference dataframes for each analysis (1 row = 1 model fit and generated quantities) are saved in:
- `/results/R_output/environmental_descriptive-analysis_setup.Rds`
- `/results/R_output/disease_descriptive-analysis_setup.Rds`

### 06_association-modeling_fit-models.R

It fits the statistical models for the association analyses between each environmental exposure and disease pair.

For each exposure-disease pair, it fits the model **`association_exposure-disease-incidence.stan`** and saves it in `/results/R_output/models/{model_name}.Rds`, `model_name` being `stanfit_association_{exposure}_{disease}`.

It saves a reference dataframe with each analysis to perform in `/results/R_output/association-analysis_setup.Rds`.

### 07_association-modeling_generated-quantities.R

It generates quantities for the association analyses between each environmental exposure and disease pair, and saves them in `/results/R_output/generated-quantities`.

These are:
- Predictions for the exposure-response function: `{model_name}_pred-expo-resp.Rds`
- Counterfactual (exposure) conditions: `{model_name}_cf-cond.Rds`
- Predictions of counterfactual incidence rates, attributable fractions (AF) and numbers (AN): `{model_name}_pred-cf.Rds`
- Aggregated (all study period) AFs and ANs: `{model_name}_pred-cf-agg.Rds`
- Predictions of model components for explainability: `{model_name}_pred-explain.Rds`

The analysis setup dataframe with the paths to the generated quantities is saved in `/results/R_output/association-analysis_setup_gen-quant.Rds`

### 08_make-figures-and-tables.R

It creates publication-ready figures and tables and saves them in **`/results/plots/figures`** and **`/results/tables`**.

It also creates additional plots to help with the interpretation of the statistical models:
- Trend models:
	- Model component contributions to fit: `/results/plots/interpretation-help/trends/descriptive_{env / dis}_{exposure / disease}_trend_help.pdf`
- Exposure-disease associations models: 
	- Model component contributions to fit: `/results/plots/interpretation-help/associations/prediction-explanation_{exposure}_{disease}.pdf`
	- Counterfactual incidence rates: `/results/plots/interpretation-help/associations/cf-IR_{exposure}_{disease}.pdf`

### other figure scripts

`figure1.R`

`figure1_b.R`

## Outputs

### Processed data

The final dataset for the study period (2015 to 2021) is stored in `data/processed/final`.

Here are the corresponding data dictionaries:
#### population_final.csv

| Variable   | Type               | Values              | Description |
| ---------- | ------------------ | ------------------- | ----------- |
| district   | Categorical        | {Moshi, Siha}       | Location    |
| date       | Datum              | \[2015, 2021]       | Year        |
| population | Numerical, natural | \[123 091, 237 715] | Population  |

#### environmental_final.csv

| Variable       | Type                         | Values              | Description                                                                                                                                                 |
| -------------- | ---------------------------- | ------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| district       | Categorical                  | {Moshi, Siha}       | Location                                                                                                                                                    |
| date           | Datum                        | \[2015-01, 2021-12] | Year-month                                                                                                                                                  |
| pm2p5          | Numerical, rational          | \[6.7, 41.6]        | PM2.5 concentration ($\mu$ g/m $^3$)                                                                                                                          |
| temp_min       | Numerical, rational          | \[13.2, 22.0]       | (Monthly mean) minimum temperature (ºC)                                                                                                                     |
| temp_mean      | Numerical, rational          | \[18.4, 27.5]       | Mean temperature (ºC)                                                                                                                                       |
| temp_max       | Numerical, rational          | \[23.0, 33.1]       | (Monthly mean) maximum temperature (ºC)                                                                                                                     |
| utci           | Numerical, rational          | \[6.7, 35.3]        | Universal Thermal Climate Index (UTCI), multivariate parameter describing the synergistic heat exchanges between the thermal environment and the human body |
| total_rainfall | Numerical, positive rational | \[0, 603.9]         | Total rainfall (mm)                                                                                                                                         |
| greenness      | Numerical, rational          | \[0.35,  0.70]      | Normalized Difference Vegetation Index (NDVI), can take values between -1 and 1                                                                             |
| n_raindays     | Numerical, natural           | \[0, 19]            | Number of rain days                                                                                                                                         |

#### disease_final.csv

| Variable      | Type               | Values                                                        | Description                                                       |
| ------------- | ------------------ | ------------------------------------------------------------- | ----------------------------------------------------------------- |
| district      | Categorical        | {Moshi, Siha}                                                 | Location                                                          |
| date          | Datum              | \[2015-01, 2021-12]                                           | Year-month                                                        |
| disease       | Categorical        | {Diarrhea, ...}, n=30                                         | Disease name                                                      |
| disease_group | Categorical        | {Gastrointestinal Infections, ...}, n=14                      | Disease classification                                            |
| disease_block | Categorical        | {Infectious/Communicable Diseases, Non-Communicable Diseases} | Greater disease classification                                    |
| n_cases       | Numerical, natural | \[0, 10 355]                                                  | Number of cases reported in the outpatient department (incidence) |
