# ripa2tan_epi

This repository contains the code for the environmental epidemiology analyses of RIPA2TAN.

## Project structure

```
.
├── data
│   ├── processed
│   └── raw
├── doc
├── LICENSE
├── README.md
├── results
│   └── plots
└── scripts
    ├── 01_data-processing.qmd
    └── 02_description-and-imputation.qmd

```

## Workflow

### 01_data-processing.R

Processed raw data into three tables:

#### Environmental data

**`environmental.csv`**

| Variable       | Type                         | Values             | Description                                                                                                                                                    |
| -------------- | ---------------------------- | ------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **district**   | Categorical                  | {Moshi, Siha}      | Moshi is an urban district, Siha is a rural district                                                                                                           |
| **date**       | Datum                        | [2012-01, 2022-12] | Year-month                                                                                                                                                     |
| pm2p5          | Numerical, positive rational | [6, 42]            | PM2.5 concentration ($\mu$g/m$^3$)                                                                                                                             |
| temp_min       | Numerical, rational          | [9, 18]            | Minimum temperature (ºC)                                                                                                                                       |
| temp_mean      | Numerical, rational          | [14, 23]           | Mean temperature (ºC)                                                                                                                                          |
| temp_max       | Numerical, rational          | [18, 28]           | Maximum temperature (ºC)                                                                                                                                       |
| total_rainfall | Numerical, positive rational | [0, 604]           | Total rainfall (mm)                                                                                                                                            |
| n_raindays     | Numerical, natural           | [0, 19]            | Number of rain days                                                                                                                                            |
| greenness      | Numerical, rational          | [0.24, 0.70]       | Normalized Difference Vegetation Index (NDVI), can take values between -1 and 1                                                                                |
| utci           | Numerical, rational          | [6, 37]            | Universal Thermal Climate Index<br>(UTCI), multivariate parameter describing the synergistic heat exchanges between the thermal environment and the human body |

UTCI reference:
Di Napoli, C., Barnard, C., Prudhomme, C., Cloke, H. L., & Pappenberger, F. (2021). ERA5-HEAT: A global gridded historical dataset of human thermal comfort indices from climate reanalysis. _Geoscience Data Journal_, _8_(1), 2–10. [https://doi.org/10.1002/gdj3.102](https://doi.org/10.1002/gdj3.102)

#### Disease incidence data

**`disease.csv`**

| Variable             | Type               | Values                               | Description                                                                                                        |
| -------------------- | ------------------ | ------------------------------------ | ------------------------------------------------------------------------------------------------------------------ |
| **district**         | Categorical        | Moshi, Siha                          | Moshi is an urban district, Siha is a rural district                                                               |
| **date**             | Datum              | [2014-01, 2022-12]                   | Year-month                                                                                                         |
| disease              | Categorical        | {Acute Flaccid Paralysis, ...}, n=47 | The disease name                                                                                                   |
| disease_group        | Categorical        | {Cancer, ...}, n=17                  | What category the disease belongs to according to Netra and Winnie's classification                                |
| disease_communicable | Categorical        | {CD, NCD, Both}                      | Whether the disease is communicable (CD), non-communicable (NCD), or could be considered in both categories (Both) |
| n_cases              | Numerical, natural | [0, 10355]                           | Number of cases                                                                                                    |


#### Population data

**`population.csv`**

| Variable     | Type               | Values           | Description                                          |
| ------------ | ------------------ | ---------------- | ---------------------------------------------------- |
| **district** | Categorical        | {Moshi, Siha}    | Moshi is an urban district, Siha is a rural district |
| **date**     | Datum              | [2012, 2021]     | Year                                                 |
| population   | Numerical, natural | [117773, 237715] | Population                                           |


---

### 02_description-and-imputation.R

#### Time series regression (1)

Describe yearly **Population** trend for Moshi and Siha, and **forecast** population for **2022**, so as to make descriptive analysis of **disease incidence rates**.


Describe **trend, seasonal and residual** components of **Environmental** variables, for Moshi and Siha (available data in each case).
**Impute/forecast** missing values in the time range **2014-2021**, to make the association analyses with the diseases possible for this range.

Describe **trend, seasonal and residual** components of **Disease incidence rates**, for Moshi and Siha. Time range **2014-2022**.
Select diseases with complete cases for the association analyses.

---

Analyses for the trend:
- Comment on shape (increase/decrease)
- Estimate trend change from 2012-01 to 2021-12 (absolute and percentage)
- (Estimate slope and point period of greatest change)
- (Estimate difference in trends between Moshi and Siha)


---

**Specific steps**:

- Linear prediction of 2022 population: **`population_imputed.csv`**
- Description of 2014-2022 incidence rates using GAM
- Description of 2012-2021(22) environmental variables using GAM
- Prediction of missing environmental data for 2014-2021: **`environmental_imputed.csv`**

#### GAM specification (1)
We employed penalized generalized additive models (GAMs) to decompose the different time series into trend and seasonal components. The models were of the following form:

$$
\begin{gathered}
g  [ \mathbb{E}(y_{i}) ] = \alpha_{d[i]} + f_{1,d[i]}(t_{i}) + f_{2,d[i]}(m_{i}) + f_{3,d[i]}(t_{i}, m_{i}) \\
\end{gathered}
$$

where $y_i$ is the response variable for observation $i$, which follows an exponential family distribution, and $g$ is the link function. The index variable $d_i$ specifies the district the observation belongs to (Moshi or Siha); $t_i$ is the time index, which has a monthly frequency and goes from 1 to $T$ (we have time series with different ranges); and $m_i$ is the month index, which takes values from 1 to 12. 

More briefly:

$$
\begin{gathered}
d_i \in \{\text{Moshi}, \text{Siha}\} \\
t_i = 1,...,T \\
m_i = 1,...,12 \\
\end{gathered}
$$

Each district presents a different intercept $\alpha$, and different smooth functions $f_1(t_i)$, $f_2(m_i)$ and $f_3(t_i,m_i)$. The smooth functions are spline functions, which are paired with penalties to their wiggliness that are taken into account during the fitting process. Spline functions consist in a linear combination of basis functions ($b$), and the number of basis functions is known as the basis complexity, which gives the maximum complexity of the spline function. The temporal trend is represented with $f_1(t_i)$, which is a cubic spline with a basis complexity of one function per year ($J$). The seasonal variation is represented with $f_2(m_i)$, which is a cyclic cubic spline with a basis complexity of 12. Finally, $f_3(t_i,m_i)$ is the tensor product interaction between the trend and seasonal smooth functions, which allows for the seasonal component to change between time steps.

We can write explicitly the spline functions in the following manner, where $\beta$ are the coefficients for the different basis functions $b$:

$$
\begin{gathered}
f_{1,d[i]}(t_i) = \sum_{j=1}^{J} \beta_{d_[i],j} \ b_{1,j}(t_i) \\
f_{2,d[i]}(m_i) = \sum_{k=1}^{12} \beta_{d_[i],k} \ b_{2,k}(m_i) \\
f_{3,d[i]}(t_i, m_i) = \sum_{j=1}^{J} \sum_{k=1}^{12} \beta_{d_[i],j,k} \
b_{1,j}(t_i) \ b_{2,k}(m_i) \\
\end{gathered}
$$

This model formulation is the same as fitting separate models for each district, but we included both districts in the model to be able to make contrasts between them.

For a given district, we can express the model equation in terms of a trend and a seasonal component:
$$
\begin{gathered}
g  [ \mathbb{E}(y_{i}) ] = \text{Trend}_i + \text{Seasonal}_i \\
\end{gathered}
$$

where:

$$
\begin{gathered}
\text{Trend}_i = \alpha + f_1(t_i) \\
\text{Seasonal}_i = f_2(m_i) + f_3(t_i,m_i) \\
\end{gathered}
$$

---

Useful references:

Splines:
Perperoglou, A., Sauerbrei, W., Abrahamowicz, M., & Schmid, M. (2019). A review of spline function procedures in R. _BMC Medical Research Methodology_, _19_(1), 46. [https://doi.org/10.1186/s12874-019-0666-3](https://doi.org/10.1186/s12874-019-0666-3)

Modelling with GAMs:
Pedersen, E. J., Miller, D. L., Simpson, G. L., & Ross, N. (2019). Hierarchical generalized additive models in ecology: an introduction with mgcv. _PeerJ_, _7_, e6876. [https://doi.org/10.7717/peerj.6876](https://doi.org/10.7717/peerj.6876)

#### Model particularities

##### Environmental variables

For variables `pm2p5`, `temp_min`, `temp_mean`, `temp_max`, `greenness` and `utci`, a Normal model was considered for the response:

$$
\begin{gathered}
y_i \sim \text{Normal}
\end{gathered}
$$

For `total_rainfall`, a Gamma model with log link function, as the variable is strictly positive and presents a heavy tail to the right. The variable had to be transformed to `total_rainfall`+1 to avoid the logarithm of zero.

$$
\begin{gathered}
y_i \sim \text{Gamma} \\
g = \log
\end{gathered}
$$

For `n_raindays`, a Negative Binomial model with log link function, as the variable is a count and we found evidence of overdispersion when fitting a Poisson model. The variable also had to be transformed to `n_raindays`+1 to avoid the logarithm of zero.

$$
\begin{gathered}
y_i \sim \text{NegBin} \\
g = \log
\end{gathered}
$$

For the Normal variables, the time series decomposition in the scale of the response is additive.

$$
\begin{gathered}
g  [ \mathbb{E}(y_{i}) ] = \mathbb{E}(y_i) = \text{Trend}_i + \text{Seasonal}_i \\
\end{gathered}
$$

However, for the variables with a log link function, the time series decomposition is additive in the log scale, but it becomes multiplicative in the response scale:

$$
\begin{gathered}
\log  [ \mathbb{E}(y_{i}) ] = \text{Trend}_i + \text{Seasonal}_i \\
\mathbb{E}(y_{i}) = \exp(\text{Trend}_i) \ \exp(\text{Seasonal}_i)
\end{gathered}
$$


##### Disease incidence rates

We can model the disease incidence rates (new monthly cases by exposed population) by specifying a count regression model for the incidence, and allowing the expected value to change with respect to the exposed population.

We considered a Negative Binomial model for the monthly incidence with a log link function:

$$
y_i \sim \text{NegBin}(\mu_i, \theta)
$$

where $\mu_i$ is the expected incidence and $\theta$ is the dispersion parameter. The incidence is equal to the incidence rate ($\lambda$) times the exposed population ($P$), so $\mu_i=\lambda_i P_i$. Therefore:

$$
\log \mu_i = \log P_i + \log \lambda_i
$$

Here, $\log P_i$ is an offset in the equation, and we can model the incidence rate as the linear predictor we specified previously:

$$
\log \lambda_i = \alpha_{d[i]} + f_{1,d[i]}(t_{i}) + f_{2,d[i]}(m_{i}) + f_{3,d[i]}(t_{i}, m_{i})
$$

The  decomposition into trend and seasonal is also multiplicative for the response in this case.

---

Useful references:

Gamma regression:
https://library.virginia.edu/data/articles/getting-started-with-gamma-regression

Rate models:
https://library.virginia.edu/data/articles/getting-started-with-rate-models


### 03_next-script.R

Exposure-lag-response models.

