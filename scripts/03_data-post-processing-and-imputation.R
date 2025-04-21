# It creates the final dataset for the study period (2015 to 2021) and saves it in
# `/data/processed/final`. The resulting files are:
# - `population_final.csv`
# - `environmental_final.csv`
# - `disease_final.csv`
# 
# Population data is kept as is, disease data is post-processed and the environmental
# data of 4 variables is imputed.

# setup
source("00_source.R")

# GAM - spline machinery
library(mgcv)

# Bayesian models
library(rstan)
library(posterior)
library(tidybayes)
library(ggdist)

# run MCMC chains in parallel
options(mc.cores = parallel::detectCores() - 1)

# Disease --------------------------------------------------------------------------------


## fix missings --------
dis <- read_csv(file.path(data_path, "processed", "initial", "disease.csv"))
dis <- 
  dis |> 
  complete(district, date, nesting(disease, disease_group, disease_communicable))


## substitute the final groupings -------------

ref_groups <- read_csv(file.path(data_path, "processed", "initial", "disease-grouping.csv"))

dis <-
  dis |> 
  left_join(ref_groups, join_by(disease)) |> 
  select(- c(disease_group, disease_communicable)) |> 
  rename(
    disease_block = disease_block_final,
    disease_group = disease_group_final
  ) |> 
  relocate(
    district, date, disease, disease_group, disease_block
  )


## remove and aggregate diseases ------------
# according to the input from the clinical team

# remove diseases with no assigned group (Other vector-borne diseases)
dis |> 
  filter(is.na(disease_group)) |> 
  pull(disease) |> unique()

dis <-
  dis |> 
  filter(! is.na(disease_group))

dis$disease |> table()

# diseases to remove according to clinical input
dis <- 
  dis |> 
  filter(! disease %in% c("Acute Flaccid Paralysis", "Meningitis",
                          "Gynecological Diseases"))


# Add cases of all Malnutrition diseases:
# - Nutritional disorders, other
# - Moderate malnutrition
# - Marasmus
# - Marasmic Kwashiorkor
# - Kwashiorkor

dis_malnutrition <- 
  dis |> 
  filter(disease_group == "Malnutrition")

dis_malnutrition <-
  dis_malnutrition |> 
  mutate(
    disease = "Malnutrition"
  ) |> 
  group_by(pick(everything(), -c(n_cases))) |> 
  summarise(
    n_cases = sum(n_cases)
  ) |> 
  ungroup()

dis <-
  dis |> 
  filter(! disease_group == "Malnutrition")

dis <- bind_rows(
  dis,
  dis_malnutrition
)

rm(dis_malnutrition)

# Add cases of Diarrhea:
# - Diarrhea With Severe Dehydration
# - Diarrhea With No Dehydration

dis_diarrhea <- dis |> 
  filter(
    disease %in% c("Diarrhea With Severe Dehydration", "Diarrhea With No Dehydration")
  )

dis_diarrhea <- 
  dis_diarrhea |> 
  mutate(
    disease = "Diarrhea"
  ) |> 
  group_by(pick(everything(), -c(n_cases))) |> 
  summarise(
    n_cases = sum(n_cases)
  ) |> 
  ungroup()

dis <-
  dis |> 
  filter(
    ! disease %in% c("Diarrhea With Severe Dehydration", "Diarrhea With No Dehydration")
  )

dis <- bind_rows(
  dis,
  dis_diarrhea
)

rm(dis_diarrhea)

# Remove other diseases with missings
# - Cholera
# - Rabies
# - Measles
# - Influenza

dis <- 
  dis |> 
  filter(! disease %in% c("Cholera", "Rabies", "Measles", "Influenza"))

### save final datasets --------------

dis <-
  dis |> 
  arrange(district, disease_block, disease_group, disease, date)


#### complete observations -------------

dis |> 
  write_csv(
    file.path(data_path, "processed", "interim", "disease_final_complete.csv")
  )

#### modeling period 2015-2021 -------------

dis |> 
  filter(
    year(date) >= 2015 & year(date) <= 2021
  ) |> 
  write_csv(
    file.path(data_path, "processed", "final", "disease_final.csv")
  )


# Population -----------------------------------------------------------------------------

# to be able to describe incidence rates for 2022, we will impute it

## impute 2022 ------------
# with a linear prediction

pop <- read_csv(file.path(data_path, "processed", "initial", "population.csv"))

# get time index for regression
pop_reg <- 
  pop |> 
  mutate(year=year(date))

# fit
fit <- lm(population ~ year*district, data=pop_reg)
summary(fit)

# predict
newdata <- data.frame(
  district = c("Moshi", "Siha"),
  year = rep(2022, 2),
  date = rep(as.Date("2022-01-01"), 2)
)

newdata <- cbind(
  newdata,
  population = round(predict(fit, type="response", newdata=newdata))
)

newdata

# Join
pop_imp <-
  pop_reg |> 
  full_join(newdata) |> 
  select(- year) |> 
  arrange(district, date)


# check imputation
pop_imp |>
  left_join(mutate(pop, population_orig = population)) |> 
  mutate(
    imputed = if_else(is.na(population_orig), "Yes", "No")
  ) |> 
  ggplot() +
  geom_vline(
    aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
  ) +
  geom_vline(
    aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
  ) +
  geom_line(
    aes(x=date, y=population, color=district), linewidth=0.8
  ) +
  geom_point(
    aes(x=date, y=population, color=district, shape=imputed, size=imputed)
  ) +
  scale_shape_manual(
    values = c(16,4)
  ) +
  scale_size_manual(
    values = c(3,8)
  ) +
  scale_y_continuous(labels = comma, n.breaks = 6) +
  scale_x_date(
    date_breaks="year", date_labels = "%Y",
  ) +
  labs(
    color = "District",
    x = "Year",
    y = "Population",
    shape = "Imputed",
    size = "Imputed"
  )

ggsave(
  file.path(res_path, "plots", "final-description", "population_imputed.pdf"),
  width=20*0.8, height=10.6*0.7
)


## save imputed dataset --------------

pop_imp |> 
  write_csv(
    file.path(data_path, "processed", "interim", "population_2022-imputed.csv")
  )


rm(fit, newdata, pop_reg)


## save final dataset (2015-2021)

pop |> 
  filter(
    year(date) >= 2015 & year(date) <= 2021
  ) |> 
  write_csv(
    file.path(data_path, "processed", "final", "population_final.csv")
  )

# Environmental --------------------------------------------------------------------------

# load
envir <- read_csv(file.path(data_path, "processed", "initial", "environmental.csv"))

## imputation 2015-2021 -------------------------

# utci: 1 observation in the middle
# greenness: several observations throughout
# n_raindays: siha in 2021
# total_rainfall: siha in 2021

# the model will be:
# latent level modeled as random walk of order 1
# annual seasonality modeled as penalized cyclic cubic spline


## prepare data --------------------------------------------

mod_df <- envir

# format variables for modeling
mod_df <-
  mod_df |> 
  mutate(
    district = factor(district, levels=c("Moshi", "Siha")),
    year_ind = year(date) - min(year(date)) + 1,
    month_ind = month(date),
    time_ind = (year_ind - 1)*12 + month_ind,
    offset = 0 # add offset to comply with stan script
  ) |> 
  select(- year_ind)

# mod_df$total_rainfall |> sort()

# environmental data to long format
mod_df <-
  mod_df |> 
  pivot_longer(
    c(pm2p5, temp_mean, utci, total_rainfall, greenness, temp_min, temp_max, n_raindays),
    names_to = "exposure",
    values_to = "E"
  )

# select variables for imputation
mod_df <-
  mod_df |> 
  filter(
    exposure %in% c("utci", "greenness", "n_raindays", "total_rainfall")
  )

# nest data by exposure ----------------.
df_iter <-
  mod_df |> 
  nest(.by=c(exposure))
rm(mod_df)

df_iter$data[[1]]

# add useful variables

# exposure labels for plots and likelihood (family) for model
# family:
# 1 - normal
# 2 - gamma
# 3 - negative binomial

# likelihood function is only evaluated at the observed data points;
# the parameters are estimated for the whole temporal range, and predictions for the
# missing points are generated

expo_ref <-
  tribble(
    ~exposure,             ~label,       ~units,            ~family,
    "utci",           "UTCI",          "",             1,
    "total_rainfall", "Rainfall",   "(mm)",            2, # strictly positive, with zeros
    "greenness",      "Greenness",  "(NDVI)",          1,
    "n_raindays",     "No. rain days", "",             3 # counts (use Gamma for imputation, then round)
  )

df_iter <-
  df_iter |> 
  left_join(
    expo_ref, join_by(exposure)
  ) |> 
  relocate(data, .after=last_col())


# add id for joins in each dataset
df_iter$data <-
  pmap(
    df_iter,
    function(data, ...) {
      data |> 
        mutate(
          i = row_number()
        ) |> 
        relocate(i)
    }
  )

df_iter

## prepare stan data --------------------------

df_iter$data[[4]] |> print(n=Inf)

df_iter |> 
  filter(exposure=="n_raindays") |> 
  pull(data) |> 
  pluck(1) |> 
  pull(E) |> 
  sort()

df_iter$dat <-
  pmap(
    df_iter,
    function(data, family, exposure, ...) {
      
      dat <-
        data |> 
        select(
          E, offset, time_ind, district
        ) |> 
        rename(
          Y = E,
          offset = offset,
          T = time_ind,
          L = district
        ) |> 
        mutate(
          T = as.factor(T)
        ) |> 
        compose_data()
      
      # missing and observed indices
      dat$miss <- data$i[which(is.na(dat$Y))]
      dat$obs <- data$i[which(! is.na(dat$Y))]
      # no. observations
      dat$n_miss <- length(dat$miss)
      dat$n_obs <- length(dat$obs)
      
      # add observed Y
      dat$Y_obs <- dat$Y[dat$obs]
      
      # remove Y vector with missings
      dat$Y <- NULL
      
      
      # if total_rainfall, add small offset so that we can model it with a Gamma likelihood
      # (12 values are exactly zero)
      if (exposure == "total_rainfall") {
        dat$Y_obs <- dat$Y_obs + 1e-6
      }
      
      # if normal variable, standardize
      if (family == 1) {
        dat$Y_obs <- standardize(dat$Y_obs)
      }
      
      
      # spline for seasonality (month index)
      basis_s <-
        smoothCon(
          s(month_ind, bs="cc", k=12),
          knots=list(
            month_ind=c(0.5, 12.5)
          ),
          data=data,
          absorb.cons = TRUE,
          null.space.penalty = TRUE,
          sparse.cons = 0
        )[[1]]
      
      dat$sp_s <- basis_s$X
      dat$n_sp_s <- ncol(basis_s$X)
      
      dat$Omega_s <- basis_s$S
      dat$n_Omega_s <- length(basis_s$S)
      
      # add likelihood setting
      dat$family <- family
      
      return(dat)
    }
  )

df_iter$dat[[2]] |> str()
df_iter$dat[[4]]$Y_obs
df_iter$dat[[2]]$obs
df_iter$data[[3]] |> print(n=Inf)


## fit models ------------------------------

# compile model
smod <-
  stan_model(
    file = file.path("stan_models", "descriptive_time-series_choose-likelihood_imputation.stan")
  )

# fit models
df_iter$fit <-
  pmap(
    df_iter,
    function(dat, exposure, ...) {
      
      print(exposure)
      
      sampling(
        object = smod,
        data = dat,
        chains=4, cores = 4,
        iter=2500, warmup=1000,
        seed = 1,
        # default adapt_delta is 0.8
        control=list(adapt_delta=0.95)
      )
    }
  )

# check fit
tictoc::tic()
pwalk(
  df_iter[3,],
  function(fit, exposure, ...) {
    print(exposure)
    
    summary(fit)$summary[, c("mean", "sd", "n_eff", "Rhat")] |> 
      as.data.frame() |> 
      rownames_to_column("par") |> 
      as_tibble() |> 
      arrange(desc(n_eff)) |> 
      print(n=Inf)
    
    pryr::object_size(fit) |> print()
  }
)
tictoc::toc()



## get imputed dataset ---------------------

df_iter$data_imp <-
  pmap(
  df_iter,
  function(fit, data, dat, family, exposure, ...) {
    
    # get draws
    dr <- as_draws_rvars(fit)
    
    # get imputed values
    Y_imp <- map_vec(dr$Y, mean)
    
    # round counts
    if (exposure == "n_raindays") {
      Y_imp <- round(Y_imp)
    }
    
    # destandardize the normal variables
    if (family == 1) {
      Y_imp <- destandardize(Y_imp, org_vec = data$E, na.rm=TRUE)
    }
    
    
    data
    # create imputed data vector
    E_imp <- data$E
    E_imp[dat$miss] <- Y_imp
    
    # add it to original dataframe
    data_imp <-
      bind_cols(
        data,
        E_imp = E_imp
      )
      
    return(data_imp)    
  }
)

## visualize imputation ---------------

p <-
  pmap(
  df_iter,
  function(data, data_imp, label, units, ...) {
    
    data_imp |> 
      mutate(
        imputed = if_else(is.na(E), "Yes", "No")
      ) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date, y=E_imp, color=district), linewidth=0.8
      ) +
      geom_point(
        aes(x=date, y=E_imp, color=district, shape=imputed, size=imputed)
      ) +
      scale_shape_manual(
        values = c(16,4)
      ) +
      scale_size_manual(
        values = c(3,8)
      ) +
      # scale_y_continuous(labels = comma, n.breaks = 6) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y",
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = TeX(glue("{label} {units}")),
        shape = "Imputed",
        size = "Imputed"
      )
  }
) |> 
  wrap_plots(
    ncol=1,
    guides="collect",
  ) +
  plot_layout(axis_titles="collect")


p

ggsave(
  file.path(res_path, "plots", "final-description", "environmental_imputed_all.pdf"),
  width=20*1, height=10.6*1
)

p &
  scale_x_date(
    limits = c("2015-01-01", "2021-12-01") |>
      as.Date(),
    date_breaks="year", date_labels = "%Y"
  )

ggsave(
  file.path(res_path, "plots", "final-description", "environmental_imputed_2015-2021.pdf"),
  width=20*1, height=10.6*1
)


## save imputed dataset -----------

tmp <-
  pmap(
  df_iter,
  function(data_imp, exposure, ...) {
    data_imp |> 
      select(-E) |> 
      rename(E = E_imp) |> 
      select(district, date, E) |> 
      mutate(exposure = exposure)
  }
) |> 
  bind_rows() |> 
  pivot_wider(
    names_from = exposure,
    values_from = E
  )

tmp

envir_imp <-
  envir |> 
  select(- c(utci, total_rainfall, greenness, n_raindays)) |> 
  left_join(tmp)


# filter to 2015-2021 and save
envir_imp |> 
  filter(
    year(date) >= 2015 & year(date) <= 2021
  ) |> 
  write_csv(
    file.path(data_path, "processed", "final", "environmental_final.csv")
  )
