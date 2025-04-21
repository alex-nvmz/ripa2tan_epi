# It fits the statistical models for the association analyses between each
# environmental exposure and disease pair

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

# prepare data ---------------------------------------------------------------------------

dis <-
  read_csv(
    file.path(data_path, "processed", "final", "disease_final.csv")
  )

pop <- 
  read_csv(
    file.path(data_path, "processed", "final", "population_final.csv")
  )

envir <- 
  read_csv(
    file.path(data_path, "processed", "final", "environmental_final.csv")
  )


# add population to disease df
dis <-
  dis |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop, year=year(date)), join_by(year, district)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

rm(pop)

# get empirical case rate per 1e5 people
dis <-
  dis |>
  mutate(
    case_rate_1e5 = n_cases / population * 1e5
  )

# model dataframe
mod_df <- dis

# join environmental data
mod_df <-
  mod_df |> 
  left_join(
    envir,
    join_by(district, date)
  )

# format variables for modeling
mod_df <-
  mod_df |> 
  mutate(
    district = factor(district, levels=c("Moshi", "Siha")),
    year_ind = year(date) - min(year(date)) + 1,
    month_ind = month(date),
    time_ind = (year_ind - 1)*12 + month_ind
  ) |> 
  select(- year_ind)

# environmental data to long format
mod_df <-
  mod_df |> 
  pivot_longer(
    c(pm2p5, temp_mean, utci, total_rainfall, greenness, temp_min, temp_max, n_raindays),
    names_to = "exposure",
    values_to = "E"
  )

# nest data by disease and exposure ------.
df_iter <-
  mod_df |> 
  nest(.by=c(exposure, disease, disease_group, disease_block))
rm(mod_df)

# add useful variables

# exposure labels for plots
expo_ref <-
  tribble(
    ~exposure,             ~label,       ~units,
    "pm2p5",          "PM2.5",      "($\\mu$g/m$^3$)",
    "temp_mean",      "Mean temp.", "(ºC)",
    "utci",           "UTCI",       "",
    "total_rainfall", "Rainfall",   "(mm)",
    "greenness",      "Greenness",  "(NDVI)",
    # supp
    "temp_min",       "Min. temp.", "(ºC)",
    "temp_max",       "Max. temp.", "(ºC)",
    "n_raindays",     "No. rain days", "",
  )

df_iter <-
  df_iter |> 
  left_join(
    expo_ref, join_by(exposure)
  ) |> 
  relocate(label, units, .after = disease)

# disease names for file writing
df_iter <-
  df_iter |> 
  mutate(
    disease_print = str_replace_all(
      disease, c(
        " "="-",
        "/"="-",
        ","="-",
        "\\("="-",
        "\\)"="-"
      )
    )
  ) |> 
  relocate(disease_print, .after=disease)


df_iter |> print(n=Inf)

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

df_iter[4,]
df_iter$data[[4]]

# order
df_iter <-
  df_iter |> 
  mutate(
    exposure = fct_relevel(exposure, main_env),
    disease = fct_relevel(disease, main_dis)
  ) |> 
  arrange(
    disease, exposure
  ) |> 
  mutate(
    exposure = as.character(exposure),
    disease = as.character(disease)
  )


# subset temporarily
# df_iter <-
  # df_iter |>
  # filter(
  #   disease %in% c("Chronic Respiratory Disease"),
  #   exposure %in% c("pm2p5")
  # )
# df_iter <-
#   df_iter[sample(1:nrow(df_iter), 1), ]

# df_iter <-
#   df_iter |> 
#   filter(disease %in% main_dis)
  

# prepare stan data --------------------------

# df_iter$data[[1]]$date |> range() |> year()
# 7 years

df_iter$dat <-
  pmap(
    df_iter,
    function(data, exposure, ...) {
      
      dat <-
        data |> 
        select(
          n_cases, population, time_ind, district
        ) |> 
        rename(
          D = n_cases,
          P = population,
          T = time_ind,
          L = district
        ) |> 
        mutate(
          T = as.factor(T)
        ) |> 
        compose_data()
      
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
      
      # spline of exposure
      nbf <- 4
      data_vec <- data$E
      knots <- quantile(data_vec, seq(0,1,length.out=nbf+1))

      basis_E <-
        smoothCon(
          s(data_vec, bs="cr", k=length(knots)),
          knots=list(
            data_vec=knots
          ),
          data=tibble(data_vec),
          absorb.cons = TRUE,
          null.space.penalty = TRUE,
          sparse.cons = 0
        )[[1]]
      
      dat$sp_E <- basis_E$X
      dat$n_sp_E <- ncol(basis_E$X)

      dat$Omega_E <- basis_E$S
      dat$n_Omega_E <- length(basis_E$S)
      
      # to reconstruct the predictor matrix for other values of E
      dat$basis_E <- basis_E
      
      return(dat)
    }
  )

# reasoning priors
tibble(
  # x = rgamma(1e5, shape=10*0.005, rate=0.005),
  x = rexp(1e5, 2)
  # x = rnorm(1e5, 100, 30)
  # x = rnorm(1e5, 30, 30)
  # x = rnorm(1e5, 5, 1) |> exp()
  # x = rnorm(1e5, 0, 2)
) |>
  ggplot() +
  geom_density(aes(x=x))

tibble(
  x = seq(-100, 100, 0.2),
  y = dcauchy(x, 0, 10)
) |>
  ggplot() +
  geom_line(aes(x=x, y=y))


## record fitted model names and paths ----------------------------

df_iter <-
  df_iter |> 
  mutate(
    model_name = g("stanfit_association_{exposure}_{disease_print}"),
    fit_path = file.path(
      res_path, "R_output", "models", g("{model_name}.Rds")
    )
  ) 

## save analysis setup ---------------------------------

df_iter |> 
  saveRDS(file.path(
    res_path, "R_output", "association-analysis_setup.Rds"
  ))


## fit models ------------------------------

# compile model
smod <-
  stan_model(
    file = file.path("stan_models", "association_exposure-disease-incidence.stan")
  )

# fit models
pwalk(
  df_iter,
  function(dat, fit_path, ...) {
    
    if (file.exists(fit_path)) {
      print(g("{fit_path}\n{'\t'}already exists; model not fitted\n"))
      
    } else {
      print(fit_path)
      sampling(
        object = smod,
        data = dat,
        chains=4, cores = 4,
        iter=2500, warmup=1000,
        seed = 1,
        # default adapt_delta is 0.8
        control=list(adapt_delta=0.95)
      ) |> 
        saveRDS(fit_path)
      
    }
  }
)


# check fit
tictoc::tic()
pwalk(
  df_iter[1,],
  function(fit_path, ...) {
    print(fit_path)
    
    fit <- readRDS(fit_path)
    
    summary(fit)$summary[, c("mean", "sd", "n_eff", "Rhat")] |> 
      as.data.frame() |> 
      rownames_to_column("par") |> 
      as_tibble() |> 
      arrange(n_eff) |> 
      print(n=Inf)
    
    pryr::object_size(fit) |> print()
  }
)
tictoc::toc()
