# It performs the trend analyses for the environmental and disease time series

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


# Describe environmental TS --------------------------------------------------------------

## prepare data 2 --------------------------------------------

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

# nest data by exposure ----------------.
df_iter <-
  mod_df |> 
  nest(.by=c(exposure))
rm(mod_df)

df_iter$data[[4]]

# add useful variables

# exposure labels for plots and likelihood (family) for model
# family:
# 1 - normal
# 2 - gamma
# 3 - negative binomial

expo_ref <-
  tribble(
    ~exposure,             ~label,       ~units,            ~family,
    "pm2p5",          "PM2.5",      "($\\mu$g/m$^3$)", 1,
    "temp_mean",      "Mean temp.", "(ºC)",            1,
    "utci",           "UTCI",          "",             1,
    "total_rainfall", "Rainfall",   "(mm)",            2, # strictly positive, with zeros
    "greenness",      "Greenness",  "(NDVI)",          1,
    "temp_min",       "Min. temp.", "(ºC)",            1,
    "temp_max",       "Max. temp.", "(ºC)",            1,
    "n_raindays",     "No. rain days", "",             3 # counts
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

df_iter$data[[1]]

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
      
      
      # if total_rainfall, add small offset so that we can model it with a Gamma likelihood
      # (12 values are exactly zero)
      if (exposure == "total_rainfall") {
        dat$Y <- dat$Y + 1e-6
      }
      
      # if normal variable, standardize
      if (family == 1) {
        dat$Y <- standardize(dat$Y)
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

df_iter$dat[[4]] |> str()
df_iter$dat[[1]] |> str()

df_iter$data[[4]] |> print(n=Inf)


## record fitted model names and paths ----------------------------

df_iter <-
  df_iter |> 
  mutate(
    model_name = g("stanfit_descriptive_{exposure}"),
    fit_path = file.path(
      res_path, "R_output", "models", g("{model_name}.Rds")
    )
  ) 

df_iter$fit_path

## fit models ------------------------------

# compile model
smod <-
  stan_model(
    file = file.path("stan_models", "descriptive_time-series_choose-likelihood.stan")
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
  df_iter[8,],
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


## get trend predictions ------------------

# define object names
df_iter <-
  df_iter |> 
  mutate(
    pred_trend_path = file.path(
      res_path, "R_output", "generated-quantities",
      g("{model_name}_pred-trend.Rds")
    )
  )

pwalk(
  df_iter,
  function(fit_path, pred_trend_path, data, dat, family, ...) {
    
    print(fit_path)
    
    # load fit
    fit <- readRDS(fit_path)
    
    # df to add predictions to
    pred_df <-
      bind_cols(
        i = data$i,
        E = data$E,
        dat[c("T", "L")]
      )
    # get draws
    dr <- as_draws_rvars(fit) |> 
      map(as.matrix)
    
    # get predictions
    pred_df <-
      pred_df |> 
      mutate(
        # trend component (linear predictor scale)
        eta_x = pmap_vec(
          list(i, L, T), \(i, L, T) {
            (dr$x[T, L]) |> 
              as.vector()
          }
        ),
        # seasonal component (linear predictor scale)
        eta_s = pmap_vec(
          list(i, L, T), \(i, L, T) {
            (as_rvar(dat$sp_s)[i,] %**% dr$theta_s[, L]) |> 
              as.vector()
          }
        ),
        # trend: intercept + latent level (outcome scale)
        pred_trend = pmap_vec(
          list(i, L, T, eta_x), \(i, L, T, eta_x) {
            (dr$alpha[L] + eta_x) |> 
              as.vector()
          }
        ),
        # fit: predicted rate
        pred_fit = pmap_vec(
          list(i, L, T, eta_x, eta_s), \(i, L, T, eta_x, eta_s) {
            (dr$alpha[L] + eta_x + eta_s) |> 
              as.vector()
          }
        )
      )
    # if Normal variable, destandardize pred_trend and pred_fit
    if (family == 1) {
      pred_df$pred_trend <-
        destandardize(pred_df$pred_trend, org_vec = pred_df$E)
      
      pred_df$pred_fit <-
        destandardize(pred_df$pred_fit, org_vec = pred_df$E)
      
      
    # if Gamma or NegBin, apply inverse link function
    } else if (family %in% c(2,3)) {
      pred_df$pred_trend <-
        exp(pred_df$pred_trend)
      
      pred_df$pred_fit <-
        exp(pred_df$pred_fit)
    }
    
    # post-process to save as .Rds
    pred_df <- rvar_tib_postproc(pred_df)
    
    # save
    pred_df |> 
      saveRDS(pred_trend_path)
    
  })


## get summary statistics of the trend -------------------

# define object names
df_iter <-
  df_iter |> 
  mutate(
    pred_trend_summ_path = file.path(
      res_path, "R_output", "generated-quantities",
      g("{model_name}_pred-trend-summ.Rds")
    )
  )

pwalk(
  df_iter,
  function(pred_trend_path, pred_trend_summ_path, data, ...) {
    
    print(pred_trend_path)
    
    # load fit
    pred_df <- readRDS(pred_trend_path)
    
    # add original data
    pred_df <-
      pred_df |> 
      left_join(data)
    
    # get trend at time start
    trend_start <-
      pred_df |> 
      group_by(district) |> 
      filter(date == min(date)) |> 
      select(district, pred_trend) |> 
      rename(trend_start = pred_trend)
    
    # get trend at time end
    trend_end <-
      pred_df |> 
      group_by(district) |> 
      filter(date == max(date)) |> 
      select(district, pred_trend) |> 
      rename(trend_end = pred_trend)
    # get trend change from start to end
    trend_change <-
      trend_start |> 
      left_join(trend_end) |> 
      summarise(
        trend_change = trend_end - trend_start
      )
    
    # get average annual trend change
    an_trend_change <-
      pred_df |> 
      group_by(district) |> 
      mutate(
        an_trend_change = slider::slide_vec(
          pred_trend,
          ~ last(.x) - first(.x),
          .before = 12,  # Include current + previous 12 for 13-step window
          .complete = TRUE,
          .i = time_ind
        )
      ) |> 
      # drop missings
      mutate(
        sel = mean(an_trend_change)
      ) |> 
      drop_na(sel) |> 
      # compute average
      select(district, an_trend_change) |> 
      summarise(
        an_trend_change = rvar_mean(an_trend_change)
      )
    
    # combine results
    res <-
      trend_start |> 
      left_join(trend_end) |> 
      left_join(trend_change) |> 
      left_join(an_trend_change) |> 
      ungroup()
    
    # post-process to save as .Rds
    res <- rvar_tib_postproc(res)
    
    # save
    res |>
      saveRDS(pred_trend_summ_path)
    
  })



## save analysis setup -------------------------------------------------------------------

df_iter |> 
  saveRDS(file.path(
    res_path, "R_output", "environmental_descriptive-analysis_setup.Rds"
  ))


# Describe disease TS --------------------------------------------------------------------

## prepare data 2 --------------------------------------------

mod_df <- dis

# format variables for modeling
mod_df <-
  mod_df |> 
  mutate(
    district = factor(district, levels=c("Moshi", "Siha")),
    year_ind = year(date) - min(year(date)) + 1,
    month_ind = month(date),
    time_ind = (year_ind - 1)*12 + month_ind,
    # offset necessary (model incidence rate per 1e5 people)
    offset = log(population/1e5)
  ) |> 
  select(- year_ind)

# mod_df$offset |> unique()

# nest data by disease ----------------.
df_iter <-
  mod_df |> 
  nest(.by=c(disease, disease_group, disease_block))
rm(mod_df)

df_iter$data[[1]]

# add useful variables

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

# family distribution indicator for stan script
# 3 = negative binomial
df_iter <-
  df_iter |> 
  mutate(
    family = 3
  ) |> 
  relocate(family, .before=data)

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

df_iter$data[[1]]

df_iter$dat <-
  pmap(
    df_iter,
    function(data, family, ...) {
      
      dat <-
        data |> 
        select(
          n_cases, offset, time_ind, district
        ) |> 
        rename(
          Y = n_cases,
          offset = offset,
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
      
      # add likelihood setting
      dat$family <- family
      
      return(dat)
    }
  )

df_iter$dat[[1]] |> str()

df_iter$data[[4]] |> print(n=Inf)


## record fitted model names and paths ----------------------------

df_iter <-
  df_iter |> 
  mutate(
    model_name = g("stanfit_descriptive_{disease_print}"),
    fit_path = file.path(
      res_path, "R_output", "models", g("{model_name}.Rds")
    )
  ) 

df_iter$fit_path

## fit models ------------------------------

# compile model
smod <-
  stan_model(
    file = file.path("stan_models", "descriptive_time-series_choose-likelihood.stan")
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
  df_iter[8,],
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


## get trend predictions ------------------

# define object names
df_iter <-
  df_iter |> 
  mutate(
    pred_trend_path = file.path(
      res_path, "R_output", "generated-quantities",
      g("{model_name}_pred-trend.Rds")
    )
  )

df_iter$pred_trend_path

pwalk(
  df_iter,
  function(fit_path, pred_trend_path, data, dat, family, ...) {
    
    print(fit_path)
    
    # load fit
    fit <- readRDS(fit_path)
    
    # df to add predictions to
    pred_df <-
      bind_cols(
        i = data$i,
        case_rate_1e5 = data$case_rate_1e5,
        dat[c("T", "L")]
      )
    # get draws
    dr <- as_draws_rvars(fit) |> 
      map(as.matrix)
    
    # get predictions
    pred_df <-
      pred_df |> 
      mutate(
        # trend component (linear predictor scale)
        eta_x = pmap_vec(
          list(i, L, T), \(i, L, T) {
            (dr$x[T, L]) |> 
              as.vector()
          }
        ),
        # seasonal component (linear predictor scale)
        eta_s = pmap_vec(
          list(i, L, T), \(i, L, T) {
            (as_rvar(dat$sp_s)[i,] %**% dr$theta_s[, L]) |> 
              as.vector()
          }
        ),
        # trend: intercept + latent level (outcome scale)
        pred_trend = pmap_vec(
          list(i, L, T, eta_x), \(i, L, T, eta_x) {
            (dr$alpha[L] + eta_x) |> 
              as.vector()
          }
        ),
        # fit: predicted rate
        pred_fit = pmap_vec(
          list(i, L, T, eta_x, eta_s), \(i, L, T, eta_x, eta_s) {
            (dr$alpha[L] + eta_x + eta_s) |> 
              as.vector()
          }
        )
      )
    
    # if Normal variable, destandardize pred_trend and pred_fit
    if (family == 1) {
      pred_df$pred_trend <-
        destandardize(pred_df$pred_trend, org_vec = pred_df$E)
      
      pred_df$pred_fit <-
        destandardize(pred_df$pred_fit, org_vec = pred_df$E)
      
      
      # if Gamma or NegBin, apply inverse link function
    } else if (family %in% c(2,3)) {
      pred_df$pred_trend <-
        exp(pred_df$pred_trend)
      
      pred_df$pred_fit <-
        exp(pred_df$pred_fit)
    }
    
    # post-process to save as .Rds
    pred_df <- rvar_tib_postproc(pred_df)
    
    # save
    pred_df |>
      saveRDS(pred_trend_path)
  })


## get summary statistics of the trend -------------------

# define object names
df_iter <-
  df_iter |> 
  mutate(
    pred_trend_summ_path = file.path(
      res_path, "R_output", "generated-quantities",
      g("{model_name}_pred-trend-summ.Rds")
    )
  )

pwalk(
  df_iter,
  function(pred_trend_path, pred_trend_summ_path, data, ...) {
    
    print(pred_trend_path)
    
    # load fit
    pred_df <- readRDS(pred_trend_path)
    
    # add original data
    pred_df <-
      pred_df |> 
      left_join(data)
    
    
    # get trend at time start
    trend_start <-
      pred_df |> 
      group_by(district) |> 
      filter(date == min(date)) |> 
      select(district, pred_trend) |> 
      rename(trend_start = pred_trend)
    
    # get trend at time end
    trend_end <-
      pred_df |> 
      group_by(district) |> 
      filter(date == max(date)) |> 
      select(district, pred_trend) |> 
      rename(trend_end = pred_trend)
    # get trend change from start to end
    trend_change <-
      trend_start |> 
      left_join(trend_end) |> 
      summarise(
        trend_change = trend_end - trend_start
      )
    
    # get average annual trend change
    an_trend_change <-
      pred_df |> 
      group_by(district) |> 
      mutate(
        an_trend_change = slider::slide_vec(
          pred_trend,
          ~ last(.x) - first(.x),
          .before = 12,  # Include current + previous 12 for 13-step window
          .complete = TRUE,
          .i = time_ind
        )
      ) |> 
      # drop missings
      mutate(
        sel = mean(an_trend_change)
      ) |> 
      drop_na(sel) |> 
      # compute average
      select(district, an_trend_change) |> 
      summarise(
        an_trend_change = rvar_mean(an_trend_change)
      )
    
    # combine results
    res <-
      trend_start |> 
      left_join(trend_end) |> 
      left_join(trend_change) |> 
      left_join(an_trend_change) |> 
      ungroup()
    
    # post-process to save as .Rds
    res <- rvar_tib_postproc(res)
    
    # save
    res |>
      saveRDS(pred_trend_summ_path)
  })



## save analysis setup -------------------------------------------------------------------

df_iter |> 
  saveRDS(file.path(
    res_path, "R_output", "disease_descriptive-analysis_setup.Rds"
  ))
