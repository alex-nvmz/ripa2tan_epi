# It generates quantities for the association analyses between each environmental
# exposure and disease pair, and saves them in `/results/R_output/generated-quantities`

# setup
source("00_source.R")

# GAM - spline machinery
library(mgcv)

# Bayesian models
library(rstan)
library(posterior)
library(tidybayes)
library(ggdist)


# load analysis setup --------------------------------------------------------------------

df_iter <-
  readRDS(file.path(
    res_path, "R_output", "association-analysis_setup.Rds"
  ))

# note:
# posterior draws are memory expensive, and we have 230 models
# do not load model fits to memory
# generate quantities and save to .Rds

# exposure-response predictions ------------------------------

## get predictions ------------------

# define object names
df_iter <-
  df_iter |> 
  mutate(
    pred_expo_resp_path = file.path(
      res_path, "R_output", "generated-quantities",
      g("{model_name}_pred-expo-resp.Rds")
    )
  )

pwalk(
  df_iter,
  function(fit_path, pred_expo_resp_path, data, dat, exposure, ...) {
    
    print(fit_path)
    
    # load fit
    fit <- readRDS(fit_path)
    
    # target predictions setup
    pred_df <-
      data |>
      # different prediction range by location
      group_by(district) |>
      reframe(
        E = seq(min(E), max(E), length.out=50)
      ) |>
      # tidy up location
      mutate(
        district = as.numeric(district)
      ) |>
      rename(
        L = district
      ) |>
      # identifiers
      mutate(
        i = row_number()
      ) |> 
      relocate(i)
    
    
    # predictor matrix for the new exposures
    data_vec <- pred_df$E
    sp_E_pred <-
      PredictMat(
        dat$basis_E,
        data=tibble(data_vec)
      ) |> 
      as_rvar()
    
    # get draws
    dr <- as_draws_rvars(fit) |> 
      map(as.matrix)
    
    
    # get predictions
    pred_df <-
      pred_df |> 
      mutate(
        eta = pmap_vec(list(i, L), \(i, L) {
          exp(
            sp_E_pred[i,] %**% dr$theta_E[, L]
          ) |> 
            as.vector()
        })
      )
    
    # post-process to save as .Rds
    pred_df <- rvar_tib_postproc(pred_df)
    
    # save
    pred_df |> 
      saveRDS(pred_expo_resp_path)
  }
)

df_iter$pred_expo_resp_path[[1]]


## get counterfactual conditions -------------------

# pm2p5, total_rainfall, n_raindays: minimum exposure
# temperature, utci: exposure of minimum risk (determined with the least uncertainty)

# define object names
df_iter <-
  df_iter |> 
  mutate(
    cf_cond_path = file.path(
      res_path, "R_output", "generated-quantities",
      g("{model_name}_cf-cond.Rds")
    )
  )

pwalk(
  df_iter,
  function(pred_expo_resp_path, cf_cond_path, exposure, fit_path, ...) {
    print(fit_path)
    
    pred_expo_resp <- 
      readRDS(pred_expo_resp_path)
    
    if (exposure %in% c("pm2p5", "total_rainfall", "n_raindays")) {
      cf_cond <-
        pred_expo_resp |>
        mutate(eta = median(eta)) |> 
        group_by(L) |>
        filter(E == min(E)) |>
        ungroup() |> 
        select(E, L, eta)
      
    } else {
      cf_cond <-
        pred_expo_resp |>
        mutate(
          eta_sd = sd(eta),
          eta = median(eta),
          # sorting metric: median weighted by std. dev.
          sorting = eta * eta_sd
        ) |> 
        group_by(L) |>
        slice_min(sorting) |> 
        ungroup() |> 
        select(E, L, eta)
    }
    
    cf_cond |>
      saveRDS(cf_cond_path)
  }
)


# attributable fractions --------------------------------------------

## predict counterfactual IRs, AFs, ANs -------------

# define object names
df_iter <-
  df_iter |> 
  mutate(
    pred_cf_path = file.path(
      res_path, "R_output", "generated-quantities",
      g("{model_name}_pred-cf.Rds")
    )
  )

pwalk(
  df_iter,
  function(fit_path, cf_cond_path, pred_cf_path, dat, data, exposure, ...) {
    
    print(fit_path)
    
    cf_cond <- readRDS(cf_cond_path)
    
    # load fit
    fit <- readRDS(fit_path)
    
    # tidy counterfactual conditions
    cf_cond <-
      cf_cond |> 
      rename(E_cf = E) |> 
      select(- eta)
    
    
    # df to add predictions to
    pred_cf <-
      bind_cols(
        i = data$i,
        E = data$E,
        dat[c("D", "L")]
      ) |> 
      left_join(
        cf_cond, join_by(L)
      )
    
    # setup predictions at counterfactual exposure
    data_vec <- pred_cf$E_cf
    sp_E_cf <-
      PredictMat(
        dat$basis_E,
        data=tibble(data_vec)
      ) |> 
      as_rvar()
    
    # setup predictions at observed exposures
    # data as rvar
    rvar_vec <- c("D", "P", "sp_s", "sp_E")
    d <-
      dat |> 
      imap(\(x, i) {
        if (i %in% rvar_vec) {
          x |> unclass() |> as_rvar()
        } else {
          x
        }
      })
    
    # get draws
    dr <- as_draws_rvars(fit) |>
      map(as.matrix)
    
    # get predictions
    pred_cf <-
      pred_cf |> 
      mutate(
        # observed
        eta = pmap_vec(list(i), \(i) {
          exp(
            dr$alpha[d$L[i]] +
              dr$x[d$T[i], d$L[i]] +
              d$sp_s[i,] %**% dr$theta_s[, d$L[i]] +
              d$sp_E[i,] %**% dr$theta_E[, d$L[i]]
          ) |> 
            as.vector()
        }),
        # counterfactual
        eta_cf = pmap_vec(list(i), \(i) {
          exp(
            dr$alpha[d$L[i]] +
              dr$x[d$T[i], d$L[i]] +
              d$sp_s[i,] %**% dr$theta_s[, d$L[i]] +
              sp_E_cf[i,] %**% dr$theta_E[, d$L[i]]
          ) |> 
            as.vector()
        })
      )
    
    
    # compute AF and AN
    pred_cf <-
      pred_cf |>
      mutate(
        AF = (eta - eta_cf)/eta,
        AN = AF * D
      )
    
    # post-process to save as .Rds
    pred_cf <- rvar_tib_postproc(pred_cf)
    
    # save
    pred_cf |> 
      saveRDS(pred_cf_path)
  }
)

## get aggregated AN and AF --------------------

# define object names
df_iter <-
  df_iter |> 
  mutate(
    pred_cf_agg_path = file.path(
      res_path, "R_output", "generated-quantities",
      g("{model_name}_pred-cf-agg.Rds")
    )
  )

pwalk(
  df_iter,
  function(pred_cf_path, pred_cf_agg_path, ...) {
    
    print(pred_cf_path)
    
    pred_cf <- readRDS(pred_cf_path)
    
    pred_cf_agg <-
      pred_cf |>
      group_by(L) |> 
      summarise(
        D = sum(D),
        AN = rvar_sum(AN)
      ) |> 
      mutate(
        AF = AN / D
      ) |> 
      ungroup()
    
    # post-process to save as .Rds
    pred_cf_agg <- rvar_tib_postproc(pred_cf_agg)
    
    # save
    pred_cf_agg |>
      saveRDS(pred_cf_agg_path)
  }
)


df_iter$pred_cf_path[[1]]
df_iter$pred_cf_agg_path[[1]]

# prediction explanation -----------------------------------------------------------------

# define object names
df_iter <-
  df_iter |> 
  mutate(
    pred_explain_path = file.path(
      res_path, "R_output", "generated-quantities",
      g("{model_name}_pred-explain.Rds")
    )
  )

pwalk(
  df_iter,
  function(dat, data, fit_path, pred_explain_path, ...) {
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
        # exposure component (linear predictor scale)
        eta_E = pmap_vec(
          list(i, L, T), \(i, L, T) {
            (as_rvar(dat$sp_E)[i,] %**% dr$theta_E[, L]) |> 
              as.vector()
          }
        ),
        # fit: predicted rate (outcome scale)
        pred_fit = pmap_vec(
          list(i, L, T, eta_x, eta_s, eta_E), \(i, L, T, eta_x, eta_s, eta_E) {
            (dr$alpha[L] + eta_x + eta_s + eta_E) |> 
              exp() |> 
              as.vector()
          }
        )
      )
    
    # post-process to save as .Rds
    pred_df <- rvar_tib_postproc(pred_df)
    
    # save
    pred_df |>
      saveRDS(pred_explain_path)
    
  })



# save analysis setup with additional variables ------------------------------------------

df_iter |> 
  saveRDS(file.path(
    res_path, "R_output", "association-analysis_setup_gen-quant.Rds"
  ))
