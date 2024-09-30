#' ---
#' title: "03: Regression"
#' execute: 
#'   warning: false
#' toc: true
#' toc-depth: 3
#' toc-expand: true
#' fig-width: 16
#' fig-asp: 0.618  # golden ratio
#' format:
#'   html:
#'     grid:
#'       body-width: 950px
#'     embed-resources: true
#' ---
#' 
#' Here we will do regression models of the disease incidence rates against each one of the environmental exposures, separately. We will consider the possibility of doing models with more than one environmental exposure. The type of models applied will be penalized distributed-lag non-linear models (DLNMs).
#' 
#' # Setup
#' 
## ---------------------------------------------------------------------------------------
library(tidyverse)
library(scales)     # plot utilities
library(patchwork)  # arrange plots
library(glue)       # string interpolation
library(gt)         # make tables

library(mgcv)       # GAMs
library(marginaleffects)  # model interpretation

#' 
## ---------------------------------------------------------------------------------------
# ggplot2 options
options(
  # Set colourblind-friendly palettes for ggplot
  ggplot2.discrete.colour = ggokabeito::palette_okabe_ito(),
  ggplot2.discrete.fill = ggokabeito::palette_okabe_ito(),
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
theme_set(theme_bw(base_size = 16))

#' 
#' 
## ---------------------------------------------------------------------------------------
base_path <- ".."
data_path <- file.path(base_path, "data")
res_path <- file.path(base_path, "results")


# data_path <- "local_data_path"

#' 
#' # Prepare data
#' 
## ---------------------------------------------------------------------------------------
pop <- read_csv(file.path(data_path, "processed", "population.csv"))
pop

df_disease <- read_csv(file.path(data_path, "processed", "disease_processed.csv"))
df_disease


df_environ <- read_csv(file.path(data_path, "processed", "environmental_imputed.csv"))
df_environ

# Merge datasets, create variables for regression, split by disease ----------------------

# Add population
df_reg <- df_disease |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop, year=year(date)),
    join_by(district, year)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

# Add environmental variables
df_reg <- df_reg |> 
  left_join(df_environ, join_by(district, date))

# Filter for dates between the years 2014 and 2021 (complete data for diseases and exposures)
df_reg <- df_reg |> 
  filter(year(date) >= 2014 & year(date) <= 2021)

# Create variables for regression
df_reg <- df_reg |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )

# Split dataset by disease
# df_reg <- df_reg |> 
#   group_by(disease, disease_group, disease_communicable) |> 
#   nest(.key="df_model")

# df_reg[[1, "df_model"]][[1]] |> 
#   print(width=Inf, n=20)


#' 
#' # Univariate regressions
#' 
## ---------------------------------------------------------------------------------------
# Split dataset by disease and environmental exposure
df_reg_uni <- df_reg |> 
  pivot_longer(
    cols=pm2p5:utci,
    names_to = "regressor",
    values_to = "x"
  ) |> 
  group_by(disease, disease_group, disease_communicable, regressor) |> 
  nest(.key="df_model")

df_reg_uni[1,]

df_reg_uni[[1, "df_model"]][[1]] |>
  print(width=Inf, n=20)


df_reg

# k_trend is 8 for all (number of basis functions for trend)
df_reg |> 
  filter(! is.na(n_cases)) |>
  pull(date) |>
  year() |>
  range() |>
  diff()+1

disease <- df_reg_uni[[1,"disease"]]
regressor <- df_reg_uni[[1,"regressor"]]
df_model <- df_reg_uni[[1,"df_model"]][[1]]

library(dlnm)

mod <- pmap(
  df_reg_uni[1,],
  function(disease, regressor, df_model, ...) {
    
    print(c(disease, regressor))
    
    # Formula
    form <- n_cases ~ 0 + district +
      te(time_ind, month_ind, bs=c("cr","cc"), k=c(8,12), by=district)
    
    print(form)
    
    
    Q <- tsModel::Lag (df_model$x[1:50],0:25)
    dim(Q)
    L <- matrix(0:25,nrow(Q),ncol(Q),byrow=TRUE)
    dim(L)
    dlnm::crossbasis(
      
    )
    
    # Start and end points of monthly cyclic splines
    # note: Slightly out of c(1,12) to not impose that January (1) is equal to December (12)
    knots <- list(month_ind = c(0.5, 12.5))
    
    gam(
      form,
      # Include the offset here instead of in the formula, so that it is not used in the predictions
      # Divide offset by 1e5 so that predictions are incidence rates per 1e5 people
      offset = log(population/1e5),
      data=df_model,
      family = mgcv::nb(link="log"),
      method = "REML",
      knots = knots
    )
  }
)

mod <- mod[[1]]
summary(mod)

gratia::draw(mod)
gratia::appraise(mod)

# ggplot() +
#   geom_line(
#     data=df_reg_uni[[1, "df_model"]][[1]],
#     aes(x=time_ind, y=n_cases/population*1e5, color=district)
#   ) +
#   geom_line(
#     data=cbind(
#       mod$model,
#       pred = predict(mod, type="response")
#     ),
#     aes(x=time_ind, y=pred, color=district),
#     linewidth=2
#   )



#' 
#' 
#' - Finish reading DLNM paper, by the part where they introduce the C matrix
#' 
#' W is composed of cross-basis variables (we can call them this)
#' 
#' - Then read penalized GAM paper
#' 
#' - Check GAM book, see what behaviour is for te() when you input matrices instead of column vectors
#' 
#' summation convention
#' 
#' - Gasparrini penalized GAM basis (2017 paper), see if it is worth it using his code
#' (or better to rely on raw mgcv and marginaleffects)
#' 
#' Example of Gasparrini code: https://github.com/gasparrini/2017_gasparrini_Biomet_Rcodedata/blob/master/example1/ex1_01.int.R
#' 
#' https://cran.r-project.org/web/packages/dlnm/dlnm.pdf
#' 
#' This guy's blog for perspective
#' 
#' https://ericrscott.com/posts/2021-01-13-dlnm-getting-started/
#' 
#' https://ericrscott.com/posts/2021-01-18-dlnm-basis/
#' 
#' https://ericrscott.com/posts/2021-02-08-tensor-product-dlnm/
#' 
#' - Review DLNM vignettes
#' 
#' https://cran.r-project.org/web/packages/dlnm/vignettes/dlnmOverview.pdf
#' 
#' 
#' - R Lowe explanation: https://www.thelancet.com/cms/10.1016/S2542-5196(20)30292-8/attachment/0e42b9dd-9979-473d-ac22-ce5906a28415/mmc2.pdf
#' 
#' - Learn what I did in the following code:
#' 
## ---------------------------------------------------------------------------------------
#| eval: false

## # Dataframe to store imputed data
## df_environ_impute <- df_environ
## 
## df_environ_impute_moshi <- df_environ_impute |>
##   filter(district == "Moshi") |>
##   arrange(date)
## 
## df_environ_impute_siha <- df_environ_impute |>
##   filter(district == "Siha") |>
##   arrange(date)
## 
## interval(min(date), date) / months(1) + 1
## 
## library(mgcv)
## library(gratia)
## library(marginaleffects)
## df_environ_impute_siha |>
##   ggplot() +
##   geom_line(
##     aes(x=date, y=n_raindays)
##   )
## 
## df_environ_impute_siha <- df_environ_impute_siha |>
##   mutate(
##     time_ind = interval(min(date), date) / months(1) + 1,
##     month_ind = month(date)
##   )
## 
## df_environ_impute_siha |>
##   print(n=Inf)
## 
## 
## 
## # Check dlnm::crossbasis() and mgcv::tensor.prod.model.matrix(X) are the same
## 
## library(dlnm)
## X <- list(matrix(0:3,2,2),matrix(c(5:8,0,0),2,3))
## X
## tensor.prod.model.matrix(X)
## X[[1]]%.%X[[2]]
## 
## dlnm::crossbasis(t(X[[1]]%.%X[[2]]))
## 
## 
## vec <- rpois(10, 10)
## 
## tsModel::Lag(vec, 3, factor(rep(c(1,2), length.out=length(vec))))
## 
## asdf <- tsModel::Lag(vec, 0:2)
## 
## library(splines)
## asdf%.%asdf |>
##   ns(df=3)
## 
## ns(asdf, df=3)%.%ns(asdf, df=3)
## 
## crossbasis(
##   asdf,
##   argvar = list(fun="ns", knots = 3),
##   arglag = list(fun="ns", knots = 1)
## )
## 
## cb <- crossbasis(
##   df_environ_impute_siha$pm2p5,
##   lag = 3,
##   argvar = list(fun="ns", knots = 3),
##   arglag = list(fun="ns", knots = 1)
## )
## 
## mod_cb <- gam(n_raindays ~ cb,
##     data=df_environ_impute_siha,
##     family = poisson()
##     )
## 
## mod_cb
## 
## model.matrix(mod_cb)
## 
## # tsModel::Lag(df_environ_impute_siha$pm2p5, 0:3)
## 
## df_environ_impute_siha_alt <- df_environ_impute_siha |>
##   mutate(
##     pm2p5_l1 = lag(pm2p5, 1),
##     pm2p5_l2 = lag(pm2p5, 2),
##     pm2p5_l3 = lag(pm2p5, 3),
##     pm2p5_l4 = lag(pm2p5, 4)
##     ) |>
##   rename(pm2p5_l0 = pm2p5) |>
##   pivot_longer(
##     cols = starts_with("pm2p5"),
##     values_to = "pm2p5",
##     names_to = "lag",
##     names_prefix = "pm2p5_l"
##   ) |>
##   mutate(lag = as.numeric(lag))
## 
## df_environ_impute_siha_alt |>
##   print(width=Inf, n=20)
## 
## mod_te <- gam(n_raindays ~ te(pm2p5, lag, bs=c("cr", "cr"), k=c(10,4)),
##     data=df_environ_impute_siha_alt,
##     family = poisson()
##     )
## 
## mod_te_cb <- gam(n_raindays ~ s(pm2p5, lag, bs="cb", k=c(10,4),
##                                 xt = list(bs = "cr")),
##     data=df_environ_impute_siha_alt,
##     family = poisson()
##     )
## 
## mod_te |> draw()
## mod_te_cb |> draw()
## mod_cb |> draw()
## 
## 
## model.matrix(mod_te) |> dim()
## model.matrix(mod_te_cb) |> dim()
## model.matrix(mod_cb) |> dim()
## 
## model.matrix(mod_te_cb)[,2]
## model.matrix(mod_cb)[,2]
## 
## # # DEFINE MATRICES TO BE INCLUDED AS TERMS IN THE SMOOTHER
## 
## df_environ_impute_siha_alt |>
##   select(pm2p5, lag, time_ind) |>
##   pivot_wider(
##     names_from = lag,
##     values_from = pm2p5
##   ) |>
##   print(n=20)
## 
## lag_max <- 4
## Q <- tsModel::Lag(df_environ_impute_siha$pm2p5,0:lag_max)
## L <- matrix(0:lag_max,nrow(Q),ncol(Q),byrow=TRUE)
## 
## 
## mod_cb <- gam(n_raindays ~ s(Q, L, bs="cb", k=c(10,4),
##                              xt = list(bs = "cr")),
##     data=df_environ_impute_siha,
##     family = poisson()
##     )
## 
## mod_cb |> draw()
## 
## mod_cb_te <- gam(n_raindays ~ te(Q, L, bs="cr", k=c(10,4)),
##     data=df_environ_impute_siha,
##     family = poisson()
##     )
## 
## mod_cb_te |> draw()
## model.matrix(mod_cb_te) |> dim()
## 
## model.matrix(mod_cb)
## 
## 
## 
## cb <- crossbasis(
##   Q,
##   argvar = list(fun="bs", knots = 10),
##   arglag = list(fun="bs", knots = 4)
## )
## 
## 
## mod_te |> draw()
## 
## fdsa <- splines::ns(df_environ_impute_siha$time_ind, df=6)
## 
## mod1 <- gam(n_raindays ~ cb,
##     data=df_environ_impute_siha,
##     family = poisson()
##     )
## 
## mod1 |> draw()
## 
## plot_predictions(mod1, type="response", by=c("cb"),
##                  # points=1
##                  )
## 
## plot_predictions(mod1, type="response", condition=c("cb"),
##                  # points=1
##                  )
## 
## 
## mod <- gam(n_raindays ~ s(time_ind, bs = "bs") + s(month_ind, bs="cc"),
##     data=df_environ_impute_siha,
##     family = poisson()
##     )
## 
## mod <- gam(n_raindays ~ te(month_ind, time_ind, bs=c("cc", "tp")),
##     data=df_environ_impute_siha,
##     family = poisson()
##     )
## 
## 
## 
## cp <- crosspred("Q",mod_cb, cen=10)
## 
## cp |> plot()
## 
## 
## 
## #
## # # RUN THE GAM MODEL AND PREDICT (TAKES ~17sec IN A 2.4 GHz PC)
## # # SET 'cb' SMOOTHER WITH DIMENSION 10 FOR EACH SPACE (MINUS CONSTRAINTS)
## # system.time({
## # gam1 <- gam(death~s(Q,L,bs="cb",k=10)+ns(time,10*14)+dow,family=quasipoisson(),
## #   london,method='REML')
## # })
## # pred3dgam1 <- crosspred("Q",gam1,at=-3:29,cen=20)
## # predslgam1 <- crosspred("Q",gam1,by=0.2,bylag=0.2,cen=20)
## 
## mod1 |> summary()
## mod |> summary()
## 
## mod |> draw()
## 
## predict(mod, type="response")
## 
## plot_predictions(mod, condition="time_ind", type="response",
##                  # points=1
##                  )
## 
## plot_predictions(mod, condition=c("time_ind", "month_ind"), type="link",
##                  points=1
##                  )
## 
## 
## plot_predictions(mod, type="response", by=c("time_ind"),
##                  points=1
##                  )
## 
## plot_predictions(mod, type="response", by=c("time_ind"), newdata=df_environ_impute_siha,
##                  points=1
##                  )
## 
## 
## plot_predictions(mod, type="response", by=c("month_ind"),
##                  points=1
##                  )
## 
## 
## 

