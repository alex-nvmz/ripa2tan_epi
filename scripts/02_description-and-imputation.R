#' ---
#' title: "02: Description and imputation"
#' execute: 
#'   warning: false
#'   cache: true
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
#' # knitr:
#' #   opts_chunk:
#' #     out.width: "100%"
#' ---
#' 
#' Here we will explore our variables, and impute the datasets if necessary.
#' 
#' # Setup
#' 
## ---------------------------------------------------------------------------------------
library(tidyverse)
theme_set(theme_bw(base_size = 16))

library(scales)     # plot utilities
library(ggokabeito) # colorblind palette
library(ggh4x)      # facet_nested
library(patchwork)  # arrange plots
library(glue)       # string interpolation
library(gt)         # make tables

library(mgcv)       # GAMs
library(gratia)     # utility functions for GAMs
library(marginaleffects)  # model interpretation

#' 
#' 
## ---------------------------------------------------------------------------------------
base_path <- ".."
data_path <- file.path(base_path, "data")
res_path <- file.path(base_path, "results")


# data_path <- "local_data_path"

#' 
#' 
#' # Population
#' 
## ---------------------------------------------------------------------------------------
# Load
pop <- read_csv(file.path(data_path, "processed", "population.csv"))


# Visualize population
pop |>
  ggplot() +
  geom_vline(
    aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
  ) +
  geom_line(
    aes(x=date, y=population, color=district), linewidth=0.8
  ) +
  geom_point(
    aes(x=date, y=population, color=district), size=3
  ) +
  scale_y_continuous(labels = scales::comma, n.breaks = 6) +
  scale_x_date(
    date_breaks="year", date_labels = "%Y",
    # limits = date_range_plot
  ) +
  scale_color_okabe_ito() +
  theme_bw(base_size = 18) +
  labs(
    color = "District",
    x = "Year",
    y = "Population"
  )

ggsave(
  file.path(res_path, "plots", "population.png"),
  width=16, height=16*0.600
)

#' 
#' Population has been increasing linearly year by year. Imputing the population of 2022 with a linear prediction seems appropriate.
#' 
## ---------------------------------------------------------------------------------------
# Get time index for regression
pop_reg <- pop |> 
  mutate(year=year(date))

# Fit
fit <- lm(population ~ year*district, data=pop_reg)
summary(fit)

# Predict
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
pop_imp <- pop_reg |> 
  full_join(newdata) |> 
  select(- year) |> 
  arrange(district, date)


# Check imputation
pop_imp |>
  left_join(mutate(pop, population_orig = population)) |> 
  mutate(
    imputed = if_else(is.na(population_orig), "Yes", "No")
  ) |> 
  ggplot() +
  geom_vline(
    aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
  ) +
  geom_line(
    aes(x=date, y=population, color=district), linewidth=0.8
  ) +
  geom_point(
    aes(x=date, y=population, color=district, shape=imputed, size=imputed),
  ) +
  scale_shape_manual(
    values = c(16,4)
  ) +
  scale_size_manual(
    values = c(3,8)
  ) +
  scale_y_continuous(labels = scales::comma, n.breaks = 6) +
  scale_x_date(
    date_breaks="year", date_labels = "%Y",
  ) +
  scale_color_okabe_ito() +
  theme_bw(base_size = 18) +
  labs(
    color = "District",
    x = "Year",
    y = "Population",
    shape = "Imputed",
    size = "Imputed"
  )

ggsave(
  file.path(res_path, "plots", "population_imputed.png"),
  width=16, height=16*0.600
)

# Save
write_csv(
  pop_imp,
  file.path(data_path, "processed", "population_imputed.csv")
)


rm(fit, newdata, pop_reg)

#' 
#' 
#' # Environment
#' 
#' TODO: visualize splines
#' 
#' We will fit GAMs to decompose the environmental time series into trend and seasonal components. These models will be used to impute observations in the year range between 2014-2021, to make possible the posterior regression analyses.
#' 
#' 
## ---------------------------------------------------------------------------------------
df_environ <- read_csv(file.path(data_path, "processed", "environmental.csv"))

# Prepare data for modelling
df_environ_model <- df_environ |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )

df_environ_model |> 
  print(width=Inf, n=13)

#' 
#' 
## ---------------------------------------------------------------------------------------
df_environ_model

# Create dataframe for analysis
# Add necesary parameters to do analysis for each environmental variable

df_environ_an <- tribble(
  ~var, ~label, ~ units,
  "pm2p5", "PM2.5", "(\U03BCg/m\U00B3)",
  "greenness", "Greenness", "(NDVI)",
  "temp_min", "Min. temp.", "(ºC)",
  "temp_mean", "Mean temp.", "(ºC)",
  "temp_max", "Max. temp.", "(ºC)",
  "utci", "UTCI", "",
  "total_rainfall", "Rainfall", "(mm)",
  "n_raindays", "No. rain days", ""
)

df_environ_an

#' 
#' ## Plot the response
#' 
## ---------------------------------------------------------------------------------------

# Step 1: Plot the response --------------------------------------------------------------

# Get range for plots
get_range <- function(var) {
  df_environ_model |> 
    filter(!is.na( .data[[var]] )) |> 
    pull(date) |> 
    range()
}

df_environ_an <- df_environ_an |> 
  mutate(
    date_range_plot = map(var, get_range)
  )

df_environ_an[["date_range_plot"]]


# Create plots
plots <- pmap(
  df_environ_an,
  function(var, label, units, date_range_plot, ...) {
    df_environ_model |>
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date, y=.data[[var]], color=district), linewidth=0.8
      ) +
      geom_point(
        aes(x=date, y=.data[[var]], color=district), size=2.5
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y",
        limits = date_range_plot
      ) +
      scale_color_okabe_ito() +
      theme_bw(base_size = 18) +
      labs(
        color = "District",
        x = "Date (month)",
        y = glue("{label} {units}")
      )
  }
)

paths <- file.path(res_path, "plots", "environmental_descriptive",
                   glue("env_{df_environ_an$var}_response.png"))
# Save plots
walk2(
  plots,
  paths,
  \(plot, path) ggsave(
    filename=path, plot=plot,
    width=16, height=16*0.600
  )
)

#' 
#' ## Fit model
#' 
## ---------------------------------------------------------------------------------------
# Step 2: Fit the model ----------------------------------------------------------------

# Penalized GAM
# Contains penalized smooth functions of time and month
# Function of time (trend) is cubic spline with 1 basis function per year
# Function of month (seasonal component) is cyclic cubic splines with 12 basis functions
# We allow the seasonal and trend components to interact, so the seasonal component can
# change over time. The interaction is computed with a tensor product of the two functions

df_environ_an

# Get number of basis functions for trend
get_k_trend <- function(var) {
  df_environ_model |> 
    filter(! is.na(.data[[var]])) |>
    pull(date) |>
    year() |>
    range() |>
    diff()+1
}

df_environ_an <- df_environ_an |> 
  mutate(
    k_trend = map_dbl(var, get_k_trend)
  )

df_environ_an[["k_trend"]]

# Add distribution family of the response
df_environ_an <- df_environ_an |> 
  mutate(
    family = c(
      replicate(6, gaussian(), simplify=FALSE),
      list(
        mgcv::tw(link="log"),
        mgcv::nb(link="log")
      )
    )
  )
df_environ_an[["family"]]

# Fit models
fit_descriptive_gam <- function(var, k_trend, family, ...) {
  # Formula
  form <- as.formula(glue("{var} ~ 0"))
  form <- update(
    form,
    . ~ 0 + district +
      ti(time_ind, bs="cr", k=k_trend, by=district) +
      ti(month_ind, bs="cc", k=12, by=district) +
      ti(time_ind, month_ind, bs=c("cr", "cc"), k=c(k_trend,12), by=district)
  )
  
  print(form)
  
  # Start and end points of monthly cyclic splines
  # note: Slightly out of c(1,12) to not impose that January (1) is equal to December (12)
  knots <- list(month_ind = c(0.5, 12.5))
  
  gam(
    form,
    data=df_environ_model,
    family = family,
    method = "REML",
    knots = knots
  )
}

df_environ_an <- df_environ_an |> 
  mutate(
    model = pmap(list(var, k_trend, family), fit_descriptive_gam)
  )

#' 
#' ## Check model
#' 
## ---------------------------------------------------------------------------------------
# Step 3: Check the model --------------------------------------------------------------

pwalk(
  df_environ_an,
  function(var, model, ...) {
    cat(glue(
      "\n
      ---------------------------------------
      Printing model diagnostics of variable:
                       {var}
      ----------------------------------------
      \n"
    ))
    
    print(summary(model))
    
    # Residual diagnostics
    p_diag <- gratia::appraise(model)
    # Save plot
    ggsave(
      plot=p_diag,
      filename = file.path(
        res_path, "plots", "environmental_descriptive",
        glue("env_{var}_gam_diagnostics.png")
      ),
      width=16, height=16*0.618
    )
    
    # Optionally: check model component predictions
    # gratia::draw(model) |> print()
  }
)

#' 
#' ## Get predictions
#' 
## ---------------------------------------------------------------------------------------
# Step 4: Get predictions, as well as decomposition into trend, seasonal and residual ----

# Get predictions and time series decomposition
get_pred_ts_decomp <- function(model, model_df=df_environ_model, var, ...) {
  # note: Standard errors computed with Delta method (marginaleffects default)
  
  # Dataframe of predictions
  pred_df <- model_df
  
  # Select columns of interest
  pred_df <- pred_df |> 
    select(district, date, all_of(var), time_ind)
  
  # Get predictions for the response
  pred_name <- "pred"
  pred_df_sub <- predictions(
    model,
    newdata = model_df,
    type = "response"
  ) |> 
    as_tibble() |> 
    select(time_ind, district, estimate, conf.low, conf.high) |> 
    rename(
      "{pred_name}" := estimate,
      "{pred_name}_conf.low" := conf.low,
      "{pred_name}_conf.high" := conf.high,
    )
  pred_df <- pred_df |> 
    left_join(pred_df_sub)
  
  # Predictions for trend
  pred_name <- "trend"
  exclude_set <- c(
    as.character(glue("ti(month_ind):district{levels(pred_df$district)}")),
    as.character(glue("ti(time_ind,month_ind):district{levels(pred_df$district)}"))
  )
  pred_df_sub <- predictions(
    model,
    newdata = model_df,
    exclude = exclude_set,
    type = "response"
  ) |> 
    as_tibble() |> 
    select(time_ind, district, estimate, conf.low, conf.high) |> 
    rename(
      "{pred_name}" := estimate,
      "{pred_name}_conf.low" := conf.low,
      "{pred_name}_conf.high" := conf.high,
    )
  pred_df <- pred_df |> 
    left_join(pred_df_sub)
  
  # Predictions for seasonality
  pred_name <- "season"
  exclude_set <- c(
    "district",
    as.character(glue("ti(time_ind):district{levels(pred_df$district)}"))
  )
  pred_df_sub <- predictions(
    model,
    newdata = model_df,
    exclude = exclude_set,
    type = "response",
  ) |> 
    as_tibble() |> 
    select(time_ind, district, estimate, conf.low, conf.high) |> 
    rename(
      "{pred_name}" := estimate,
      "{pred_name}_conf.low" := conf.low,
      "{pred_name}_conf.high" := conf.high,
    )
  pred_df <- pred_df |> 
    left_join(pred_df_sub)
  
  # Add response residuals
  pred_df <- pred_df |> 
    left_join(cbind(model$model, residuals = residuals(model, type="response")))
  
  return(pred_df)
}

df_environ_an <- df_environ_an |> 
  mutate(
    pred_df = pmap(list(model=model, var=var), get_pred_ts_decomp)
  )

#' 
#' ## Plot time series decomposition
#' 
## ---------------------------------------------------------------------------------------
# Step 5: Plot time series decomposition -----------------------------------------------

# df_environ_an$pred_df[[1]] |> colnames()

pwalk(
  df_environ_an,
  function(pred_df, var, date_range_plot, label, units, ...) {
    print(var)
    
    # Reference bar for y scale
    barlength <- select(pred_df, c(all_of(var), pred:season_conf.high)) |> 
      sapply(\(x) diff(range(x, na.rm=TRUE))) |>
      min()
    
    # Plot fitted line
    p_fit <- pred_df |> 
      filter(date >= date_range_plot[1] & date <= date_range_plot[2]) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_point(
        aes(x=date, y=.data[[var]], color=district), size=2.5
      ) + 
      geom_line(
        aes(x=date, y=pred, color=district), linewidth=0.8
      ) +
      geom_ribbon(
        aes(x=date, ymin=pred_conf.low, ymax=pred_conf.high, fill=district),
        alpha=0.2
      ) +
      geom_rect(
        aes(
          xmin=max(date_range_plot)+60, xmax=max(date_range_plot)+90,
          ymin=mean(.data[[var]], na.rm=TRUE) - (barlength/2),
          ymax=mean(.data[[var]], na.rm=TRUE) + (barlength/2)
        ),
        color="black", fill="gray75"
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      scale_color_okabe_ito() +
      scale_fill_okabe_ito() +
      theme_bw(base_size = 18) +
      labs(
        color = "District",
        fill = "District",
        x = "Date (month)",
        y = glue("{label} {units}")
      )
    
    # Plot trend
    p_trend <- pred_df |> 
      filter(date >= date_range_plot[1] & date <= date_range_plot[2]) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date, y=trend, color=district), linewidth=0.8
      ) +
      geom_ribbon(
        aes(x=date, ymin=trend_conf.low, ymax=trend_conf.high, fill=district),
        alpha=0.2
      ) +
      geom_rect(
        aes(
          xmin=max(date_range_plot)+60, xmax=max(date_range_plot)+90,
          ymin=mean(trend, na.rm=TRUE) - (barlength/2),
          ymax=mean(trend, na.rm=TRUE) + (barlength/2)
        ),
        color="black", fill="gray75"
      ) +
      scale_x_date(
        date_breaks = "year", date_labels = "%Y"
      ) +
      scale_color_okabe_ito() +
      scale_fill_okabe_ito() +
      theme_bw(base_size = 18) +
      labs(
        color = "District",
        fill = "District",
        x = "Date (month)",
        y = "Trend"
      )
    
    # Plot seasonal component
    p_season <- pred_df |> 
      filter(date >= date_range_plot[1] & date <= date_range_plot[2]) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date, y=season, color=district), linewidth=0.8
      ) +
      geom_ribbon(
        aes(x=date, ymin=season_conf.low, ymax=season_conf.high, fill=district),
        alpha=0.2
      ) +
      geom_rect(
        aes(
          xmin=max(date_range_plot)+60, xmax=max(date_range_plot)+90,
          ymin=mean(season, na.rm=TRUE) - (barlength/2),
          ymax=mean(season, na.rm=TRUE) + (barlength/2)
        ),
        color="black", fill="gray75"
      ) +
      scale_x_date(
        date_breaks = "year", date_labels = "%Y"
      ) +
      scale_color_okabe_ito() +
      scale_fill_okabe_ito() +
      theme_bw(base_size = 18) +
      labs(
        color = "District",
        fill = "District",
        x = "Date (month)",
        y = "Seasonality"
      )
    
    # Plot residuals
    # p_resid <- pred_df |>
    #   filter(date >= date_range_plot[1] & date <= date_range_plot[2]) |> 
    #   ggplot() +
    #   geom_vline(
    #     aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    #   ) +
    #   geom_vline(
    #     aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    #   ) +
    #   geom_segment(
    #     aes(x=date, y=0, yend=residuals, color=district), linewidth= 1.5, lineend = "butt",
    #     position = position_dodge(width=20)
    #   ) +
    #   geom_hline(yintercept = 0) +
    #   ylab("residuals") +
    #   geom_rect(
    #     aes(
    #       xmin=max(date_range_plot)+60, xmax=max(date_range_plot)+90,
    #       ymin=mean(residuals, na.rm=TRUE) - (barlength/2),
    #       ymax=mean(residuals, na.rm=TRUE) + (barlength/2)
    #     ),
    #     color="black", fill="gray75"
    #   ) +
    #   scale_x_date(
    #     date_breaks = "year", date_labels = "%Y"
    #   ) +
    #   scale_color_okabe_ito() +
    #   scale_fill_okabe_ito() +
    #   theme_bw(base_size = 18) +
    #   labs(
    #     color = "District",
    #     x = "Date (month)",
    #     y = "Residuals"
    #   )
    
    # Merge plots
    p_joint <- p_fit / (p_trend + theme(legend.position = "none")) /
      (p_season + theme(legend.position = "none"))  +
      # (p_resid + theme(legend.position = "none")) +
      plot_layout(axes = "collect", guides="collect",
                  heights = c(4,3,3))
    
    # Save plot
    ggsave(
      plot=p_joint,
      filename=file.path(
        res_path, "plots", "environmental_descriptive",
        glue("env_{var}_gam_decomposition.png")
      ),
      width=16, height=16*0.700
    )
  }
)

#' 
#' 
#' ## Get imputations
#' 
## ---------------------------------------------------------------------------------------
# Step 6: Get imputations needed for the regression model --------------------------------
# Full observations in year range 2014-2022


# Dataframe to store imputations
df_environ_imp <- df_environ

# Dataframe with the predictions
df_environ_pred <- df_environ_an |> 
  unnest(pred_df) |> 
  select(var, district, date, pred) |> 
  pivot_wider(
    names_from = var,
    values_from = pred
  )


# Make sure both dataframes are in the same order and are of the same dimensions
df_environ_imp <- df_environ_imp |> 
  arrange(district, date)

df_environ_pred <- df_environ_pred |> 
  arrange(district, date)

dim(df_environ_imp)
dim(df_environ_pred)

# Replace NAs with predictions -----.
# For each variable
for (var in df_environ_an$var) {
  print(var)
  
  # Get indices where NA
  na_ind <- which(is.na(df_environ_imp[[var]]))
  # Substitute predictions
  df_environ_imp[na_ind, var] <- df_environ_pred[na_ind, var]
}

# Fix n_raindays so that it is integer
df_environ_imp <- mutate(df_environ_imp, n_raindays = round(n_raindays))

# Filter imputations in year range 2012-2021 (cut out 2022)
df_environ_imp <- filter(df_environ_imp, year(date) <= 2021)

# Plot imputations -----------------------------------------------------------------------

# Prepare df for plotting
df_environ_imp_plot <- df_environ_imp |> 
  left_join(df_environ, join_by(district, date), suffix=c(".imp", ".org"))

for (i in seq_len(nrow(df_environ_an))) {
  var <- df_environ_an[["var"]][i]
  
  # If no NA, skip
  vec <- df_environ_imp_plot[[glue("{var}.org")]]
  if(!any(is.na(vec))) next
  
  print(var)
  # Else, plot imputation
  df_environ_imp_plot |>
    mutate(
      imputed = if_else(is.na(.data[[glue("{var}.org")]]), "Yes", "No")
    ) |> 
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_line(
      aes(x=date, y=.data[[glue("{var}.imp")]], color=district), linewidth=0.8
    ) +
    geom_point(
      aes(x=date, y=.data[[glue("{var}.imp")]], color=district, shape=imputed, size=imputed),
    ) +
    scale_shape_manual(
      values = c(16,4)
    ) +
    scale_size_manual(
      values = c(2.5,8)
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y",
    ) +
    scale_color_okabe_ito() +
    theme_bw(base_size = 18) +
    labs(
      color = "District",
      x = "Date (month)",
      y = glue("{df_environ_an[['label']][i]} {df_environ_an[['units']][i]}"),
      shape = "Imputed",
      size = "Imputed"
    )
  
  ggsave(
    file.path(res_path, "plots", "environmental_descriptive",
              glue("env_{var}_response_imputed.png")),
    width=16, height=16*0.600
  )
}

# Save dataframe with imputed environmental variables
write_csv(
  df_environ_imp,
  file.path(data_path, "processed", "environmental_imputed.csv")
)

#' 
#' ## Make contrasts for the trend
#' 
## ---------------------------------------------------------------------------------------
# Step 7: Make contrasts for the trend -------------------------------------------------
# note: Standard errors and hypothesis tests computed with Delta method
# (marginaleffects default)

# Check time indices for contrast
# df_environ_model |>
#   filter(date %in% c("2012-01-01", "2021-12-01", "2022-12-01")) |>
#   print(width=Inf)

make_trend_contrasts <- function(model, var, date_range_plot, label, units, ...) {
  print(var)
  
  # Exclude terms to make predictions for the trend
  exclude_set <- c(
    as.character(glue("ti(month_ind):district{levels(df_environ_model$district)}")),
    as.character(glue("ti(time_ind,month_ind):district{levels(df_environ_model$district)}"))
  )
  
  # Average slope ----------------------------------------------------------------------
  
  # Average change in the trend per 1 year increase
  avg_yearly_trend_slope <- avg_comparisons(
    model,
    variables = list(time_ind = 12),
    by = "district",
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    mutate(
      name = "avg_yearly_trend_slope"
    )
  
  avg_yearly_trend_slope
  
  
  # Difference in the slopes (Moshi - Siha)
  avg_yearly_trend_slope_diff <- avg_comparisons(
    model,
    variables = list(time_ind = 12),
    by = "district",
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
    # Test for the equality of both estimates
    hypothesis = "b2 - b1 = 0"
  ) |> 
    as_tibble() |> 
    mutate(
      name = "avg_yearly_trend_slope_diff"
    )
  
  avg_yearly_trend_slope_diff
  
  
  # Plot yearly trend change before taking the average by district ---.
  
  # plot_comparisons(
  #   model,
  #   variables = list(time_ind = 12),
  #   by = c("time_ind", "district"),
  #   comparison = "difference",
  #   type = "response",
  #   exclude = exclude_set,
  # )
  
  p_yearly_trend_slope <- comparisons(
    model,
    newdata = df_environ_model,
    variables = list(time_ind = 12),
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    filter(date >= date_range_plot[1] & date <= date_range_plot[2]) |>
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_hline(yintercept = 0) +
    geom_line(
      aes(x=date, y=estimate, color=district), linewidth=0.8
    ) +
    geom_ribbon(
      aes(x=date, ymin=conf.low, ymax=conf.high, fill=district),
      alpha=0.2
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y"
    ) +
    scale_color_okabe_ito() +
    scale_fill_okabe_ito() +
    theme_bw(base_size = 18) +
    labs(
      color = "District",
      fill = "District",
      x = "Date (month)",
      y = glue("{label}\nyearly trend change {units}")
    )
  
  
  # Average trend difference -----------------------------------------------------------
  
  # Average trends
  avg_trend <- avg_predictions(
    model,
    variables = "district",
    type = "response",
    exclude = exclude_set
  ) |> 
    as_tibble() |> 
    mutate(
      name = "avg_trend"
    )
  
  avg_trend
  
  # Average difference in trends
  avg_trend_diff <- avg_comparisons(
    model,
    variables = "district",
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    mutate(
      name = "avg_trend_diff"
    )
  
  avg_trend_diff
  
  # Plot trend difference before taking the average by district ----.
  
  # plot_comparisons(
  #   model,
  #   variables = c("district"),
  #   by = c("time_ind"),
  #   comparison = "difference",
  #   type = "response",
  #   exclude = exclude_set,
  # )
  
  p_trend_diff <- comparisons(
    model,
    newdata = df_environ_model,
    variables = "district",
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    filter(date >= date_range_plot[1] & date <= date_range_plot[2]) |>
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_hline(yintercept = 0) +
    geom_line(
      aes(x=date, y=estimate), linewidth=0.8
    ) +
    geom_ribbon(
      aes(x=date, ymin=conf.low, ymax=conf.high),
      alpha=0.2
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y"
    ) +
    theme_bw(base_size = 18) +
    labs(
      color = "District",
      fill = "District",
      x = "Date (month)",
      y = glue("{label}\ntrend difference\n(Siha - Moshi) {units}")
    )
  
  # Trend difference between 2012-01 and 2021-12 ---------------------------------------
  
  start_indices <- c(time_ind=1, month_ind=1)
  end_indices <- c(time_ind=120, month_ind=12)
  
  # Alternatively ---.
  # Each variable has observations available for different time ranges, so might want
  # to set different time indices
  # PM2.5 has observations until 2022-12
  # The rain variables for Siha rely on extrapolations for the contrast
  # ---.
  # start_indices <- df_environ_model |> 
  #   filter(date == date_range_plot[[1]]) |> 
  #   select(time_ind, month_ind) |> 
  #   distinct() |> 
  #   as_vector()
  # 
  # end_indices <- df_environ_model |> 
  #   filter(date == date_range_plot[[2]]) |> 
  #   select(time_ind, month_ind) |> 
  #   distinct() |> 
  #   as_vector()
  # -------------------.
  
  # Query explanation:
  # For the observations we had at 2012-01 (time_ind==1, month_ind==1),
  # what is the effect on the trend of changing time_ind to 120 and month_ind to 12 (date being 2021-12)
  
  # Temporal difference
  temp_trend_diff <- comparisons(
    model,
    newdata = filter(df_environ_model, time_ind==1),
    variables = list(
      time_ind = c(start_indices[["time_ind"]], end_indices[["time_ind"]]),
      month_ind = c(start_indices[["month_ind"]], end_indices[["month_ind"]])
    ),
    cross = TRUE,
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    relocate(district) |> 
    mutate(
      name = "temp_trend_diff"
    )
  
  temp_trend_diff
  
  # Difference in temporal differences
  temp_trend_diff_diff <- comparisons(
    model,
    newdata = filter(df_environ_model, time_ind==1),
    variables = list(
      time_ind = c(start_indices[["time_ind"]], end_indices[["time_ind"]]),
      month_ind = c(start_indices[["month_ind"]], end_indices[["month_ind"]])
    ),
    cross = TRUE,
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
    # Test for the equality of both estimates
    hypothesis = "b2 - b1 = 0"
  ) |> 
    as_tibble() |> 
    mutate(
      name = "temp_trend_diff_diff"
    )
  
  temp_trend_diff_diff
  
  # Plot the trends
  p_trends <- predictions(
    model,
    newdata = df_environ_model,
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    filter(date >= date_range_plot[1] & date <= date_range_plot[2]) |>
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      xintercept = as.Date(c("2012-01-01", "2021-12-01")), linewidth=1
    ) +
    geom_line(
      aes(x=date, y=estimate, color=district), linewidth=0.8
    ) +
    geom_ribbon(
      aes(x=date, ymin=conf.low, ymax=conf.high, fill=district),
      alpha=0.2
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y"
    ) +
    scale_color_okabe_ito() +
    scale_fill_okabe_ito() +
    theme_bw(base_size = 18) +
    labs(
      color = "District",
      fill = "District",
      x = "Date (month)",
      y = glue("{label}\ntrend {units}")
    )
  
  # Merge results ----------------------------------------------------------------------
  
  # Combine earlier plots
  p_joint <- p_trends / p_yearly_trend_slope / p_trend_diff +
    plot_layout(axes="collect")
  
  ggsave(
    plot=p_joint,
    filename=file.path(
      res_path, "plots", "environmental_descriptive",
      glue("env_{var}_trend_contrasts.png")
    ),
    width=16, height=16*0.700
  )
  
  # Combine tables
  contrast_res <- bind_rows(
    avg_yearly_trend_slope,
    avg_yearly_trend_slope_diff,
    avg_trend,
    avg_trend_diff,
    temp_trend_diff,
    temp_trend_diff_diff
  ) |>
    select(! c(predicted_lo:predicted, date:time_ind)) |> 
    relocate(name)
  
  return(contrast_res)
}

df_environ_an <- df_environ_an |> 
  mutate(
    contrast_res = pmap(list(model, var, date_range_plot, label, units), make_trend_contrasts)
  )


# df_environ_an[["contrast_res"]][[1]] |> print(width=Inf)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Prepare table for contrasts
contrast_tbl_raw <- df_environ_an |> 
  select(var, label, units, contrast_res) |> 
  unnest(contrast_res) |> 
  select(label, units, name, district, estimate, conf.low, conf.high, p.value) |> 
  # If checking average trend, H0 = 0 not informative, so remove p.value
  mutate(
    p.value = if_else(name=="avg_trend", NA, p.value)
  )

# All constrasts
contrast_tbl_full <- contrast_tbl_raw |> 
  mutate(
    label = glue("{label} {units}"),
    Contrast = case_match(
      name,
      "avg_yearly_trend_slope" ~ "Average yearly trend change",
      "avg_yearly_trend_slope_diff" ~ "Average yearly trend change",
      "avg_trend" ~ "Average trend level",
      "avg_trend_diff" ~ "Average trend level",
      "temp_trend_diff" ~ "Trend difference (2012-01 to 2021-12)",
      "temp_trend_diff_diff" ~ "Trend difference (2012-01 to 2021-12)",
    ),
    Contrast = factor(
      Contrast, levels=c("Average trend level", "Average yearly trend change",
                         "Trend difference (2012-01 to 2021-12)")
    ),
    District = if_else(is.na(district), "Siha - Moshi", district),
    `Estimate (95% CI)` = glue("{round(estimate, 3)} ({round(conf.low, 3)}, {round(conf.high, 3)})"),
    star = case_when(
      p.value < 0.05 ~ "*",
      .default = ""
    ),
    `p value` = Hmisc::format.pval(p.value, na.form="-", digits=4, eps=1e-4),
    `p value` = paste0(`p value`, star)
  ) |> 
  select(- c(estimate, conf.low, conf.high, p.value, district, name, units, star)) |> 
  arrange(Contrast) |> 
  gt(
    groupname_col = "label",
    rowname_col = "Contrast"
  ) |> 
  cols_align(align = "left", columns = "Contrast")

contrast_tbl_full

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_full,
    file.path(res_path, "tables", glue("environmental_contrasts_full.{extension}"))
    )
)

# p.value < 0.05
contrast_tbl_filt <- contrast_tbl_raw |>
  filter(p.value < 0.05) |> 
  mutate(
    label = glue("{label} {units}"),
    Contrast = case_match(
      name,
      "avg_yearly_trend_slope" ~ "Average yearly trend change",
      "avg_yearly_trend_slope_diff" ~ "Average yearly trend change",
      "avg_trend" ~ "Average trend level",
      "avg_trend_diff" ~ "Average trend level",
      "temp_trend_diff" ~ "Trend difference (2012-01 to 2021-12)",
      "temp_trend_diff_diff" ~ "Trend difference (2012-01 to 2021-12)",
    ),
    Contrast = factor(
      Contrast, levels=c("Average trend level", "Average yearly trend change",
                         "Trend difference (2012-01 to 2021-12)")
    ),
    District = if_else(is.na(district), "Siha - Moshi", district),
    `Estimate (95% CI)` = glue("{round(estimate, 3)} ({round(conf.low, 3)}, {round(conf.high, 3)})"),
    star = case_when(
      p.value < 0.05 ~ "*",
      .default = ""
    ),
    `p value` = Hmisc::format.pval(p.value, na.form="-", digits=4, eps=1e-4),
    `p value` = paste0(`p value`, star)
  ) |> 
  select(- c(estimate, conf.low, conf.high, p.value, district, name, units, star)) |> 
  arrange(Contrast) |> 
  gt(
    groupname_col = "label",
    rowname_col = "Contrast"
  ) |> 
  cols_align(align = "left", columns = "Contrast")

contrast_tbl_filt

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_filt,
    file.path(res_path, "tables", glue("environmental_contrasts_filt.{extension}"))
    )
)


#' 
#' 
## ---------------------------------------------------------------------------------------
# Save results
save(
  df_environ_an,
  df_environ_model,
  file = file.path(res_path, "R_output", "environmental_descriptive.RData")
)

# load(
#   file.path(res_path, "R_output", "environmental_descriptive.RData")
#   )

#' 
#' 
#' # Disease
#' 
#' ## Prepare data
#' 
## ---------------------------------------------------------------------------------------
df_disease <- read_csv(file.path(data_path, "processed", "disease.csv"))

# Rename diseases
df_disease <- df_disease |> 
  mutate(
    disease = case_match(
      disease,
      "Bronchial Asthma" ~ "Chronic Respiratory Disease",
      .default = disease
    )
  )

#' 
## ---------------------------------------------------------------------------------------
# Add population (2022 imputed) to disease dataframe
df_disease_model <- df_disease |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop_imp, year=year(date)),
    join_by(district, year)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

# Compute incidence rate per 100k people
df_disease_model <- df_disease_model |> 
  mutate(case_rate = n_cases / population * 1e5)


# Set up variables for modelling
df_disease_model <- df_disease_model |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )

df_disease_model |> 
  print(width=Inf, n=13)

#' 
#' ### Disease groupings
#' 
## ---------------------------------------------------------------------------------------

# Re-define disease groupings according to Renz and Skevaki notations
# Type manually

# Renz

ref_renz <- tribble(
  ~disease_group_renz, ~disease,
  # 01
  "Gastrointestinal Infections", c(
    "Diarrhea With No Dehydration",
    "Diarrhea With Severe Dehydration",
    "Dysentery",
    "Cholera",
    "Typhoid",
    "Intestinal Worms"
  ),
  # 02
  "Urinary Infections", c(
    "Urinary Tract Infections"
  ),
  # 03
  "Respiratory Infections", c(
    "Upper Respiratory Infections",
    "Tuberculosis",
    "Pneumonia, Severe",
    "Influenza"
  ),
  # 04
  "Other infections", c(
    "Meningitis",
    "Rabies",
    "Measles",
    "Leprosy",
    "Infectious Eye Disease",
    "Skin Infection - Fungal",
    "Malaria"
  ),
  # 05
  "Malnutrition", c(
    "Nutritional Disorders, Other",
    "Moderate Malnutrition",
    "Marasmus",
    "Marasmic Kwashiorkor",
    "Kwashiorkor"
  ),
  # 06
  "Diabetes Mellitus", c(
    "Diabetes Mellitus"
  ),
  # 07
  "Respiratory Diseases", c(
    "Chronic Respiratory Disease"
  ),
  # 08
  "Trauma and Injuries", c(
    "Road Traffic Accidents",
    "Fractures"
  ),
  # 09
  "Cardiovascular Diseases", c(
    "Other Cardiovascular Diseases",
    "Hypertension"
  ),
  # 10
  "Cancer", c(
    "Neoplasms/Cancer"
  )
) |> 
  unnest(cols=c(disease))

ref_renz |> 
  print(n=Inf)


#' 
#' 
## ---------------------------------------------------------------------------------------
# Skevaki

ref_skevaki_block <- tribble(
  ~disease_block_skevaki, ~disease_group_skevaki,
  # A
  "Infectious/Communicable Diseases", c(
    # a
    "Gastrointestinal Infections",
    # b
    "Vector-borne Infections",
    # c
    "Respiratory Infections",
    # d
    "Other Communicable Diseases"
  ),
  # B
  "Disorders of Mixed Etiology", c(
    # e
    "Diarrheal Diseases",
    # f
    "Neurological Diseases"
  ),
  # C
  "Non-Communicable Diseases", c(
    # g
    "Cardiovascular Diseases",
    # h
    "Respiratory Diseases",
    # i
    "Gynecological Diseases",
    # j
    "Diabetes Mellitus",
    # k
    "Malnutrition",
    # l
    "Cancer",
    # m
    "Neurological",
    # n
    "Trauma",
    # o
    "Other Non-Communicable"
  )
) |> 
  unnest(cols=c(disease_group_skevaki))

# ---------------------------------------------

ref_skevaki_group <- tribble(
  ~disease_group_skevaki, ~disease,
  # A
  # a
  "Gastrointestinal Infections", c(
    "Diarrhea With Severe Dehydration",
    "Dysentery",
    "Cholera",
    "Typhoid",
    "Intestinal Worms"
  ),
  # b
  "Vector-borne Infections", c(
    "Malaria"
  ),
  # c
  "Respiratory Infections", c(
    "Upper Respiratory Infections",
    "Tuberculosis",
    "Pneumonia, Severe",
    "Influenza"
  ),
  # d
  "Other Communicable Diseases", c(
    "Schistosomiasis",
    "Urinary Tract Infections",
    "Rabies",
    "Measles",
    "Leprosy",
    "Infectious Eye Disease",
    "Skin Infection - Fungal"
  ),
  # B
  # e
  "Diarrheal Diseases", c(
    "Diarrhea With No Dehydration"
  ),
  # f
  "Neurological Diseases", c(
    "Meningitis",
    "Acute Flaccid Paralysis"
  ),
  # C
  # g
  "Cardiovascular Diseases", c(
    "Other Cardiovascular Diseases",
    "Hypertension"
  ),
  # h
  "Respiratory Diseases", c(
    "Chronic Respiratory Disease"
  ),
  # i
  "Gynecological Diseases", c(
    "Gynecological Diseases"
  ),
  # j
  "Diabetes Mellitus", c(
    "Diabetes Mellitus"
  ),
  # k
  "Malnutrition", c(
    "Nutritional Disorders, Other",
    "Moderate Malnutrition",
    "Marasmus",
    "Marasmic Kwashiorkor",
    "Kwashiorkor"
  ),
  # l
  "Cancer", c(
    "Neoplasms/Cancer"
  ),
  # m
  "Neurological", c(
    "Psychoses",
    "Neuroses",
    "Epilepsy"
  ),
  # n
  "Trauma", c(
    "Road Traffic Accidents",
    "Fractures"
  ),
  # o
  "Other Non-Communicable", c(
    "Substance Abuse",
    "Snake and Insect Bites",
    "Poisoning",
    "Peptic Ulcers",
    "Caries"
  )
) |>
  unnest(cols=c(disease))


ref_skevaki <- ref_skevaki_block |> 
  full_join(ref_skevaki_group)

ref_skevaki |> 
  print(n=Inf)

rm(ref_skevaki_block, ref_skevaki_group)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Final grouping (after discussion)

ref_final_block <- tribble(
  ~disease_block_final, ~disease_group_final,
  # A
  "Infectious/Communicable Diseases", c(
    # a
    "Gastrointestinal Infections",
    # new
    "Urinary Infections",
    # new
    "Infectious Eye Disease",
    # b
    "Vector-borne Infections",
    # c
    "Respiratory Infections",
    # d
    "Other Communicable Diseases",
    # new
    "Meningitis"
  ),
  # B
  "Disorders of Mixed Etiology", c(
    # f
    "Neurological Diseases"
  ),
  # C
  "Non-Communicable Diseases", c(
    # g
    "Cardiovascular Diseases",
    # h - new (new)
    "Respiratory Diseases",
    # i
    "Gynecological Diseases",
    # j
    "Diabetes Mellitus",
    # k
    "Malnutrition",
    # l
    "Cancer",
    # m
    "Neurological",
    # n
    "Trauma",
    # o
    "Other Non-Communicable"
  )
) |> 
  unnest(cols=c(disease_group_final))


ref_final_group <- tribble(
  ~disease_group_final, ~disease,
  # A
  # a
  "Gastrointestinal Infections", c(
    "Diarrhea With Severe Dehydration",
    # new
    "Diarrhea With No Dehydration",
    "Dysentery",
    "Cholera",
    "Typhoid",
    "Intestinal Worms"
  ),
  # new
  "Urinary Infections", c(
    "Urinary Tract Infections"
  ),
  # new
  "Infectious Eye Disease", c(
    "Infectious Eye Disease"
  ),
  # b
  "Vector-borne Infections", c(
    "Malaria"
  ),
  # c
  "Respiratory Infections", c(
    "Upper Respiratory Infections",
    "Tuberculosis",
    "Pneumonia, Severe",
    "Influenza"
  ),
  # d
  "Other Communicable Diseases", c(
    "Schistosomiasis",
    "Rabies",
    "Measles",
    "Leprosy",
    "Skin Infection - Fungal"
  ),
  # new
  "Meningitis", c(
    "Meningitis"
  ),
  # B
  # e - removed - Diarrheal Diseases
  # f
  "Neurological Diseases", c(
    "Acute Flaccid Paralysis"
  ),
  # C
  # g
  "Cardiovascular Diseases", c(
    "Other Cardiovascular Diseases",
    "Hypertension"
  ),
  # h
  # new (new)
  "Respiratory Diseases", c(
    "Chronic Respiratory Disease"
  ),
  # i
  "Gynecological Diseases", c(
    "Gynecological Diseases"
  ),
  # j
  "Diabetes Mellitus", c(
    "Diabetes Mellitus"
  ),
  # k
  "Malnutrition", c(
    "Nutritional Disorders, Other",
    "Moderate Malnutrition",
    "Marasmus",
    "Marasmic Kwashiorkor",
    "Kwashiorkor"
  ),
  # l
  "Cancer", c(
    "Neoplasms/Cancer"
  ),
  # m
  "Neurological", c(
    "Psychoses",
    "Neuroses",
    "Epilepsy"
  ),
  # n
  "Trauma", c(
    "Road Traffic Accidents",
    "Fractures"
  ),
  # o
  "Other Non-Communicable", c(
    "Substance Abuse",
    "Snake and Insect Bites",
    "Poisoning",
    "Peptic Ulcers",
    "Caries"
    )
  ) |> 
  unnest(cols=c(disease))


ref_final <- ref_final_block |> 
  full_join(ref_final_group)

ref_final |> 
  print(n=Inf)

rm(ref_final_block, ref_final_group)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Add new disease grouping to model table

df_disease_model_explore <- df_disease_model |> 
  left_join(ref_renz) |>
  left_join(ref_skevaki) |> 
  left_join(ref_final)

#' 
#' ### Explore
#' 
#' Explore disease incidence rates and the different disease groupings.
#' 
## ---------------------------------------------------------------------------------------
# Original disease classification


plot_disease_table <- function(df, group_over, group_under) {
  df |> 
    ggplot() +
    geom_tile(
      aes(x=date, y=disease, fill=case_rate),
      color="black",
      width=31
    ) +
    scale_fill_viridis_c(
      transform = "log1p",
      breaks = c(0,10,100,1000,6000),
    ) +
    facet_nested(
      cols = vars(district),
      rows = vars({{ group_over }}, {{ group_under }}),
      scales = "free_y",
      space = "free_y",
      strip = strip_nested(size = "variable", clip="off", bleed = FALSE),
      solo_line = TRUE,
      nest_line = TRUE,
    ) +
    guides(
      fill = guide_colorbar(
        barheight = 12,
        title.vjust = 2,
      )
    ) +
    labs(
      fill = "Incidence rate\n(per 100k people)",
      x = "Date (month)",
      y = ""
    ) +
    theme(
      legend.title = element_text(face="bold"),
      axis.title = element_text(face="bold"),
      strip.text = element_text(face="bold"),
      # strip.background = element_rect(fill="white", color="gray"),
      strip.background = element_blank(),
      ggh4x.facet.nestline = element_line(colour = "black"),
      strip.text.y = element_text(angle=0, size=rel(1)),
      strip.text.x = element_text(angle=0, size=rel(1.5)),
      legend.text = element_text(size=rel(1.0))
    )
}

df_disease_model_explore |> 
  # More space
  mutate(
    disease_group = recode(
      disease_group,
      `Diarrheal disease/Gastrointesntinal infections`="Diarrheal disease/\nGastrointesntinal infections",
      `Other Communicable Diseases/Gastrointestinal infections`="Other Communicable Diseases/\nGastrointestinal infections"
    )
  ) |>
  plot_disease_table(group_over = disease_communicable, group_under = disease_group)

ggsave(
  file.path(res_path, "plots", "disease_incidence-rate_original-groups.png"),
  width=22, height=22*0.530
)


#' 
#' 
## ---------------------------------------------------------------------------------------
# Original groups again, but remove category with many missings

df_disease_model_explore |> 
  # More space
  mutate(
    disease_group = recode(
      disease_group,
      `Diarrheal disease/Gastrointesntinal infections`="Diarrheal disease/\nGastrointesntinal infections",
      `Other Communicable Diseases/Gastrointestinal infections`="Other Communicable Diseases/\nGastrointestinal infections"
    )
  ) |>
  # Remove disease category with many missings
  filter(disease_group != "Other vector borne diseases") |> 
  plot_disease_table(group_over = disease_communicable, group_under = disease_group)

ggsave(
  file.path(res_path, "plots", "disease_incidence-rate_original-groups_minus-missings.png"),
  width=22, height=22*0.530
)


#' 
#' 
## ---------------------------------------------------------------------------------------

# Skevaki groups

df_disease_model_explore |> 
  filter(
    !is.na(disease_group_skevaki)
  ) |> 
  plot_disease_table(group_over = disease_block_skevaki, group_under = disease_group_skevaki)

ggsave(
  file.path(res_path, "plots", "disease_incidence-rate_skevaki-groups.png"),
  width=22, height=22*0.530
)


#' 
## ---------------------------------------------------------------------------------------
# Renz groups

df_disease_model_explore |> 
  filter(
    !is.na(disease_group_renz)
  ) |>
  plot_disease_table(group_over = disease_block_skevaki, group_under = disease_group_renz)

ggsave(
  file.path(res_path, "plots", "disease_incidence-rate_renz-groups.png"),
  width=22, height=22*0.530
)


#' 
## ---------------------------------------------------------------------------------------
# Final groups

df_disease_model_explore |> 
  filter(
    !is.na(disease_group_final)
  ) |>
  plot_disease_table(group_over = disease_block_final, group_under = disease_group_final)

ggsave(
  file.path(res_path, "plots", "disease_incidence-rate_final-groups.png"),
  width=22, height=22*0.530
)

#' 
#' 
#' #### Plot all incidence rate time series
#' 
## ---------------------------------------------------------------------------------------
df_disease_iter1 <- tibble(
  disease = unique(df_disease_model$disease),
  print_name = str_replace_all(disease, c(" "="-",
                                          "/"="-",
                                          "\\("="-",
                                          "\\)"="-",
                                          ","="-"))
)


pwalk(
  df_disease_iter1,
  function(disease, print_name, ...) {
    
    print(disease)
    
    df_disease_model |>
      filter(disease == {{disease}}) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date, y=case_rate, color=district), linewidth=0.8
      ) +
      geom_point(
        aes(x=date, y=case_rate, color=district), size=2.5
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y",
        # limits = date_range_plot
      ) +
      scale_color_okabe_ito() +
      theme_bw(base_size = 18) +
      theme(
        axis.title.y = element_text(size=rel(0.9))
      ) +
      labs(
        color = "District",
        x = "Date (month)",
        y = glue("{disease}\nincidence rate\n(per 100k people)")
      )
    
    ggsave(
      file.path(res_path, "plots", "disease_descriptive",
                glue("disease_{print_name}_incidence-rate.png")),
      width=16, height=16*0.600
    )
  }
)

#' 
#' 
#' ### Disease filtering
#' 
#' Use final grouping and filter out diseases according to clinical input, and also if there are many missing values or the incidence rate is too low.
#' 
## ---------------------------------------------------------------------------------------
# Remove old grouping and replace by the final one
df_disease_model <- df_disease_model |> 
  left_join(ref_final) |> 
  select(- c(disease_group, disease_communicable)) |> 
  rename(
    disease_communicable = disease_block_final,
    disease_group = disease_group_final
  )

#' 
## ---------------------------------------------------------------------------------------
# Remove diseases with no assigned group
df_disease_model |> 
  filter(is.na(disease_group)) |> 
  pull(disease) |> unique()

df_disease_model <- df_disease_model |> 
  filter(! is.na(disease_group))

df_disease_model$disease |> table()

# Diseases to remove according to clinical input
df_disease_model <- df_disease_model |> 
  filter(! disease %in% c("Acute Flaccid Paralysis", "Meningitis", "Gynecological Diseases"))


# Add cases of all Malnutrition diseases:
# - Nutritional disorders, other
# - Moderate malnutrition
# - Marasmus
# - Marasmic Kwashiorkor
# - Kwashiorkor

df_disease_model_malnutrition <- df_disease_model |> 
  filter(disease_group == "Malnutrition")

df_disease_model_malnutrition <- df_disease_model_malnutrition |> 
  mutate(
    disease = "Malnutrition"
  ) |> 
  group_by(pick(everything(), -c(n_cases, case_rate))) |> 
  summarise(
    n_cases = sum(n_cases)
  ) |> 
  ungroup() |> 
  mutate(case_rate = n_cases / population * 1e5)

df_disease_model <- df_disease_model |> 
  filter(! disease_group == "Malnutrition")

df_disease_model <- bind_rows(
  df_disease_model,
  df_disease_model_malnutrition
)

rm(df_disease_model_malnutrition)

# Add cases of Diarrhea:
# - Diarrhea With Severe Dehydration
# - Diarrhea With No Dehydration

df_disease_model_diarrhea <- df_disease_model |> 
  filter(
    disease %in% c("Diarrhea With Severe Dehydration", "Diarrhea With No Dehydration")
  )

df_disease_model_diarrhea <- df_disease_model_diarrhea |> 
  mutate(
    disease = "Diarrhea"
  ) |> 
  group_by(pick(everything(), -c(n_cases, case_rate))) |> 
  summarise(
    n_cases = sum(n_cases)
  ) |> 
  ungroup() |> 
  mutate(case_rate = n_cases / population * 1e5)

df_disease_model <- df_disease_model |> 
  filter(
    ! disease %in% c("Diarrhea With Severe Dehydration", "Diarrhea With No Dehydration")
    )

df_disease_model <- bind_rows(
  df_disease_model,
  df_disease_model_diarrhea
)

rm(df_disease_model_diarrhea)

#' 
## ---------------------------------------------------------------------------------------
# Check initial disease filtering and aggregation

df_disease_model |> 
  plot_disease_table(group_over = disease_communicable, group_under = disease_group)

ggsave(
  file.path(res_path, "plots", "disease_incidence-rate_final-groups_initial-filtering.png"),
  width=22, height=22*0.530
)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Remove other diseases with missings
# - Cholera
# - Rabies
# - Measles
# - Influenza


df_disease_model <- df_disease_model |> 
  filter(! disease %in% c("Cholera", "Rabies", "Measles", "Influenza"))

df_disease_model |> 
  plot_disease_table(group_over = disease_communicable, group_under = disease_group)

ggsave(
  file.path(res_path, "plots", "disease_incidence-rate_final-groups_initial-filtering-2.png"),
  width=22, height=22*0.530
)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Filter diseases according to incidence rate

# We have 1 missing for Typhoid, but that's it

# Average incidence rate per 100k for the whole period
df_avg_case_rate <- df_disease_model |> 
  group_by(disease) |> 
  summarise(
    avg_case_rate = mean(case_rate, na.rm=TRUE)
  )

df_avg_case_rate |> 
  print(n=Inf)

df_avg_case_rate |> 
  mutate(
    disease = fct_reorder(disease, desc(avg_case_rate))
  ) |> 
  ggplot() +
  geom_point(
    aes(y=disease, x=avg_case_rate)
  ) +
  geom_vline(xintercept = 100) +
  scale_x_continuous(n.breaks=12)

ggsave(
  file.path(res_path, "plots", "disease_average-incidence-rate.png"),
  width=22, height=22*0.530
)

# I am not sure whether I should remove any disease


#' 
#' ### Save processed disease data
#' 
#' 
## ---------------------------------------------------------------------------------------
# df_disease_model |> 
#   pull(disease) |> 
#   unique() |> 
#   length()

# Save processed disease dataset
df_disease_model |> 
  select(- c(population, case_rate, time_ind, month_ind)) |> 
  relocate(district, date, disease, disease_group, disease_communicable, n_cases) |> 
  write_csv(
    file.path(data_path, "processed", "disease_processed.csv")
  )

# Save dataset without year 2014 (inconsistent diagnoses)
df_disease_model |> 
  select(- c(population, case_rate, time_ind, month_ind)) |> 
  relocate(district, date, disease, disease_group, disease_communicable, n_cases) |> 
  filter(year(date) > 2014) |> 
  write_csv(
    file.path(data_path, "processed", "disease_processed_wo-2014.csv")
  )

#' 
#' 
#' ## Analysis
#' 
#' ## All data (2014 onwards)
#' 
#' ### Per disease
#' 
## ---------------------------------------------------------------------------------------
# Setup data
df_disease <- read_csv(file.path(data_path, "processed", "disease_processed.csv"))
pop_imp <- read_csv(file.path(data_path, "processed", "population_imputed.csv"))

# Add population (2022 imputed) to disease dataframe
df_disease_model <- df_disease |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop_imp, year=year(date)),
    join_by(district, year)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

# Compute incidence rate per 100k people
df_disease_model <- df_disease_model |> 
  mutate(case_rate = n_cases / population * 1e5)

# Set up variables for modelling
df_disease_model <- df_disease_model |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )

df_disease_model |> 
  print(width=Inf, n=13)


#' 
#' 
## ---------------------------------------------------------------------------------------
# Dataframe to iterate over for the analyses

df_disease_an <- tibble(
  disease = unique(df_disease_model$disease),
  print_name = str_replace_all(disease, c(" "="-",
                                          "/"="-",
                                          "\\("="-",
                                          "\\)"="-",
                                          ","="-"))
)

# Add data filtered for each disease
df_disease_an <- df_disease_an |> 
  mutate(
    df_model = map(disease, \(x) filter(df_disease_model, disease==x))
  )

#' 
#' 
#' #### Plot the response
#' 
## ---------------------------------------------------------------------------------------
# Step 1: Plot the response --------------------------------------------------------------

# Redo the incidence rate plots after disease filtering
# (We have Malnutrition as an aggregate of diseases now)


plot_response_disease <- function(disease, print_name, df_model,
                                  folder_path=file.path(res_path, "plots", "disease_descriptive"), ...) {
  print(disease)
  
  df_model |>
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_line(
      aes(x=date, y=case_rate, color=district), linewidth=0.8
    ) +
    geom_point(
      aes(x=date, y=case_rate, color=district), size=2.5
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y",
      # limits = date_range_plot
    ) +
    scale_color_okabe_ito() +
    theme_bw(base_size = 18) +
    theme(
      axis.title.y = element_text(size=rel(0.9))
    ) +
    labs(
      color = "District",
      x = "Date (month)",
      y = glue("{disease}\nincidence rate\n(per 100k people)")
    )
  
  ggsave(
    file.path(folder_path,
              glue("disease_{print_name}_incidence-rate.png")),
    width=16, height=16*0.600
  )
}


pwalk(
  df_disease_an,
  plot_response_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
)
  

#' 
#' 
#' #### Fit model
#' 
#' 
## ---------------------------------------------------------------------------------------
# Step 2: Fit the model ----------------------------------------------------------------

# Get number of basis functions for trend
get_k_trend_disease <- function(df_model) {
  df_model |> 
    filter(! is.na(n_cases)) |>
    pull(date) |>
    year() |>
    range() |>
    diff()+1
}

df_disease_an <- df_disease_an |> 
  mutate(
    k_trend = map(df_model, get_k_trend_disease)
  )

df_disease_an[["k_trend"]]

# Add distribution family of the response
df_disease_an <- df_disease_an |> 
  mutate(
    family = list(mgcv::nb(link="log"))
  )

# Fit models
fit_descriptive_gam_disease <- function(disease, df_model, k_trend, family, ...) {
  # Formula
  form <- n_cases ~ 0 + district +
    ti(time_ind, bs="cr", k=k_trend, by=district) +
    ti(month_ind, bs="cc", k=12, by=district) +
    ti(time_ind, month_ind, bs=c("cr", "cc"), k=c(k_trend,12), by=district)
  
  print(disease)
  print(form)
  
  # Start and end points of monthly cyclic splines
  # note: Slightly out of c(1,12) to not impose that January (1) is equal to December (12)
  knots <- list(month_ind = c(0.5, 12.5))
  
  gam(
    form,
    # Include the offset here instead of in the formula, so that it is not used in the predictions
    # Divide offset by 1e5 so that predictions are incidence rates per 1e5 people
    offset = log(population/1e5),
    data=df_model,
    family = family,
    method = "REML",
    knots = knots
  )
}

df_disease_an <- df_disease_an |> 
  mutate(
    model = pmap(list(disease, df_model, k_trend, family), fit_descriptive_gam_disease)
  )

#' 
#' 
#' #### Check model
#' 
## ---------------------------------------------------------------------------------------
# Step 3: Check the model --------------------------------------------------------------

check_gam_disease <- function(disease, print_name, model,
                              folder_path=file.path(res_path, "plots", "disease_descriptive"), ...) {
  cat(glue(
    "\n
      ---------------------------------------
      Printing model diagnostics of variable:
                       {disease}
      ----------------------------------------
      \n"
  ))
  
  print(summary(model))
  
  # Residual diagnostics
  gratia::appraise(model)
  
  # Save plot
    ggsave(
      filename = file.path(
        folder_path,
        glue("disease_{print_name}_gam_diagnostics.png")
      ),
      width=16, height=16*0.618
    )
  
  # Optionally: check model component predictions
  # gratia::draw(model) |> print()
}

pwalk(
  df_disease_an,
  check_gam_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
)


#' 
#' 
#' #### Get predictions
#' 
## ---------------------------------------------------------------------------------------
# Step 4: Get predictions, as well as decomposition into trend, seasonal and residual ----

# Get predictions and time series decomposition
df_disease_an <- df_disease_an |> 
  mutate(
    pred_df = pmap(list(model=model, model_df=df_model, var="case_rate"), get_pred_ts_decomp)
  )

#' 
#' 
#' 
#' #### Plot time series decomposition
#' 
## ---------------------------------------------------------------------------------------

# Step 5: Plot time series decomposition -----------------------------------------------

plot_ts_decomp_disease <- function(pred_df, disease, print_name, var="case_rate",
                                   folder_path=file.path(res_path, "plots", "disease_descriptive"), ...) {
  print(disease)
  
  # Reference bar for y scale
  barlength <- select(pred_df, c(all_of(var), pred:season_conf.high)) |> 
    sapply(\(x) diff(range(x, na.rm=TRUE))) |>
    min()
  
  # Plot fitted line
  p_fit <- pred_df |> 
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_point(
      aes(x=date, y=.data[[var]], color=district), size=2.5
    ) + 
    geom_line(
      aes(x=date, y=pred, color=district), linewidth=0.8
    ) +
    geom_ribbon(
      aes(x=date, ymin=pred_conf.low, ymax=pred_conf.high, fill=district),
      alpha=0.2
    ) +
    geom_rect(
      aes(
        xmin=max(date)+60, xmax=max(date)+90,
        ymin=mean(.data[[var]], na.rm=TRUE) - (barlength/2),
        ymax=mean(.data[[var]], na.rm=TRUE) + (barlength/2)
      ),
      color="black", fill="gray75"
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y"
    ) +
    scale_color_okabe_ito() +
    scale_fill_okabe_ito() +
    theme_bw(base_size = 18) +
    theme(
      axis.title.y = element_text(size=rel(0.9))
    ) +
    labs(
      color = "District",
      fill = "District",
      x = "Date (month)",
      y = glue("{disease}\nincidence rate\n(per 100k people)")
    )
  
  # Plot trend
  p_trend <- pred_df |> 
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_line(
      aes(x=date, y=trend, color=district), linewidth=0.8
    ) +
    geom_ribbon(
      aes(x=date, ymin=trend_conf.low, ymax=trend_conf.high, fill=district),
      alpha=0.2
    ) +
    geom_rect(
      aes(
        xmin=max(date)+60, xmax=max(date)+90,
        ymin=mean(trend, na.rm=TRUE) - (barlength/2),
        ymax=mean(trend, na.rm=TRUE) + (barlength/2)
      ),
      color="black", fill="gray75"
    ) +
    scale_x_date(
      date_breaks = "year", date_labels = "%Y"
    ) +
    scale_color_okabe_ito() +
    scale_fill_okabe_ito() +
    theme_bw(base_size = 18) +
    labs(
      color = "District",
      fill = "District",
      x = "Date (month)",
      y = "Trend"
    )
  
  # Plot seasonal component
  p_season <- pred_df |> 
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_line(
      aes(x=date, y=season, color=district), linewidth=0.8
    ) +
    geom_ribbon(
      aes(x=date, ymin=season_conf.low, ymax=season_conf.high, fill=district),
      alpha=0.2
    ) +
    geom_rect(
      aes(
        xmin=max(date)+60, xmax=max(date)+90,
        ymin=mean(season, na.rm=TRUE) - (barlength/2),
        ymax=mean(season, na.rm=TRUE) + (barlength/2)
      ),
      color="black", fill="gray75"
    ) +
    scale_x_date(
      date_breaks = "year", date_labels = "%Y"
    ) +
    scale_color_okabe_ito() +
    scale_fill_okabe_ito() +
    theme_bw(base_size = 18) +
    labs(
      color = "District",
      fill = "District",
      x = "Date (month)",
      y = "Seasonality"
    )
  
  
  # Merge plots
  p_joint <- p_fit / (p_trend + theme(legend.position = "none")) /
    (p_season + theme(legend.position = "none"))  +
    plot_layout(axes = "collect", guides="collect",
                heights = c(4,3,3))
  
  p_joint
  
  # Save plot
  ggsave(
    file.path(folder_path,
              glue("disease_{print_name}_gam_decomposition.png")),
    width=16, height=16*0.700
  )
}

pwalk(
  df_disease_an,
  plot_ts_decomp_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
)


#' 
#' 
#' #### Make contrasts for the trend
#' 
## ---------------------------------------------------------------------------------------
# Step 7: Make contrasts for the trend -------------------------------------------------
# note: Standard errors and hypothesis tests computed with Delta method
# (marginaleffects default)

# Check time indices for contrast
# df_disease_model |>
#   filter(disease == "Bronchial Asthma") |> 
#   filter(date %in% c("2014-01-01", "2022-12-01")) |>
#   print(width=Inf)


make_trend_contrasts_disease <- function(model, disease, print_name, df_model,
                                         date_range_contrast = c("2014-01-01", "2022-12-01"),
                                         folder_path=file.path(res_path, "plots", "disease_descriptive"), ...) {
  print(disease)
  
  # Get model indices for the start and end date of the contrast
  start_indices <- df_model |> 
    filter(date == date_range_contrast[1]) |> 
    head(1) |> 
    select(time_ind, month_ind)
  
  end_indices <- df_model |> 
    filter(date == date_range_contrast[2]) |> 
    head(1) |> 
    select(time_ind, month_ind)
  
  # Exclude terms to make predictions for the trend
  exclude_set <- c(
    as.character(glue("ti(month_ind):district{levels(model$model$district)}")),
    as.character(glue("ti(time_ind,month_ind):district{levels(model$model$district)}"))
  )
  
  # Average slope ----------------------------------------------------------------------
  
  # Average change in the trend per 1 year increase
  avg_yearly_trend_slope <- avg_comparisons(
    model,
    variables = list(time_ind = 12),
    by = "district",
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    mutate(
      name = "avg_yearly_trend_slope"
    )
  
  avg_yearly_trend_slope
  
  
  # Difference in the slopes (Moshi - Siha)
  avg_yearly_trend_slope_diff <- avg_comparisons(
    model,
    variables = list(time_ind = 12),
    by = "district",
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
    # Test for the equality of both estimates
    hypothesis = "b2 - b1 = 0"
  ) |> 
    as_tibble() |> 
    mutate(
      name = "avg_yearly_trend_slope_diff"
    )
  
  avg_yearly_trend_slope_diff
  
  
  # Plot yearly trend change before taking the average by district ---.
  
  # plot_comparisons(
  #   model,
  #   variables = list(time_ind = 12),
  #   by = c("time_ind", "district"),
  #   comparison = "difference",
  #   type = "response",
  #   exclude = exclude_set,
  # )
  
  p_yearly_trend_slope <- comparisons(
    model,
    newdata = df_model,
    variables = list(time_ind = 12),
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_hline(yintercept = 0) +
    geom_line(
      aes(x=date, y=estimate, color=district), linewidth=0.8
    ) +
    geom_ribbon(
      aes(x=date, ymin=conf.low, ymax=conf.high, fill=district),
      alpha=0.2
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y"
    ) +
    scale_color_okabe_ito() +
    scale_fill_okabe_ito() +
    theme_bw(base_size = 18) +
    theme(
        axis.title.y = element_text(size=rel(0.9))
    ) +
    labs(
      color = "District",
      fill = "District",
      x = "Date (month)",
      y = glue("{disease}\nincidence rate\n(per 100k people)\nyearly trend change")
    )
  
  # Average trend difference -----------------------------------------------------------
  
  # Average trends
  avg_trend <- avg_predictions(
    model,
    variables = "district",
    type = "response",
    exclude = exclude_set
  ) |> 
    as_tibble() |> 
    mutate(
      name = "avg_trend"
    )
  
  avg_trend
  
  # Average difference in trends
  avg_trend_diff <- avg_comparisons(
    model,
    variables = "district",
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    mutate(
      name = "avg_trend_diff"
    )
  
  avg_trend_diff
  
  # Plot trend difference before taking the average by district ----.
  
  # plot_comparisons(
  #   model,
  #   variables = c("district"),
  #   by = c("time_ind"),
  #   comparison = "difference",
  #   type = "response",
  #   exclude = exclude_set,
  # )

  p_trend_diff <- comparisons(
    model,
    newdata = df_model,
    variables = "district",
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_hline(yintercept = 0) +
    geom_line(
      aes(x=date, y=estimate), linewidth=0.8
    ) +
    geom_ribbon(
      aes(x=date, ymin=conf.low, ymax=conf.high),
      alpha=0.2
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y"
    ) +
    theme_bw(base_size = 18) +
    theme(
        axis.title.y = element_text(size=rel(0.9))
    ) +
    labs(
      color = "District",
      fill = "District",
      x = "Date (month)",
      y = glue("{disease}\nincidence rate\n(per 100k people)\ntrend difference (Siha - Moshi)")
    )
  
  # Trend difference between Start and End ---------------------------------------
  
  # Query explanation:
  # For the observations we had at 2014-01 (time_ind==1, month_ind==1),
  # what is the effect on the trend of changing time_ind to 108 and month_ind to 12 (date being 2022-12)
  
  # Temporal difference
  temp_trend_diff <- comparisons(
    model,
    newdata = filter(df_model, time_ind==1),
    variables = list(
      time_ind = c(start_indices[["time_ind"]], end_indices[["time_ind"]]),
      month_ind = c(start_indices[["month_ind"]], end_indices[["month_ind"]])
    ),
    cross = TRUE,
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    relocate(district) |> 
    mutate(
      name = "temp_trend_diff"
    )
  
  temp_trend_diff
  
  # Difference in temporal differences
  temp_trend_diff_diff <- comparisons(
    model,
    newdata = filter(df_model, time_ind==1),
    variables = list(
      time_ind = c(start_indices[["time_ind"]], end_indices[["time_ind"]]),
      month_ind = c(start_indices[["month_ind"]], end_indices[["month_ind"]])
    ),
    cross = TRUE,
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
    # Test for the equality of both estimates
    hypothesis = "b2 - b1 = 0"
  ) |> 
    as_tibble() |> 
    mutate(
      name = "temp_trend_diff_diff"
    )
  
  temp_trend_diff_diff
  
  # Plot the trends
  p_trends <- predictions(
    model,
    newdata = df_model,
    type = "response",
    exclude = exclude_set,
  ) |> 
    as_tibble() |> 
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      xintercept = as.Date(date_range_contrast), linewidth=1
    ) +
    geom_line(
      aes(x=date, y=estimate, color=district), linewidth=0.8
    ) +
    geom_ribbon(
      aes(x=date, ymin=conf.low, ymax=conf.high, fill=district),
      alpha=0.2
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y"
    ) +
    scale_color_okabe_ito() +
    scale_fill_okabe_ito() +
    theme_bw(base_size = 18) +
    theme(
        axis.title.y = element_text(size=rel(0.9))
    ) +
    labs(
      color = "District",
      fill = "District",
      x = "Date (month)",
      y = glue("{disease}\nincidence rate\n(per 100k people)\ntrend")
    )
  
  # Merge results ----------------------------------------------------------------------
  
  # Combine earlier plots
  p_joint <- p_trends / p_yearly_trend_slope / p_trend_diff +
    plot_layout(axes="collect")
  
  ggsave(
    plot=p_joint,
    filename=file.path(
      folder_path,
      glue("disease_{print_name}_trend_contrasts.png")
    ),
    width=16, height=16*0.700
  )
  
  # Combine tables
  contrast_res <- bind_rows(
    avg_yearly_trend_slope,
    avg_yearly_trend_slope_diff,
    avg_trend,
    avg_trend_diff,
    temp_trend_diff,
    temp_trend_diff_diff
  ) |>
    select(! c(predicted_lo:predicted, date:time_ind)) |> 
    relocate(name)
  
  return(contrast_res)
}

#' 
#' 
## ---------------------------------------------------------------------------------------
df_disease_an <- df_disease_an |> 
  mutate(
    contrast_res = pmap(
      list(model, disease, print_name, df_model),
      make_trend_contrasts_disease,
      date_range_contrast = c("2014-01-01", "2022-12-01"),
      folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
    )
  )


# df_disease_an[["contrast_res"]][[1]] |> print(width=Inf)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Prepare table for contrasts
contrast_tbl_dis_raw <- df_disease_an |> 
  select(disease, contrast_res) |> 
  unnest(contrast_res) |> 
  select(disease, name, district, estimate, conf.low, conf.high, p.value) |> 
  # If checking average trend, H0 = 0 not informative, so remove p.value
  mutate(
    p.value = if_else(name=="avg_trend", NA, p.value)
  )

# All constrasts
prepare_contrast_table_full <- function(contrast_tbl_dis_raw, date_range_contrast_label="(2014-01 to 2022-12)") {
  contrast_tbl_dis_raw |> 
    mutate(
      Contrast = case_match(
        name,
        "avg_yearly_trend_slope" ~ "Average yearly trend change",
        "avg_yearly_trend_slope_diff" ~ "Average yearly trend change",
        "avg_trend" ~ "Average trend level",
        "avg_trend_diff" ~ "Average trend level",
        "temp_trend_diff" ~ glue("Trend difference {date_range_contrast_label}"),
        "temp_trend_diff_diff" ~ glue("Trend difference {date_range_contrast_label}"),
      ),
      Contrast = factor(
        Contrast, levels=c("Average trend level", "Average yearly trend change",
                           glue("Trend difference {date_range_contrast_label}"))
      ),
      District = if_else(is.na(district), "Siha - Moshi", district),
      `Estimate (95% CI)` = glue("{round(estimate, 3)} ({round(conf.low, 3)}, {round(conf.high, 3)})"),
      star = case_when(
        p.value < 0.05 ~ "*",
        .default = ""
      ),
      `p value` = Hmisc::format.pval(p.value, na.form="-", digits=4, eps=1e-4),
      `p value` = paste0(`p value`, star)
    ) |> 
    select(- c(estimate, conf.low, conf.high, p.value, district, name, star)) |> 
    arrange(Contrast) |> 
    gt(
      groupname_col = "disease",
      rowname_col = "Contrast"
    ) |> 
    cols_align(align = "left", columns = "Contrast")
}


contrast_tbl_dis_full <- prepare_contrast_table_full(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2014-01 to 2022-12)")

contrast_tbl_dis_full

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_full,
    file.path(res_path, "tables", "2014_onwards",
              glue("disease_contrasts_full.{extension}"))
    )
)

# p.value < 0.05
prepare_contrast_table_filt <- function(contrast_tbl_dis_raw, date_range_contrast_label="(2014-01 to 2022-12)") {
  contrast_tbl_dis_raw |>
    filter(p.value < 0.05) |> 
    mutate(
      Contrast = case_match(
        name,
        "avg_yearly_trend_slope" ~ "Average yearly trend change",
        "avg_yearly_trend_slope_diff" ~ "Average yearly trend change",
        "avg_trend" ~ "Average trend level",
        "avg_trend_diff" ~ "Average trend level",
        "temp_trend_diff" ~ glue("Trend difference {date_range_contrast_label}"),
        "temp_trend_diff_diff" ~ glue("Trend difference {date_range_contrast_label}"),
      ),
      Contrast = factor(
        Contrast, levels=c("Average trend level", "Average yearly trend change",
                           glue("Trend difference {date_range_contrast_label}"))
      ),
      District = if_else(is.na(district), "Siha - Moshi", district),
      `Estimate (95% CI)` = glue("{round(estimate, 3)} ({round(conf.low, 3)}, {round(conf.high, 3)})"),
      star = case_when(
        p.value < 0.05 ~ "*",
        .default = ""
      ),
      `p value` = Hmisc::format.pval(p.value, na.form="-", digits=4, eps=1e-4),
      `p value` = paste0(`p value`, star)
    ) |> 
    select(- c(estimate, conf.low, conf.high, p.value, district, name, star)) |> 
    arrange(Contrast) |> 
    gt(
      groupname_col = "disease",
      rowname_col = "Contrast"
    ) |> 
    cols_align(align = "left", columns = "Contrast")
}

contrast_tbl_dis_filt <- prepare_contrast_table_filt(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2014-01 to 2022-12)")

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_filt,
    file.path(res_path, "tables", "2014_onwards",
              glue("disease_contrasts_filt.{extension}"))
    )
)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Save results
save(
  df_disease_an,
  df_disease_model,
  file = file.path(res_path, "R_output", "2014_onwards",
                   "disease_descriptive.RData")
)

# load(
#   file.path(res_path, "R_output", "2014_onwards", "disease_descriptive.RData")
# )

#' 
#' 
#' ### Per disease_communicable
#' 
## ---------------------------------------------------------------------------------------
# Setup data
df_disease <- read_csv(file.path(data_path, "processed", "disease_processed.csv"))
pop_imp <- read_csv(file.path(data_path, "processed", "population_imputed.csv"))


# Aggregate n_cases by disease_communicable status
# Rename 'disease_communicable' to 'disease', to keep the rest of the code unchanged
df_disease_model_comm <- df_disease |> 
  group_by(district, date, disease_communicable) |> 
  summarise(
    n_cases = sum(n_cases)
  ) |> 
  ungroup() |> 
  rename(
    disease = disease_communicable
  )

# Add population (2022 imputed) to disease dataframe
df_disease_model_comm <- df_disease_model_comm |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop_imp, year=year(date)),
    join_by(district, year)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

# Compute incidence rate per 100k people
df_disease_model_comm <- df_disease_model_comm |> 
  mutate(case_rate = n_cases / population * 1e5)

# Set up variables for modelling
df_disease_model_comm <- df_disease_model_comm |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )

df_disease_model_comm |> 
  print(width=Inf, n=13)

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------
# Dataframe to iterate over for the analyses

df_disease_comm_an <- tibble(
  disease = unique(df_disease_model_comm$disease),
  print_name = str_replace_all(disease, c(" "="-",
                                          "/"="-",
                                          "\\("="-",
                                          "\\)"="-",
                                          ","="-"))
)

# Add data filtered for each disease
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    df_model = map(disease, \(x) filter(df_disease_model_comm, disease==x))
  )

#' 
#' 
#' #### Plot the response
#' 
## ---------------------------------------------------------------------------------------
# Step 1: Plot the response --------------------------------------------------------------

pwalk(
  df_disease_comm_an,
  plot_response_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
)

#' 
#' 
#' #### Fit model
#' 
#' 
## ---------------------------------------------------------------------------------------
# Step 2: Fit the model ----------------------------------------------------------------

# Get number of basis functions for trend
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    k_trend = map(df_model, get_k_trend_disease)
  )

df_disease_comm_an[["k_trend"]]

# Add distribution family of the response
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    family = list(mgcv::nb(link="log"))
  )

# Fit models
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    model = pmap(list(disease, df_model, k_trend, family), fit_descriptive_gam_disease)
  )

#' 
#' 
#' #### Check model
#' 
## ---------------------------------------------------------------------------------------
# Step 3: Check the model --------------------------------------------------------------

pwalk(
  df_disease_comm_an,
  check_gam_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
)

#' 
#' #### Get predictions
#' 
## ---------------------------------------------------------------------------------------
# Step 4: Get predictions, as well as decomposition into trend, seasonal and residual ----

# Get predictions and time series decomposition
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    pred_df = pmap(list(model=model, model_df=df_model, var="case_rate"), get_pred_ts_decomp)
  )

#' 
#' 
#' 
#' #### Plot time series decomposition
#' 
## ---------------------------------------------------------------------------------------

# Step 5: Plot time series decomposition -----------------------------------------------

pwalk(
  df_disease_comm_an,
  plot_ts_decomp_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
)


#' 
#' 
#' #### Make contrasts for the trend
#' 
## ---------------------------------------------------------------------------------------
# Step 7: Make contrasts for the trend -------------------------------------------------
# note: Standard errors and hypothesis tests computed with Delta method
# (marginaleffects default)

df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    contrast_res = pmap(
      list(model, disease, print_name, df_model),
      make_trend_contrasts_disease,
      date_range_contrast = c("2014-01-01", "2022-12-01"),
      folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
      )
  )

# df_disease_comm_an[["contrast_res"]][[1]] |> print(width=Inf)

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------

# Prepare table for contrasts
contrast_tbl_dis_raw <- df_disease_comm_an |> 
  select(disease, contrast_res) |> 
  unnest(contrast_res) |> 
  select(disease, name, district, estimate, conf.low, conf.high, p.value) |> 
  # If checking average trend, H0 = 0 not informative, so remove p.value
  mutate(
    p.value = if_else(name=="avg_trend", NA, p.value)
  )

# All constrasts
contrast_tbl_dis_full <- prepare_contrast_table_full(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2014-01 to 2022-12)")

contrast_tbl_dis_full

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_full,
    file.path(res_path, "tables",
              "2014_onwards",
              glue("disease-communicable_contrasts_full.{extension}"))
    )
)

# p.value < 0.05
contrast_tbl_dis_filt <- prepare_contrast_table_filt(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2014-01 to 2022-12)")

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_filt,
    file.path(res_path, "tables",
              "2014_onwards",
              glue("disease-communicable_contrasts_filt.{extension}"))
    )
)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Save results
save(
  df_disease_comm_an,
  df_disease_model_comm,
  file = file.path(res_path, "R_output", "2014_onwards",
                   "disease-communicable_descriptive.RData")
)

# load(
#   file.path(res_path, "R_output", "2014_onwards", "disease-communicable_descriptive.RData")
# )

#' 
#' 
#' ### Per disease_group
#' 
## ---------------------------------------------------------------------------------------
# Setup data
df_disease <- read_csv(file.path(data_path, "processed", "disease_processed.csv"))
pop_imp <- read_csv(file.path(data_path, "processed", "population_imputed.csv"))


# Aggregate n_cases by disease_group status
# Rename 'disease_group' to 'disease', to keep the rest of the code unchanged
df_disease_model_group <- df_disease |> 
  group_by(district, date, disease_group) |> 
  summarise(
    n_cases = sum(n_cases)
  ) |> 
  ungroup() |> 
  rename(
    disease = disease_group
  )

# Add population (2022 imputed) to disease dataframe
df_disease_model_group <- df_disease_model_group |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop_imp, year=year(date)),
    join_by(district, year)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

# Compute incidence rate per 100k people
df_disease_model_group <- df_disease_model_group |> 
  mutate(case_rate = n_cases / population * 1e5)

# Set up variables for modelling
df_disease_model_group <- df_disease_model_group |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )

df_disease_model_group |> 
  print(width=Inf, n=13)


#' 
#' 
#' 
## ---------------------------------------------------------------------------------------
# Dataframe to iterate over for the analyses

df_disease_group_an <- tibble(
  disease = unique(df_disease_model_group$disease),
  print_name = str_replace_all(disease, c(" "="-",
                                          "/"="-",
                                          "\\("="-",
                                          "\\)"="-",
                                          ","="-"))
)

df_disease_group_an <- df_disease_group_an |> 
  mutate(
    print_name = glue("Group_{print_name}")
  )

# Add data filtered for each disease
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    df_model = map(disease, \(x) filter(df_disease_model_group, disease==x))
  )

#' 
#' 
#' #### Plot the response
#' 
## ---------------------------------------------------------------------------------------
# Step 1: Plot the response --------------------------------------------------------------

pwalk(
  df_disease_group_an,
  plot_response_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
)

#' 
#' #### Fit model
#' 
#' 
## ---------------------------------------------------------------------------------------
# Step 2: Fit the model ----------------------------------------------------------------

# Get number of basis functions for trend
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    k_trend = map(df_model, get_k_trend_disease)
  )

df_disease_group_an[["k_trend"]]

# Add distribution family of the response
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    family = list(mgcv::nb(link="log"))
  )

# Fit models
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    model = pmap(list(disease, df_model, k_trend, family), fit_descriptive_gam_disease)
  )

#' 
#' 
#' #### Check model
#' 
## ---------------------------------------------------------------------------------------
# Step 3: Check the model --------------------------------------------------------------

pwalk(
  df_disease_group_an,
  check_gam_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
)

#' 
#' #### Get predictions
#' 
## ---------------------------------------------------------------------------------------
# Step 4: Get predictions, as well as decomposition into trend, seasonal and residual ----

# Get predictions and time series decomposition
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    pred_df = pmap(list(model=model, model_df=df_model, var="case_rate"), get_pred_ts_decomp)
  )

#' 
#' 
#' 
#' #### Plot time series decomposition
#' 
## ---------------------------------------------------------------------------------------

# Step 5: Plot time series decomposition -----------------------------------------------

pwalk(
  df_disease_group_an,
  plot_ts_decomp_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
)

#' 
#' 
#' #### Make contrasts for the trend
#' 
## ---------------------------------------------------------------------------------------
# Step 7: Make contrasts for the trend -------------------------------------------------
# note: Standard errors and hypothesis tests computed with Delta method
# (marginaleffects default)

df_disease_group_an <- df_disease_group_an |> 
  mutate(
    contrast_res = pmap(
      list(model, disease, print_name, df_model),
      make_trend_contrasts_disease,
      date_range_contrast = c("2014-01-01", "2022-12-01"),
      folder_path=file.path(res_path, "plots", "disease_descriptive", "2014_onwards")
      )
  )

# df_disease_group_an[["contrast_res"]][[1]] |> print(width=Inf)

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------

# Prepare table for contrasts
contrast_tbl_dis_raw <- df_disease_group_an |> 
  select(disease, contrast_res) |> 
  unnest(contrast_res) |> 
  select(disease, name, district, estimate, conf.low, conf.high, p.value) |> 
  # If checking average trend, H0 = 0 not informative, so remove p.value
  mutate(
    p.value = if_else(name=="avg_trend", NA, p.value)
  )

# All constrasts
contrast_tbl_dis_full <- prepare_contrast_table_full(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2014-01 to 2022-12)")

contrast_tbl_dis_full

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_full,
    file.path(res_path, "tables",
              "2014_onwards",
              glue("disease-group_contrasts_full.{extension}"))
    )
)

# p.value < 0.05
contrast_tbl_dis_filt <- prepare_contrast_table_filt(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2014-01 to 2022-12)")

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_filt,
    file.path(res_path, "tables",
              "2014_onwards",
              glue("disease-group_contrasts_filt.{extension}"))
    )
)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Save results
save(
  df_disease_group_an,
  df_disease_model_group,
  file = file.path(res_path, "R_output",
                   "2014_onwards",
                   "disease-group_descriptive.RData")
)

# load(
#   file.path(res_path, "R_output", "2014_onwards", "disease-group_descriptive.RData")
# )

#' 
#' 
#' ## Filtered data (2015 onwards)
#' 
#' Year 2014 is unreliable regarding diagnoses.
#' 
#' 
#' 
#' ### Per disease
#' 
## ---------------------------------------------------------------------------------------
# Setup data

# Filtered year 2014
df_disease <- read_csv(file.path(data_path, "processed", "disease_processed_wo-2014.csv"))
pop_imp <- read_csv(file.path(data_path, "processed", "population_imputed.csv"))

# Add population (2022 imputed) to disease dataframe
df_disease_model <- df_disease |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop_imp, year=year(date)),
    join_by(district, year)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

# Compute incidence rate per 100k people
df_disease_model <- df_disease_model |> 
  mutate(case_rate = n_cases / population * 1e5)

# Set up variables for modelling
df_disease_model <- df_disease_model |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )


#' 
#' 
#' ##### Repeat incidence rate heatmap
#' 
## ---------------------------------------------------------------------------------------
df_disease_model |> 
  plot_disease_table(group_over = disease_communicable, group_under = disease_group)

ggsave(
  file.path(res_path, "plots",
            "disease_incidence-rate_final-groups_after-filtering_2015-onwards.png"),
  width=22, height=22*0.530
)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Dataframe to iterate over for the analyses

df_disease_an <- tibble(
  disease = unique(df_disease_model$disease),
  print_name = str_replace_all(disease, c(" "="-",
                                          "/"="-",
                                          "\\("="-",
                                          "\\)"="-",
                                          ","="-"))
)

# Add data filtered for each disease
df_disease_an <- df_disease_an |> 
  mutate(
    df_model = map(disease, \(x) filter(df_disease_model, disease==x))
  )


#' 
#' 
#' #### Plot the response
#' 
## ---------------------------------------------------------------------------------------
# Step 1: Plot the response --------------------------------------------------------------

# Redo the incidence rate plots after disease filtering
# (We have Malnutrition as an aggregate of diseases now)

pwalk(
  df_disease_an,
  plot_response_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
)

#' 
#' 
#' #### Fit model
#' 
#' 
## ---------------------------------------------------------------------------------------
# Step 2: Fit the model ----------------------------------------------------------------

# Get number of basis functions for trend
df_disease_an <- df_disease_an |> 
  mutate(
    k_trend = map(df_model, get_k_trend_disease)
  )

df_disease_an[["k_trend"]]

# Add distribution family of the response
df_disease_an <- df_disease_an |> 
  mutate(
    family = list(mgcv::nb(link="log"))
  )

# Fit models
df_disease_an <- df_disease_an |> 
  mutate(
    model = pmap(list(disease, df_model, k_trend, family), fit_descriptive_gam_disease)
  )

#' 
#' 
#' #### Check model
#' 
## ---------------------------------------------------------------------------------------
# Step 3: Check the model --------------------------------------------------------------

pwalk(
  df_disease_an,
  check_gam_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
)


#' 
#' 
#' #### Get predictions
#' 
## ---------------------------------------------------------------------------------------
# Step 4: Get predictions, as well as decomposition into trend, seasonal and residual ----

# Get predictions and time series decomposition
df_disease_an <- df_disease_an |> 
  mutate(
    pred_df = pmap(list(model=model, model_df=df_model, var="case_rate"), get_pred_ts_decomp)
  )

#' 
#' 
#' 
#' #### Plot time series decomposition
#' 
## ---------------------------------------------------------------------------------------

# Step 5: Plot time series decomposition -----------------------------------------------

pwalk(
  df_disease_an,
  plot_ts_decomp_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
)


#' 
#' 
#' #### Make contrasts for the trend
#' 
## ---------------------------------------------------------------------------------------
# Step 7: Make contrasts for the trend -------------------------------------------------
# note: Standard errors and hypothesis tests computed with Delta method
# (marginaleffects default)

# Check time indices for contrast
# df_disease_model |>
#   filter(disease == "Chronic Respiratory Disease") |>
#   filter(date %in% c("2015-01-01", "2022-12-01")) |>
#   print(width=Inf)


df_disease_an <- df_disease_an |> 
  mutate(
    contrast_res = pmap(
      list(model, disease, print_name, df_model),
      make_trend_contrasts_disease,
      date_range_contrast = c("2015-01-01", "2022-12-01"),
      folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
    )
  )

# df_disease_an[["contrast_res"]][[1]] |> print(width=Inf)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Prepare table for contrasts
contrast_tbl_dis_raw <- df_disease_an |> 
  select(disease, contrast_res) |> 
  unnest(contrast_res) |> 
  select(disease, name, district, estimate, conf.low, conf.high, p.value) |> 
  # If checking average trend, H0 = 0 not informative, so remove p.value
  mutate(
    p.value = if_else(name=="avg_trend", NA, p.value)
  )

# All constrasts
contrast_tbl_dis_full <- prepare_contrast_table_full(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2015-01 to 2022-12)")

contrast_tbl_dis_full

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_full,
    file.path(res_path, "tables", "2015_onwards",
              glue("disease_contrasts_full.{extension}"))
    )
)

# p.value < 0.05
contrast_tbl_dis_filt <- prepare_contrast_table_filt(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2015-01 to 2022-12)")

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_filt,
    file.path(res_path, "tables", "2015_onwards",
              glue("disease_contrasts_filt.{extension}"))
    )
)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Save results
save(
  df_disease_an,
  df_disease_model,
  file = file.path(res_path, "R_output", "2015_onwards",
                   "disease_descriptive.RData")
)

# load(
#   file.path(res_path, "R_output", "2015_onwards", "disease_descriptive.RData")
# )

#' 
#' 
#' ### Per disease_communicable
#' 
## ---------------------------------------------------------------------------------------
# Setup data

# Filtered year 2014
df_disease <- read_csv(file.path(data_path, "processed", "disease_processed_wo-2014.csv"))
pop_imp <- read_csv(file.path(data_path, "processed", "population_imputed.csv"))


# Aggregate n_cases by disease_communicable status
# Rename 'disease_communicable' to 'disease', to keep the rest of the code unchanged
df_disease_model_comm <- df_disease |> 
  group_by(district, date, disease_communicable) |> 
  summarise(
    n_cases = sum(n_cases)
  ) |> 
  ungroup() |> 
  rename(
    disease = disease_communicable
  )

# Add population (2022 imputed) to disease dataframe
df_disease_model_comm <- df_disease_model_comm |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop_imp, year=year(date)),
    join_by(district, year)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

# Compute incidence rate per 100k people
df_disease_model_comm <- df_disease_model_comm |> 
  mutate(case_rate = n_cases / population * 1e5)

# Set up variables for modelling
df_disease_model_comm <- df_disease_model_comm |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )

df_disease_model_comm |> 
  print(width=Inf, n=13)

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------
# Dataframe to iterate over for the analyses

df_disease_comm_an <- tibble(
  disease = unique(df_disease_model_comm$disease),
  print_name = str_replace_all(disease, c(" "="-",
                                          "/"="-",
                                          "\\("="-",
                                          "\\)"="-",
                                          ","="-"))
)

# Add data filtered for each disease
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    df_model = map(disease, \(x) filter(df_disease_model_comm, disease==x))
  )

#' 
#' 
#' #### Plot the response
#' 
## ---------------------------------------------------------------------------------------
# Step 1: Plot the response --------------------------------------------------------------

pwalk(
  df_disease_comm_an,
  plot_response_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
)

#' 
#' 
#' #### Fit model
#' 
#' 
## ---------------------------------------------------------------------------------------
# Step 2: Fit the model ----------------------------------------------------------------

# Get number of basis functions for trend
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    k_trend = map(df_model, get_k_trend_disease)
  )

df_disease_comm_an[["k_trend"]]

# Add distribution family of the response
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    family = list(mgcv::nb(link="log"))
  )

# Fit models
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    model = pmap(list(disease, df_model, k_trend, family), fit_descriptive_gam_disease)
  )

#' 
#' 
#' #### Check model
#' 
## ---------------------------------------------------------------------------------------
# Step 3: Check the model --------------------------------------------------------------

pwalk(
  df_disease_comm_an,
  check_gam_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
)

#' 
#' #### Get predictions
#' 
## ---------------------------------------------------------------------------------------
# Step 4: Get predictions, as well as decomposition into trend, seasonal and residual ----

# Get predictions and time series decomposition
df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    pred_df = pmap(list(model=model, model_df=df_model, var="case_rate"), get_pred_ts_decomp)
  )

#' 
#' 
#' 
#' #### Plot time series decomposition
#' 
## ---------------------------------------------------------------------------------------

# Step 5: Plot time series decomposition -----------------------------------------------

pwalk(
  df_disease_comm_an,
  plot_ts_decomp_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
)


#' 
#' 
#' #### Make contrasts for the trend
#' 
## ---------------------------------------------------------------------------------------
# Step 7: Make contrasts for the trend -------------------------------------------------
# note: Standard errors and hypothesis tests computed with Delta method
# (marginaleffects default)

df_disease_comm_an <- df_disease_comm_an |> 
  mutate(
    contrast_res = pmap(
      list(model, disease, print_name, df_model),
      make_trend_contrasts_disease,
      date_range_contrast = c("2015-01-01", "2022-12-01"),
      folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
      )
  )

# df_disease_comm_an[["contrast_res"]][[1]] |> print(width=Inf)

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------

# Prepare table for contrasts
contrast_tbl_dis_raw <- df_disease_comm_an |> 
  select(disease, contrast_res) |> 
  unnest(contrast_res) |> 
  select(disease, name, district, estimate, conf.low, conf.high, p.value) |> 
  # If checking average trend, H0 = 0 not informative, so remove p.value
  mutate(
    p.value = if_else(name=="avg_trend", NA, p.value)
  )

# All constrasts
contrast_tbl_dis_full <- prepare_contrast_table_full(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2015-01 to 2022-12)")

contrast_tbl_dis_full

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_full,
    file.path(res_path, "tables",
              "2015_onwards",
              glue("disease-communicable_contrasts_full.{extension}"))
    )
)

# p.value < 0.05
contrast_tbl_dis_filt <- prepare_contrast_table_filt(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2015-01 to 2022-12)")

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_filt,
    file.path(res_path, "tables",
              "2015_onwards",
              glue("disease-communicable_contrasts_filt.{extension}"))
    )
)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Save results
save(
  df_disease_comm_an,
  df_disease_model_comm,
  file = file.path(res_path, "R_output", "2015_onwards",
                   "disease-communicable_descriptive.RData")
)

# load(
#   file.path(res_path, "R_output", "2015_onwards", "disease-communicable_descriptive.RData")
# )

#' 
#' 
#' ### Per disease_group
#' 
## ---------------------------------------------------------------------------------------
# Setup data

# Filtered year 2014
df_disease <- read_csv(file.path(data_path, "processed", "disease_processed_wo-2014.csv"))
pop_imp <- read_csv(file.path(data_path, "processed", "population_imputed.csv"))


# Aggregate n_cases by disease_group status
# Rename 'disease_group' to 'disease', to keep the rest of the code unchanged
df_disease_model_group <- df_disease |> 
  group_by(district, date, disease_group) |> 
  summarise(
    n_cases = sum(n_cases)
  ) |> 
  ungroup() |> 
  rename(
    disease = disease_group
  )

# Add population (2022 imputed) to disease dataframe
df_disease_model_group <- df_disease_model_group |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop_imp, year=year(date)),
    join_by(district, year)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

# Compute incidence rate per 100k people
df_disease_model_group <- df_disease_model_group |> 
  mutate(case_rate = n_cases / population * 1e5)

# Set up variables for modelling
df_disease_model_group <- df_disease_model_group |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )

df_disease_model_group |> 
  print(width=Inf, n=13)


#' 
#' 
#' 
## ---------------------------------------------------------------------------------------
# Dataframe to iterate over for the analyses

df_disease_group_an <- tibble(
  disease = unique(df_disease_model_group$disease),
  print_name = str_replace_all(disease, c(" "="-",
                                          "/"="-",
                                          "\\("="-",
                                          "\\)"="-",
                                          ","="-"))
)

df_disease_group_an <- df_disease_group_an |> 
  mutate(
    print_name = glue("Group_{print_name}")
  )

# Add data filtered for each disease
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    df_model = map(disease, \(x) filter(df_disease_model_group, disease==x))
  )

#' 
#' 
#' #### Plot the response
#' 
## ---------------------------------------------------------------------------------------
# Step 1: Plot the response --------------------------------------------------------------

pwalk(
  df_disease_group_an,
  plot_response_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
)

#' 
#' #### Fit model
#' 
#' 
## ---------------------------------------------------------------------------------------
# Step 2: Fit the model ----------------------------------------------------------------

# Get number of basis functions for trend
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    k_trend = map(df_model, get_k_trend_disease)
  )

df_disease_group_an[["k_trend"]]

# Add distribution family of the response
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    family = list(mgcv::nb(link="log"))
  )

# Fit models
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    model = pmap(list(disease, df_model, k_trend, family), fit_descriptive_gam_disease)
  )

#' 
#' 
#' #### Check model
#' 
## ---------------------------------------------------------------------------------------
# Step 3: Check the model --------------------------------------------------------------

pwalk(
  df_disease_group_an,
  check_gam_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
)

#' 
#' #### Get predictions
#' 
## ---------------------------------------------------------------------------------------
# Step 4: Get predictions, as well as decomposition into trend, seasonal and residual ----

# Get predictions and time series decomposition
df_disease_group_an <- df_disease_group_an |> 
  mutate(
    pred_df = pmap(list(model=model, model_df=df_model, var="case_rate"), get_pred_ts_decomp)
  )

#' 
#' 
#' 
#' #### Plot time series decomposition
#' 
## ---------------------------------------------------------------------------------------

# Step 5: Plot time series decomposition -----------------------------------------------

pwalk(
  df_disease_group_an,
  plot_ts_decomp_disease,
  folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
)

#' 
#' 
#' #### Make contrasts for the trend
#' 
## ---------------------------------------------------------------------------------------
# Step 7: Make contrasts for the trend -------------------------------------------------
# note: Standard errors and hypothesis tests computed with Delta method
# (marginaleffects default)

df_disease_group_an <- df_disease_group_an |> 
  mutate(
    contrast_res = pmap(
      list(model, disease, print_name, df_model),
      make_trend_contrasts_disease,
      date_range_contrast = c("2015-01-01", "2022-12-01"),
      folder_path=file.path(res_path, "plots", "disease_descriptive", "2015_onwards")
      )
  )

# df_disease_group_an[["contrast_res"]][[1]] |> print(width=Inf)

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------

# Prepare table for contrasts
contrast_tbl_dis_raw <- df_disease_group_an |> 
  select(disease, contrast_res) |> 
  unnest(contrast_res) |> 
  select(disease, name, district, estimate, conf.low, conf.high, p.value) |> 
  # If checking average trend, H0 = 0 not informative, so remove p.value
  mutate(
    p.value = if_else(name=="avg_trend", NA, p.value)
  )

# All constrasts
contrast_tbl_dis_full <- prepare_contrast_table_full(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2015-01 to 2022-12)")

contrast_tbl_dis_full

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_full,
    file.path(res_path, "tables",
              "2015_onwards",
              glue("disease-group_contrasts_full.{extension}"))
    )
)

# p.value < 0.05
contrast_tbl_dis_filt <- prepare_contrast_table_filt(contrast_tbl_dis_raw,
                                                     date_range_contrast_label="(2015-01 to 2022-12)")

# Save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    contrast_tbl_dis_filt,
    file.path(res_path, "tables",
              "2015_onwards",
              glue("disease-group_contrasts_filt.{extension}"))
    )
)

#' 
#' 
## ---------------------------------------------------------------------------------------
# Save results
save(
  df_disease_group_an,
  df_disease_model_group,
  file = file.path(res_path, "R_output",
                   "2015_onwards",
                   "disease-group_descriptive.RData")
)

# load(
#   file.path(res_path, "R_output", "2015_onwards", "disease-group_descriptive.RData")
# )

#' 
