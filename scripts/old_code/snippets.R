sm <- smooth_estimates(mod) |> 
  add_confint()

# sm |> 
#   pivot_wider(
#     names_from = .smooth,
#     values_from = .estimate
#     ) |> 
#   print(width = Inf)


sm$.smooth |> unique()

sm |> 
  ggplot() +
  # Trend Moshi
  geom_line(
    data=filter(sm, .smooth=="s(time_ind):districtMoshi"),
    aes(
      x=time_ind,
      y=.estimate + coef(mod)["(Intercept)"],
      color=district
      )
  ) +
  geom_ribbon(
    data=filter(sm, .smooth=="s(time_ind):districtMoshi"),
    aes(
      x=time_ind,
      ymin=.lower_ci + coef(mod)["(Intercept)"],
      ymax=.upper_ci + coef(mod)["(Intercept)"],
      fill=district
    ), alpha=0.2
  ) +
  # Trend Siha
  geom_line(
    data=filter(sm, .smooth=="s(time_ind):districtSiha"),
    aes(
      x=time_ind,
      y=.estimate + coef(mod)["(Intercept)"] + coef(mod)["districtSiha"],
      color=district
      )
  ) +
  geom_ribbon(
    data=filter(sm, .smooth=="s(time_ind):districtSiha"),
    aes(
      x=time_ind,
      ymin=.lower_ci + coef(mod)["(Intercept)"] + coef(mod)["districtSiha"],
      ymax=.upper_ci + coef(mod)["(Intercept)"] + coef(mod)["districtSiha"],
      fill=district
      ), alpha=0.2
  )


sm |> 
  ggplot() +
  # Seasonal Moshi
  geom_line(
    data=filter(sm, .smooth=="te(year_ind,month_ind):districtMoshi"),
    aes(
      x=time_ind,
      y=.estimate + coef(mod)["(Intercept)"],
      color=district
      )
  ) +
  geom_ribbon(
    data=filter(sm, .smooth=="te(year_ind,month_ind):districtMoshi"),
    aes(
      x=time_ind,
      ymin=.lower_ci + coef(mod)["(Intercept)"],
      ymax=.upper_ci + coef(mod)["(Intercept)"],
      fill=district
    ), alpha=0.2
  ) +
  # Trend Siha
  geom_line(
    data=filter(sm, .smooth=="te(year_ind,month_ind):districtSiha"),
    aes(
      x=time_ind,
      y=.estimate + coef(mod)["(Intercept)"] + coef(mod)["districtSiha"],
      color=district
      )
  ) +
  geom_ribbon(
    data=filter(sm, .smooth=="te(year_ind,month_ind):districtSiha"),
    aes(
      x=time_ind,
      ymin=.lower_ci + coef(mod)["(Intercept)"] + coef(mod)["districtSiha"],
      ymax=.upper_ci + coef(mod)["(Intercept)"] + coef(mod)["districtSiha"],
      fill=district
      ), alpha=0.2
  )

# --------------------------------------------


# Raw time series decomposition

library(mgcv)
library(gratia)
library(marginaleffects)
library(patchwork)

# Prepare data for modelling
df_environ_model <- df_environ

df_environ_model <- df_environ_model |> 
  mutate(
    time_ind = interval(min(date), date) / months(1) + 1,
    year_ind = year(date) - min(year(date)) + 1,
    month_ind = month(date),
    district = factor(district, levels=c("Moshi", "Siha"))
  )

# DF to save the forecasts
df_environ_impute <- df_environ_model



# greenness


# Separate for Moshi and Siha

df_environ_model_siha <- filter(df_environ_model, district=="Siha")

df_environ_impute_siha <- df_environ_model_siha


# Step 1: Plot the response

df_environ_model_siha |> 
  ggplot() +
  geom_line(
    aes(x=date, y=greenness, color=district)
  ) +
  geom_point(
    aes(x=date, y=greenness, color=district)
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y",
               limits=as.Date(c("2012-01-01", "2022-01-01"))) +
  geom_vline(xintercept = unique(floor_date(df_environ_model_siha$date, "year")),
             color="gray1", linetype=2, linewidth=0.8)


# Step 2: Fit the model

mod <- gam(
  # greenness ~ s(time_ind, bs="cr", k=10) + te(year_ind, month_ind, bs=c("cr", "cc"), k=c(10,12)),
  # greenness ~ s(time_ind, bs="cr", k=10) + te(year_ind, month_ind, bs=c("cr", "cc"), k=c(3,12)),
  # greenness ~ s(time_ind, bs="cr", k=10) + s(month_ind, bs=c("cc"), k=c(12)),
  greenness ~ ti(time_ind, bs="cr", k=10) + ti(month_ind, bs="cc", k=12) + 
    ti(time_ind, month_ind, bs=c("cr", "cc"), k=c(10,12)),
  # greenness ~ s(time_ind, bs="cr", k=10) + s(month_ind, bs=c("cc"), k=c(12)),
  data=df_environ_model_siha,
  family = gaussian(),
  method = "REML",
  knots = list(month_ind = c(0,12))
)

# Step 3: Check the model

mod
summary(mod)

gratia::appraise(mod)

# Step 4: Get predictions for missings and forecasts up to 2021 (inclusive)

# Save predicted greenness
df_environ_impute_siha <- bind_cols(
  df_environ_impute_siha,
  greenness_imp = predict(mod, type="response", newdata = df_environ_model_siha)
)

# Visualize imputation
df_environ_impute_siha |> 
  mutate(
    imputed = if_else(is.na(greenness), "Yes", "No")
  ) |> 
  ggplot() +
  geom_point(
    aes(x=date, y=greenness_imp, color=district, shape=imputed), size=2.5
  ) +
  geom_line(
    aes(x=date, y=greenness_imp, color=district)
  ) +
  scale_shape_manual(
    values = c(16,4)
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y",
               limits=as.Date(c("2012-01-01", "2021-12-01"))) +
  geom_vline(xintercept = unique(floor_date(df_environ_model_siha$date, "year")),
             color="gray1", linetype=2, linewidth=0.8)


# Step 5: Get predictions for trend and seasonal

mod |> draw()

pred_terms <- predict(mod, newdata=df_environ_model_siha, type="terms", se.fit = TRUE)
pred_fit <- predict(mod, newdata=df_environ_model_siha, type="response", se.fit = TRUE)


pred_df <- df_environ_model_siha |> 
  mutate(
    pred = with(pred_fit, fit),
    pred_high = with(pred_fit, fit + qnorm(0.975)*se.fit),
    pred_low = with(pred_fit, fit + qnorm(0.025)*se.fit),
    trend = with(pred_terms, fit[,1] + attr(pred_terms, "constant")),
    trend_high = with(pred_terms, fit[,1] + qnorm(0.975)*se.fit[,1] + attr(pred_terms, "constant")),
    trend_low = with(pred_terms, fit[,1] + qnorm(0.025)*se.fit[,1] + attr(pred_terms, "constant")),
    # season = with(pred_terms, fit[,2] + fit[,3] + attr(pred_terms, "constant")),
    season = with(pred_terms, fit[,2] + fit[,3]),
    # season_high = with(pred_terms, fit[,2] + qnorm(0.975)*se.fit[,2] + 
    #                      fit[,3] + qnorm(0.975)*se.fit[,3] +
    #                      attr(pred_terms, "constant")),
    season_high = with(pred_terms, fit[,2] + qnorm(0.975)*se.fit[,2] + 
                         fit[,3] + qnorm(0.975)*se.fit[,3]),
    # season_low = with(pred_terms, fit[,2] + qnorm(0.025)*se.fit[,2] +
    #                     fit[,3] + qnorm(0.025)*se.fit[,3] + 
    #                     attr(pred_terms, "constant"))
    season_low = with(pred_terms, fit[,2] + qnorm(0.025)*se.fit[,2] +
                        fit[,3] + qnorm(0.025)*se.fit[,3])
  )


# Add residuals
pred_df <- pred_df |> 
  left_join(cbind(mod$model, residuals = mod$residuals))


barlength <- select(pred_df, c(greenness, pred:residuals)) |> 
  sapply(\(x) diff(range(x, na.rm=TRUE))) |> 
  min()

pred_df |> 
  ggplot() +
  geom_point(
    aes(x=date, y=greenness)
  ) +
  geom_line(
    aes(x=date, y=greenness)
  ) +
  geom_rect(
    aes(
      xmin=max(date)+60, xmax=max(date)+90,
      ymin=mean(greenness, na.rm=TRUE) - (barlength/2),
      ymax=mean(greenness, na.rm=TRUE) + (barlength/2)
    ),
    color="black", fill="gray75"
  )

p1 <- pred_df |> 
  ggplot() +
  geom_point(
    aes(x=date, y=greenness)
  ) +
  geom_line(
    aes(x=date, y=pred)
  ) +
  geom_ribbon(
    aes(x=date, ymin=pred_low, ymax=pred_high),
    alpha=0.2
  ) +
  geom_rect(
    aes(
      xmin=max(date)+60, xmax=max(date)+90,
      ymin=mean(greenness, na.rm=TRUE) - (barlength/2),
      ymax=mean(greenness, na.rm=TRUE) + (barlength/2)
    ),
    color="black", fill="gray75"
  )




p2 <- pred_df |> 
  ggplot() +
  geom_line(
    aes(x=date, y=trend)
  ) +
  geom_ribbon(
    aes(x=date, ymin=trend_low, ymax=trend_high),
    alpha=0.2
  ) +
  geom_rect(
    aes(
      xmin=max(date)+60, xmax=max(date)+90,
      ymin=mean(trend, na.rm=TRUE) - (barlength/2),
      ymax=mean(trend, na.rm=TRUE) + (barlength/2)
    ),
    color="black", fill="gray75"
  )

p3 <- pred_df |> 
  ggplot() +
  geom_line(
    aes(x=date, y=season)
  ) +
  geom_ribbon(
    aes(x=date, ymin=season_low, ymax=season_high),
    alpha=0.2
  ) +
  geom_rect(
    aes(
      xmin=max(date)+60, xmax=max(date)+90,
      ymin=mean(season, na.rm=TRUE) - (barlength/2),
      ymax=mean(season, na.rm=TRUE) + (barlength/2)
    ),
    color="black", fill="gray75"
  )


p4 <- pred_df |> 
  ggplot() +
  geom_segment(
    aes(x=date, y=0, yend=residuals)
  ) +
  ylab("residuals") +
  geom_rect(
    aes(
      xmin=max(date)+60, xmax=max(date)+90,
      ymin=mean(residuals, na.rm=TRUE) - (barlength/2),
      ymax=mean(residuals, na.rm=TRUE) + (barlength/2)
    ),
    color="black", fill="gray75"
  )



p1 / p2 / p3 / p4 + plot_layout(axes = "collect")


# ----------------------------------------------------------------------------------------


# 01. Plot the responses

call_list <- list(
  "pm2p5" = list(var="pm2p5", df=df_environ_model, label="PM2.5 (\U03BCg/m\U00B3)"),
  "greenness" = list(var="greenness", df=df_environ_model, label="Greenness (NDVI)"),
  "temp_min" = list(var="temp_min", df=df_environ_model, label="Min. temperature (ºC)"),
  "temp_mean" = list(var="temp_mean", df=df_environ_model, label="Mean temperature (ºC)"),
  "temp_max" = list(var="temp_max", df=df_environ_model, label="Max. temperature (ºC)"),
  "utci" = list(var="utci", df=df_environ_model, label="UTCI"),
  "total_rainfall" = list(var="total_rainfall", df=df_environ_model, label="Rainfall (mm)"),
  "n_raindays" = list(var="n_raindays", df=df_environ_model, label="No. rain days")
)


plot_response <- function(var, df, label) {
  
  # Rename response to 'y'
  df <- rename(df, y := {{var}})
  
  # Date range for which there are observations
  date_range_plot <- df |> 
    filter(! is.na(y)) |> 
    pull(date) |> 
    range()
  
  # Plot the response ------------------------------------------------------------
  
  df |>
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_line(
      aes(x=date, y=y, color=district), linewidth=0.8
    ) +
    geom_point(
      aes(x=date, y=y, color=district), size=2.5
    ) +
    scale_x_date(
      date_breaks="year", date_labels = "%Y",
      limits = date_range_plot
    ) +
    scale_color_okabe_ito() +
    theme_bw(base_size = 18) +
    labs(
      color = "District",
      x = "Date",
      y = label
    )
  
  ggsave(
    file.path(res_path, "plots", glue("env_{var}_response.png")),
    width=16, height=16*0.600
  )
}

map(call_list, \(x) do.call(plot_response, x))



# ----
# Old environmental analysis


```{r}
call_list <- list(
  "pm2p5" = list(
    var="pm2p5",
    df=df_environ_model,
    label="PM2.5 (\U03BCg/m\U00B3)",
    family=gaussian()
  ),
  "greenness" = list(
    var="greenness",
    df=df_environ_model,
    label="Greenness (NDVI)",
    family=gaussian()
  ),
  "temp_min" = list(
    var="temp_min",
    df=df_environ_model,
    label="Min. temperature (ºC)",
    family=gaussian()
  ),
  "temp_mean" = list(
    var="temp_mean",
    df=df_environ_model,
    label="Mean temperature (ºC)",
    family=gaussian()
  ),
  "temp_max" = list(
    var="temp_max",
    df=df_environ_model,
    label="Max. temperature (ºC)",
    family=gaussian()
  ),
  "utci" = list(
    var="utci",
    df=df_environ_model,
    label="UTCI",
    family=gaussian()
  ),
  "total_rainfall" = list(
    var="total_rainfall",
    df=df_environ_model,
    label="Rainfall (mm)",
    family=Gamma(link="log"),
    transform = \(x) x+1,
    inv_transform = \(x) ifelse(x-1<0, 0, x-1)
  ),
  "n_raindays" = list(
    var="n_raindays",
    df=df_environ_model,
    label="No. rain days",
    family=mgcv::nb(link="log"),
    transform = \(x) x+1,
    inv_transform = \(x) ifelse(x-1<0, 0, x-1))
)

```



```{r}
env_analysis <- function(var, df, family, label,
                         transform=\(x) x, inv_transform=\(x) x){
  #' This function does descriptive analyses for each environmental variable
  #' 
  #' Arguments:
  #'  var: str, variable name
  #'  df: dataframe with the data
  #'  family: distribution of the response
  #'  label: str, label for the response
  #'  transform: function to transform the response (default: identity)
  #'  inv_transform: function to undo the transformation of the response (default: identity)
  #'  
  #'  Steps:
  #'    1: Plot the response
  #'    2: Fit the model
  #'    3: Validate model (plot diagnostics)
  #'    4: Compute predictions
  #'    5: Plot time series decomposition
  #'    6: Compute contrasts for the trend
  #'  
  #'  Returns:
  #'    A list with the model, predictions, plots and contrasts  
  
  
  cat(glue("
  \n\n\n
  ========================================================================================    
                        Doing analysis for environemntal variable:
                                        {label}
  ========================================================================================
  
  "))
  
  # Rename response to 'y'
  df <- rename(df, y := {{var}})
  
  # Date range for which there are observations
  date_range_plot <- df |> 
    filter(! is.na(y)) |> 
    pull(date) |> 
    range()
  
  # Step 1: Plot the response ------------------------------------------------------------
  
  p_var <- df |>
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_line(
      aes(x=date, y=y, color=district), linewidth=0.8
    ) +
    geom_point(
      aes(x=date, y=y, color=district), size=2.5
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
      y = label
    )
  
  ggsave(
    file.path(res_path, "plots", "environmental_descriptive", glue("env_{var}_response.png")),
    width=16, height=16*0.600
  )
  
  # Step 2: Fit the model ----------------------------------------------------------------
  
  # Penalized GAM
  # Contains penalized smooth functions of time and month
  # Function of time (trend) is cubic spline with 1 basis function per year
  # Function of month (seasonal component) is cyclic cubic splines with 12 basis functions
  # We allow the seasonal and trend components to interact, so the seasonal component can
  # change over time. The interaction is computed with a tensor product of the two functions
  
  # Number of basis functions for trend: 10 or 11
  k_trend <- df |>
    filter(! is.na(y)) |>
    pull(date) |>
    year() |>
    range() |>
    diff()+1
  
  # Formula
  form <- transform(y) ~ 0 + district +
    ti(time_ind, bs="cr", k=k_trend, by=district) +
    ti(month_ind, bs="cc", k=12, by=district) +
    ti(time_ind, month_ind, bs=c("cr", "cc"), k=c(k_trend,12), by=district)
  
  # Start and end points of monthly cyclic splines
  # note: Slightly out of c(1,12) to not impose that January (1) is equal to December (12)
  knots = list(month_ind = c(0.5, 12.5))
  
  mod <- gam(
    form,
    data=df,
    family = family,
    method = "REML",
    knots = knots
  )
  
  # Step 3: Check the model --------------------------------------------------------------
  
  # Check terms and significance
  print(summary(mod))
  # Check model assumptions
  p_diag <- gratia::appraise(mod)
  
  ggsave(
    plot=p_diag,
    file.path(res_path, "plots", "environmental_descriptive", glue("env_{var}_gam_diagnostics.png")),
    width=16, height=16*0.618
  )
  
  # Step 4: Get predictions, as well as decomposition into trend, seasonal and residual ----
  
  # note: Standard errors computed with Delta method (marginaleffects default)
  
  pred_df <- df
  
  # Get predictions for the response
  pred_name <- "pred"
  
  pred_df_sub <- predictions(
    mod,
    newdata = df,
    type = "response",
    # Apply inverse transformation
    transform = inv_transform
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
    mod,
    newdata = df,
    exclude = exclude_set,
    type = "response",
    # Do not apply inverse transformation (it should be applied to the full linear predictor)
    # transform = inv_transform
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
    mod,
    newdata = df,
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
    left_join(cbind(mod$model, residuals = residuals(mod, type="response")))
  
  
  # Step 5: Plot time series decomposition -----------------------------------------------
  
  # Reference bar for y scale
  barlength <- select(pred_df, c(y, pred:residuals)) |> 
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
      aes(x=date, y=y, color=district), size=2.5
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
        ymin=mean(y, na.rm=TRUE) - (barlength/2),
        ymax=mean(y, na.rm=TRUE) + (barlength/2)
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
      y = label
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
  p_resid <- pred_df |>
    filter(date >= date_range_plot[1] & date <= date_range_plot[2]) |> 
    ggplot() +
    geom_vline(
      aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
    ) +
    geom_vline(
      aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
    ) +
    geom_segment(
      aes(x=date, y=0, yend=residuals, color=district), linewidth= 1.5, lineend = "butt",
      position = position_dodge(width=20)
    ) +
    geom_hline(yintercept = 0) +
    ylab("residuals") +
    geom_rect(
      aes(
        xmin=max(date_range_plot)+60, xmax=max(date_range_plot)+90,
        ymin=mean(residuals, na.rm=TRUE) - (barlength/2),
        ymax=mean(residuals, na.rm=TRUE) + (barlength/2)
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
      x = "Date (month)",
      y = "Residuals"
    )
  
  
  # Merge plots
  p_joint <- p_fit / (p_trend + theme(legend.position = "none")) /
    (p_season + theme(legend.position = "none"))  +
    # (p_resid + theme(legend.position = "none")) +
    plot_layout(axes = "collect", guides="collect")
  
  ggsave(
    plot=p_joint,
    file.path(res_path, "plots", "environmental_descriptive", glue("env_{var}_gam_decomposition.png")),
    width=16, height=16*0.700
  )
  
  
  # Step 6: Make contrasts for the trend -------------------------------------------------
  # note: Standard errors computed with Delta method (marginaleffects default)
  
  # - Trend change between 2012-01 and 2021-12
  
  # -------------------------------------------.
  
  # - Trend change between 2012-01 and 2021-12
  
  # Check what values of time_ind and month_ind we need for the contrast
  # df |>
  #   filter(date %in% c("2012-01-01", "2021-12-01")) |>
  #   print(width=Inf)
  # time_ind=1, month_ind=1
  # time_ind=120, month_ind=12
  
  # Exclude terms to make predictions for the trend
  exclude_set <- c(
    as.character(glue("ti(month_ind):district{levels(pred_df$district)}")),
    as.character(glue("ti(time_ind,month_ind):district{levels(pred_df$district)}"))
  )
  
  cat("
    ------------------------------
    Doing contrasts for the trend:
    ------------------------------
    ")
  
  # Query explanation:
  # For the observations we had at 2012-01 (time_ind==1, month_ind==1),
  # what is the effect on the trend of changing time_ind to 120 and month_ind to 12 (date being 2021-12)
  
  # Difference
  contrast_trend_diff <- comparisons(
    mod,
    newdata = filter(df, time_ind==1),
    variables = list(time_ind = c(1, 120),
                     month_ind = c(1, 12)),
    cross = TRUE,
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
    
  ) |> 
    as_tibble()
  
  cat("
      ------------------------------------
      2012-01-01 to 2021-12-01 difference:
      
      ")
  contrast_trend_diff |> 
    print(width=Inf)
  
  # Difference in differences test
  contrast_trend_diff_diff <- comparisons(
    mod,
    newdata = filter(df, time_ind==1),
    variables = list(time_ind = c(1, 120),
                     month_ind = c(1, 12)),
    cross = TRUE,
    comparison = "difference",
    type = "response",
    exclude = exclude_set,
    # Test for the equality of both estimates
    hypothesis = "b1 = b2"
  ) |> 
    as_tibble()
  
  cat("
      ---------------------------------------------------
      2012-01-01 to 2021-12-01 difference in differences:
      
      ")
  contrast_trend_diff_diff |> 
    print(width=Inf)
  
  # Relative change
  contrast_trend_rc <- comparisons(
    mod,
    newdata = filter(df, time_ind==1),
    variables = list(time_ind = c(1, 120),
                     month_ind = c(1, 12)),
    cross = TRUE,
    comparison = "lift",
    type = "response",
    exclude = exclude_set
  ) |> 
    as_tibble()
  
  cat("
      -----------------------------------------
      2012-01-01 to 2021-12-01 relative change:
      
      ")
  contrast_trend_rc |> 
    print(width=Inf)
  
  # Difference in relative changes test
  contrast_trend_rc_diff <- comparisons(
    mod,
    newdata = filter(df, time_ind==1),
    variables = list(time_ind = c(1, 120),
                     month_ind = c(1, 12)),
    cross = TRUE,
    comparison = "lift",
    type = "response",
    exclude = exclude_set,
    hypothesis = "b1 = b2"
  ) |> 
    as_tibble()
  cat("
      -------------------------------------------------------
      2012-01-01 to 2021-12-01 difference in relative change:
      
      ")
  print(contrast_trend_rc_diff)
  
  pred_df |> print(width=Inf)
  
  # Results ------------------------------------------------------------------------------
  res <- list(
    mod = mod,
    pred_df = pred_df,
    plots = list(
      p_var = p_var,
      p_diag = p_diag,
      p_fit = p_fit,
      p_trend = p_trend,
      p_season = p_season,
      p_resid = p_resid,
      p_joint = p_joint
    ),
    contrasts = list(
      contrast_trend_diff = contrast_trend_diff,
      contrast_trend_diff_diff = contrast_trend_diff_diff,
      contrast_trend_rc = contrast_trend_rc,
      contrast_trend_rc_diff = contrast_trend_rc_diff
    )
  )
  
  return(res)
}

```

  
  
