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

