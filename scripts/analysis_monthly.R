
# Libraries

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
theme_set(theme_bw())
library(ggokabeito)
library(modelr)
library(classInt)
library(glue)
library(GGally)
library(tibble)
library(splines)

# Parameters

base_path <- "."
data_path <- file.path(base_path, "data")
res_path <- file.path(base_path, "results")

# Functions

save_fullsize_png <- function(p, path, width=19.65, height=10.30) {
  png(path,
      width = width,
      height = height,
      units = "in",
      res = 96)
  print(p)
  dev.off()
}


get_breaks_labels <- function(fill_vec, n_divs, option, nsmall, round_to=nsmall) {
  
  breaks <- classIntervals(fill_vec, n=n_divs, style=option)$brks
  
  if (option == "quantile") {
    percentages <- seq(0, 100, length.out = n_divs + 1) |> 
      round(1) |> 
      format(nsmall = 1) |> 
      paste0("%")
    
    values <- round(breaks, round_to) |>
      format(nsmall = nsmall)
    
    labels <- glue("{values} ({percentages})") |> 
      as.character()
    
  } else{
    
    labels <- round(breaks, round_to) |>
      format(nsmall = nsmall)
    
  }
  
  return(list(breaks, labels))
}




# Load data

environ_df <- readRDS(
  file.path(data_path, "processed", "environmental-variables_moshi-siha_monthly_2014-2022.Rds")
  )

disease_df <- readRDS(
  file.path(data_path, "processed", "disease-cases_moshi-siha_monthly_2014-2022.Rds")
)


# Exploratory analysis ===================================================================

## Missing data --------------------------------------------------------------------------

### Environmental ----

var_order <- colnames(environ_df)[3:10]

p <- environ_df |>
  pivot_longer(
    cols = population:temp_max,
    names_to="variable",
    values_to="value"
  ) |> 
  mutate(
    variable = factor(variable, levels=var_order)
  ) |> 
  ggplot() +
  geom_tile(
    aes(x=time, y=variable, fill=is.na(value)),
    color="black",
    width=31
  ) +
  scale_fill_manual(
    values = c("steelblue3", "gray20"),
    labels = c("No", "Yes")
  ) +
  labs(
    fill = "Missing",
    x = "Month",
    y = ""
  ) +
  facet_wrap(
    ~ district,
    ncol=2
  ) +
  theme_bw(
    base_size=22
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    strip.text = element_text(face="bold")
  )


save_fullsize_png(
  p,
  file.path(res_path, "plots", "environmental-data_missing.png")
)


### Diseases ----

# Diseases without grouping

breaks_labels <- get_breaks_labels(fill_vec=disease_df$n_cases, n_divs=4,
                                   option="quantile",
                                   nsmall=0, round_to=0)

disease_order <- disease_df |> 
  group_by(disease) |> 
  summarise(n = n()) |> 
  arrange(n) |> 
  pull(disease)


p <- disease_df |> 
  mutate(disease = factor(disease, levels=rev(disease_order))) |>
  ggplot() +
  geom_tile(
    aes(x=time, y=disease, fill=n_cases),
    color="black",
    width=31
  ) +
  scale_fill_viridis_b(
    limits = c(min(breaks_labels[[1]]), max(breaks_labels[[1]])),
    breaks=breaks_labels[[1]],
    labels=breaks_labels[[2]]
  ) +
  facet_wrap(
    ~ district,
    ncol=2
  ) +
  guides(
    fill = guide_colorsteps(
      even.steps = TRUE,
      barheight = 20,
      title.vjust = 2,
      # title.hjust = -0.4
    )
  ) +
  labs(
    y = "",
    x = "Month",
    fill = "No. cases"
  ) +
  theme_bw(
    base_size=16
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold", size=14),
    strip.text = element_text(face="bold"),
    legend.text = element_text(family="mono", face="bold",
                               size=rel(1.1)
                               )
  )

save_fullsize_png(
  p,
  file.path(res_path, "plots", "disease-data_diseases_missing.png")
)


# ######################################################.
# Initial approach:
# Remove diseases not fully reported for Moshi and Siha
# ######################################################.

# 9years * 12months *2districts
9*12*2

disease_list <- disease_df |> 
  group_by(disease) |> 
  summarise(
    n = n()
  ) |> 
  filter(n >=216) |> 
  pull(disease)

disease_df_filt <- disease_df |> 
  filter(disease %in% disease_list)

# Repeat missingness plot

breaks_labels <- get_breaks_labels(fill_vec=disease_df_filt$n_cases, n_divs=6,
                                   option="quantile",
                                   nsmall=0, round_to=0)

disease_order <- disease_df_filt |> 
  group_by(disease) |> 
  summarise(n = n()) |> 
  arrange(n) |> 
  pull(disease)


p <- disease_df_filt |> 
  mutate(disease = factor(disease, levels=rev(disease_order))) |>
  ggplot() +
  geom_tile(
    aes(x=time, y=disease, fill=n_cases),
    color="black",
    width=31
  ) +
  scale_fill_viridis_b(
    limits = c(min(breaks_labels[[1]]), max(breaks_labels[[1]])),
    breaks=breaks_labels[[1]],
    labels=breaks_labels[[2]]
  ) +
  facet_wrap(
    ~ district,
    ncol=2
  ) +
  guides(
    fill = guide_colorsteps(
      even.steps = TRUE,
      barheight = 35,
      title.vjust = 2,
      # title.hjust = -0.4
    )
  ) +
  labs(
    y = "",
    x = "Month",
    fill = "No. cases"
  ) +
  theme_bw(
    base_size=18
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    strip.text = element_text(face="bold"),
    legend.text = element_text(family="mono", face="bold",
                               size=rel(1.0)
    )
  )

save_fullsize_png(
  p,
  file.path(res_path, "plots", "disease-data_diseases_missing_after-filter.png")
)


# Missingness by disease groups
# (for the filtered diseases)


disease_df_filt_grouped <- disease_df_filt |> 
  group_by(time, district, disease_group) |> 
  summarise(
    n_cases = sum(n_cases, na.rm = TRUE),
    included_diseases = list(unique(disease))
  ) |> 
  ungroup()

breaks_labels <- get_breaks_labels(fill_vec=disease_df_filt_grouped$n_cases, n_divs=6,
                                   option="quantile",
                                   nsmall=0, round_to=0)

p <- disease_df_filt_grouped |> 
  ggplot() +
  geom_tile(
    aes(x=time, y=disease_group, fill=n_cases),
    color="black",
    width=31
  ) +
  scale_fill_viridis_b(
    limits = c(min(breaks_labels[[1]]), max(breaks_labels[[1]])),
    breaks=breaks_labels[[1]],
    labels=breaks_labels[[2]]
  ) +
  facet_wrap(
    ~ district,
    ncol=2
  ) +
  guides(
    fill = guide_colorsteps(
      even.steps = TRUE,
      barheight = 35,
      title.vjust = 2,
      # title.hjust = -0.4
    )
  ) +
  labs(
    y = "",
    x = "Month",
    fill = "No. cases"
  ) +
  theme_bw(
    base_size=18
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    strip.text = element_text(face="bold"),
    legend.text = element_text(family="mono", face="bold",
                               size=rel(1.0)
    )
  )

save_fullsize_png(
  p,
  file.path(res_path, "plots", "disease-data_disease-groups_after-filter.png")
)


# Select interesting diseases

tmp <- disease_df_filt_grouped |> 
  group_by(disease_group) |> 
  summarise(included_diseases = list(first(included_diseases)))

for (i in 1:nrow(tmp)) {
  print(tmp[[i,1]])
  print(tmp[[i,2]])
  cat("\n")
}

disease_df_filt$disease |> unique()

c(
  # Hypertension
  "Hypertension",
  # Respiratory Disease
  "Bronchial Asthma",
  # Respiratory Infection
  "Pneumonia, Severe",
  "Tuberculosis",
  # Neurological Disorders
  "Psychoses",
  "Epilepsy",
  # Mental Health Disorders
  "Substance Abuse",
  # Diarrheal Disease
  "Diarrhea With No Dehydration",
  "Diarrhea With Severe Dehydration",
  "Dysentery",
  # Other CD
  "Typhoid",
  "Intestinal Worms",
  # Neglected Tropical Disease (NTD)
  "Schistosomiasis",
  # Malnutrition
  "Kwashiorkor",
  "Marasmus",
  "Moderate Malnutrition",
  # Trauma and Injuries
  "Road Traffic Accidents",
  # Other NCD
  "Caries",
  "Leprosy",
  "Peptic Ulcers",
  "Poisoning",
  "Snake and Insect Bites1"
  )

# Start with cardiovascular and respiratory diseases


## Environmental data --------------------------------------------------------------------

p <- environ_df |> 
  pivot_longer(
    cols = 3:last_col(),
    names_to="variable",
    values_to="value"
  ) |> 
  mutate(
    variable = factor(variable, levels=colnames(environ_df)[3:10])
  ) |> 
  ggplot() +
  geom_point(
    aes(x=time, y=value, color=district),
    size=2
  ) +
  geom_line(
    aes(x=time, y=value, color=district),
    linewidth=0.9
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(environ_df$time, "year")),
             color="gray70", linetype=2, linewidth=1) +
  scale_color_okabe_ito() +
  facet_grid(
    rows = vars(variable),
    scales = "free_y",
    labeller = as_labeller(c(
      "population"="Population",
      "pm2p5"="PM2.5",
      "greenness"="Greenness",
      "total_rainfall"="Total rainfall",
      "n_raindays"="No. rain days",
      "temp_mean"="Mean temp.",
      "temp_min"="Min. temp.",
      "temp_max"="Max. temp."
    ))
  ) +
  labs(
    x="Month",
    y="",
    color="District"
  ) +
  theme_bw(
    base_size=16
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )
  
save_fullsize_png(
  p,
  file.path(res_path, "plots", "environmental-data_exploratory_all.png")
)

# ---

p <- environ_df |> 
  pivot_longer(
    cols = 3:last_col(),
    names_to="variable",
    values_to="value"
  ) |> 
  mutate(
    variable = factor(variable, levels=colnames(environ_df)[3:10])
  ) |> 
  filter(
    ! variable %in% c("temp_min", "temp_max")
  ) |> 
  ggplot() +
  geom_point(
    aes(x=time, y=value, color=district),
    size=2
  ) +
  geom_line(
    aes(x=time, y=value, color=district),
    linewidth=0.9
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(environ_df$time, "year")),
             color="gray70", linetype=2, linewidth=1) +
  scale_color_okabe_ito() +
  facet_grid(
    rows = vars(variable),
    scales = "free_y",
    labeller = as_labeller(c(
      "population"="Population",
      "pm2p5"="PM2.5",
      "greenness"="Greenness",
      "total_rainfall"="Total rainfall",
      "n_raindays"="No. rain days",
      "temp_mean"="Mean temperature"
    ))
  ) +
  labs(
    x="Month",
    y="",
    color="District"
  ) +
  theme_bw(
    base_size=16
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )

save_fullsize_png(
  p,
  file.path(res_path, "plots", "environmental-data_exploratory_filtered.png")
)



# Correlations

p <- ggpairs(
  environ_df,
  columns = c(3:10),
  mapping = aes(color=district),
  upper = list(continuous = wrap("cor", method = "kendall"))
) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  theme_bw(
    base_size=16
  )

save_fullsize_png(
  p,
  file.path(res_path, "plots", "environmental-data_exploratory_correlations.png")
)





# Disease case models --------------------------------------------------------------------

# # Hypertension
# var <- "Hypertension"
# # Respiratory Disease
# var <- "Bronchial Asthma"
# # Respiratory Infection
# var <- "Pneumonia, Severe"
# var <- "Tuberculosis"
# # Neurological Disorders
# var <- "Psychoses"
# var <- "Epilepsy"
# # Mental Health Disorders
# var <- "Substance Abuse"

regression_vars <- c(
  "n_cases", "population", "pm2p5", "greenness", "n_raindays", "temp_mean"
)

## Hypertension --------------------------------------------------------------------------

var <- "Hypertension"


# Filter disease cases and join environmental data
df_analysis <- disease_df_filt |> 
  filter(disease == var) |> 
  select(time, district, n_cases) |> 
  full_join(environ_df, by=c("time", "district"))

# Create time index and prepare other variables
df_analysis <- df_analysis |> 
  mutate(
    year = year(time),
    month = month(time),
    time_ind = (year - min(year))*12 + month
  ) |> 
  mutate(
    district = factor(district, levels=c("Moshi", "Siha")),
    ln_population = log(population)
  )

# Plot case rate per 1,000 people

df_analysis |> 
  ggplot() +
  geom_point(
    aes(x=time, y=n_cases/population*1e3, color=district),
    size=2
  ) +
  geom_line(
    aes(x=time, y=n_cases/population*1e3, color=district),
    linewidth=0.9
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(df_analysis$time, "year")),
             color="gray70", linetype=2, linewidth=1) +
  scale_color_okabe_ito() +
  labs(
    x="Month",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


## Model 1: Trend and districts ----------------------------------------------------------
# Model to evaluate linear trend of incidence rate and difference between districts

mod <- MASS::glm.nb(
  n_cases ~ offset(ln_population) + district + time_ind + district:time_ind,
  data=df_analysis
)

# Validate
par(mfrow = c(2,2))
plot(mod, ask=FALSE)
par(mfrow = c(1,1))

# Check coefficients
summary(mod)

# mod_tbl1 <- broom::tidy(mod)
# mod_tbl2 <- Epi::ci.exp(mod) |>
#   as.data.frame() |>
#   rownames_to_column("Est.") |> 
#   as_tibble()
# mod_tbl <- bind_cols(mod_tbl1, mod_tbl2[,-1])


mod_tbl <- confint(mod, level=0.95) |>
  as.data.frame() |> 
  rownames_to_column("term") |> 
  as_tibble() |> 
  mutate(
    exp_estimate = exp(mod$coefficients),
    exp_conf.low = exp(`2.5 %`),
    exp_conf.high = exp(`97.5 %`),
  )

mod_tbl

# rateSiha <- mod_tbl2[1,2:4] * mod_tbl2[2,2:4]
# rateMoshi <- mod_tbl2[1,2:4]


# Plot fitted values
pred_link <- predict(mod, se.fit=TRUE, type="link")


df_analysis |> 
  mutate(
    pred = with(pred_link, exp(fit)),
    pred_low = with(pred_link, exp(fit + qnorm(0.025)*se.fit)),
    pred_high = with(pred_link, exp(fit + qnorm(0.975)*se.fit))  
  ) |> 
  ggplot() +
  geom_ribbon(
    aes(x=time, ymin=pred_low/population*1e3, ymax=pred_high/population*1e3, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=time, y=pred/population*1e3, color=district),
    linewidth=1
  ) +
  geom_point(
    aes(x=time, y=n_cases/population*1e3, color=district),
    size=2.5
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(df_analysis$time, "year")),
             color="gray70", linetype=2, linewidth=0.8) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Month",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )

# Effect plots

predict_response(mod, c("time_ind", "district")) |>
  plot(show_data=TRUE)

predict_response(mod, c("time_ind", "ln_population", "district")) |>
  plot(show_data=TRUE)


## Model 2: Environmental effects --------------------------------------------------------
# Model to evaluate effects of environmental variables on incidence rate

# Separate districts

### Moshi ----

df_analysis_moshi <- df_analysis |> 
  filter(district == "Moshi")


# 5 degrees of freedom per year
degfree <- df_analysis_moshi$year |>
  range() |>
  diff()*5

mod <- MASS::glm.nb(
  n_cases ~ offset(ln_population) + pm2p5 + greenness + n_raindays + ns(time_ind, df=degfree),
  data=df_analysis_moshi
)

# Validate
par(mfrow = c(2,2))
plot(mod, ask=FALSE)
par(mfrow = c(1,1))

# Check coefficients
summary(mod)

mod_tbl <- confint(mod, level=0.95) |>
  as.data.frame() |> 
  rownames_to_column("term") |> 
  as_tibble() |> 
  mutate(
    exp_estimate = exp(mod$coefficients),
    exp_conf.low = exp(`2.5 %`),
    exp_conf.high = exp(`97.5 %`),
  )

mod_tbl

# Effect plots

predict_response(mod, c("time_ind"),
                 condition = c(ln_population=log(1e3))) |>
  plot(show_data=FALSE)

# Time

resp_df <- predict_response(mod, c("time_ind"),
                 condition = c(ln_population=log(1e3)))

df_analysis_moshi |> 
  right_join(resp_df, by=c("time_ind"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=time, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=time, y=predicted, color=district),
    linewidth=1
  ) +
  geom_point(
    aes(x=time, y=n_cases/population*1e3, color=district),
    size=2.5
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(df_analysis$time, "year")),
             color="gray70", linetype=2, linewidth=0.8) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Month",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


# PM2.5

resp_df <- predict_response(mod, c("pm2p5"),
                 condition = c(ln_population=log(1e3)))

resp_df |> 
  plot(show_data=FALSE)


df_analysis_moshi |> 
  right_join(resp_df, by=c("pm2p5"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=pm2p5, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=pm2p5, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=pm2p5, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="PM2.5",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )

# No. Rain days

resp_df <- predict_response(mod, c("n_raindays"),
                 condition = c(ln_population=log(1e3)))


resp_df |> 
  plot(show_data=FALSE)


df_analysis_moshi |> 
  right_join(resp_df, by=c("n_raindays"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=n_raindays, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=n_raindays, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=n_raindays, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="No. rain days",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


# Greenness

resp_df <- predict_response(mod, c("greenness"),
                 condition = c(ln_population=log(1e3)))

resp_df |> 
  plot(show_data=FALSE)


df_analysis_moshi |> 
  right_join(resp_df, by=c("greenness"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=greenness, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=greenness, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=greenness, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Greenness",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )



### Siha ----

df_analysis_siha <- df_analysis |> 
  filter(district == "Siha")


# 5 degrees of freedom per year
degfree <- df_analysis_siha$year |>
  range() |>
  diff()*5

mod <- MASS::glm.nb(
  n_cases ~ offset(ln_population) + pm2p5 + greenness + n_raindays + ns(time_ind, df=degfree),
  data=df_analysis_siha
)

# Validate
par(mfrow = c(2,2))
plot(mod, ask=FALSE)
par(mfrow = c(1,1))

# Check coefficients
summary(mod)

mod_tbl <- confint(mod, level=0.95) |>
  as.data.frame() |> 
  rownames_to_column("term") |> 
  as_tibble() |> 
  mutate(
    exp_estimate = exp(mod$coefficients),
    exp_conf.low = exp(`2.5 %`),
    exp_conf.high = exp(`97.5 %`),
  )

mod_tbl

# Effect plots

predict_response(mod, c("time_ind"),
                 condition = c(ln_population=log(1e3))) |>
  plot(show_data=FALSE)

# Time

resp_df <- predict_response(mod, c("time_ind"),
                            condition = c(ln_population=log(1e3)))

df_analysis_siha |> 
  right_join(resp_df, by=c("time_ind"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=time, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=time, y=predicted, color=district),
    linewidth=1
  ) +
  geom_point(
    aes(x=time, y=n_cases/population*1e3, color=district),
    size=2.5
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(df_analysis$time, "year")),
             color="gray70", linetype=2, linewidth=0.8) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Month",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


# PM2.5

resp_df <- predict_response(mod, c("pm2p5"),
                            condition = c(ln_population=log(1e3)))

resp_df |> 
  plot(show_data=FALSE)


df_analysis_siha |> 
  right_join(resp_df, by=c("pm2p5"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=pm2p5, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=pm2p5, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=pm2p5, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="PM2.5",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )

# No. Rain days

resp_df <- predict_response(mod, c("n_raindays"),
                            condition = c(ln_population=log(1e3)))


resp_df |> 
  plot(show_data=FALSE)


df_analysis_siha |> 
  right_join(resp_df, by=c("n_raindays"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=n_raindays, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=n_raindays, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=n_raindays, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="No. rain days",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


# Greenness

resp_df <- predict_response(mod, c("greenness"),
                            condition = c(ln_population=log(1e3)))

resp_df |> 
  plot(show_data=FALSE)


df_analysis_siha |> 
  right_join(resp_df, by=c("greenness"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=greenness, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=greenness, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=greenness, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Greenness",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )




## Bronchial Asthma --------------------------------------------------------------------------

var <- "Bronchial Asthma"


# Filter disease cases and join environmental data
df_analysis <- disease_df_filt |> 
  filter(disease == var) |> 
  select(time, district, n_cases) |> 
  full_join(environ_df, by=c("time", "district"))

# Create time index and prepare other variables
df_analysis <- df_analysis |> 
  mutate(
    year = year(time),
    month = month(time),
    time_ind = (year - min(year))*12 + month
  ) |> 
  mutate(
    district = factor(district, levels=c("Moshi", "Siha")),
    ln_population = log(population)
  )

# Plot case rate per 1,000 people

p <- df_analysis |> 
  ggplot() +
  geom_point(
    aes(x=time, y=n_cases/population*1e3, color=district),
    size=2
  ) +
  geom_line(
    aes(x=time, y=n_cases/population*1e3, color=district),
    linewidth=0.9
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(df_analysis$time, "year")),
             color="gray70", linetype=2, linewidth=1) +
  scale_color_okabe_ito() +
  labs(
    x="Month",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


save_fullsize_png(
  p,
  file.path(res_path, "plots", "tmp-plot.png")
)

## Model 1: Trend and districts ----------------------------------------------------------
# Model to evaluate linear trend of incidence rate and difference between districts

mod <- MASS::glm.nb(
  n_cases ~ offset(ln_population) + district + time_ind + district:time_ind,
  data=df_analysis
)

# Validate
par(mfrow = c(2,2))
plot(mod, ask=FALSE)
par(mfrow = c(1,1))

# Check coefficients
summary(mod)

# mod_tbl1 <- broom::tidy(mod)
# mod_tbl2 <- Epi::ci.exp(mod) |>
#   as.data.frame() |>
#   rownames_to_column("Est.") |> 
#   as_tibble()
# mod_tbl <- bind_cols(mod_tbl1, mod_tbl2[,-1])


mod_tbl <- confint(mod, level=0.95) |>
  as.data.frame() |> 
  rownames_to_column("term") |> 
  as_tibble() |> 
  mutate(
    exp_estimate = exp(mod$coefficients),
    exp_conf.low = exp(`2.5 %`),
    exp_conf.high = exp(`97.5 %`),
  )

mod_tbl

# rateSiha <- mod_tbl2[1,2:4] * mod_tbl2[2,2:4]
# rateMoshi <- mod_tbl2[1,2:4]


# Plot fitted values
pred_link <- predict(mod, se.fit=TRUE, type="link")


p <- df_analysis |> 
  mutate(
    pred = with(pred_link, exp(fit)),
    pred_low = with(pred_link, exp(fit + qnorm(0.025)*se.fit)),
    pred_high = with(pred_link, exp(fit + qnorm(0.975)*se.fit))  
  ) |> 
  ggplot() +
  geom_ribbon(
    aes(x=time, ymin=pred_low/population*1e3, ymax=pred_high/population*1e3, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=time, y=pred/population*1e3, color=district),
    linewidth=1
  ) +
  geom_point(
    aes(x=time, y=n_cases/population*1e3, color=district),
    size=2.5
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(df_analysis$time, "year")),
             color="gray70", linetype=2, linewidth=0.8) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Month",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


save_fullsize_png(
  p,
  file.path(res_path, "plots", "tmp-plot.png")
)

# Effect plots

predict_response(mod, c("time_ind", "district")) |>
  plot(show_data=TRUE)

predict_response(mod, c("time_ind", "ln_population", "district")) |>
  plot(show_data=TRUE)


## Model 2: Environmental effects --------------------------------------------------------
# Model to evaluate effects of environmental variables on incidence rate

# Separate districts

### Moshi ----

df_analysis_moshi <- df_analysis |> 
  filter(district == "Moshi")


# 5 degrees of freedom per year
degfree <- df_analysis_moshi$year |>
  range() |>
  diff()*5

mod <- MASS::glm.nb(
  n_cases ~ offset(ln_population) + pm2p5 + greenness + n_raindays + ns(time_ind, df=degfree),
  data=df_analysis_moshi
)

# Validate
par(mfrow = c(2,2))
plot(mod, ask=FALSE)
par(mfrow = c(1,1))

# Check coefficients
summary(mod)

mod_tbl <- confint(mod, level=0.95) |>
  as.data.frame() |> 
  rownames_to_column("term") |> 
  as_tibble() |> 
  mutate(
    exp_estimate = exp(mod$coefficients),
    exp_conf.low = exp(`2.5 %`),
    exp_conf.high = exp(`97.5 %`),
  )

mod_tbl

# Effect plots

predict_response(mod, c("time_ind"),
                 condition = c(ln_population=log(1e3))) |>
  plot(show_data=FALSE)

# Time

resp_df <- predict_response(mod, c("time_ind"),
                            condition = c(ln_population=log(1e3)))

p <- df_analysis_moshi |> 
  right_join(resp_df, by=c("time_ind"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=time, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=time, y=predicted, color=district),
    linewidth=1
  ) +
  geom_point(
    aes(x=time, y=n_cases/population*1e3, color=district),
    size=2.5
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(df_analysis$time, "year")),
             color="gray70", linetype=2, linewidth=0.8) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Month",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )

save_fullsize_png(
  p,
  file.path(res_path, "plots", "tmp-plot.png")
)

# PM2.5

resp_df <- predict_response(mod, c("pm2p5"),
                            condition = c(ln_population=log(1e3)))

resp_df |> 
  plot(show_data=FALSE)


p<- df_analysis_moshi |> 
  right_join(resp_df, by=c("pm2p5"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=pm2p5, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=pm2p5, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=pm2p5, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="PM2.5",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )

# No. Rain days

resp_df <- predict_response(mod, c("n_raindays"),
                            condition = c(ln_population=log(1e3)))


resp_df |> 
  plot(show_data=FALSE)


df_analysis_moshi |> 
  right_join(resp_df, by=c("n_raindays"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=n_raindays, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=n_raindays, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=n_raindays, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="No. rain days",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


# Greenness

resp_df <- predict_response(mod, c("greenness"),
                            condition = c(ln_population=log(1e3)))

resp_df |> 
  plot(show_data=FALSE)


p <- df_analysis_moshi |> 
  right_join(resp_df, by=c("greenness"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=greenness, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=greenness, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=greenness, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Greenness",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )



### Siha ----

df_analysis_siha <- df_analysis |> 
  filter(district == "Siha")


# 5 degrees of freedom per year
degfree <- df_analysis_siha$year |>
  range() |>
  diff()*5

mod <- MASS::glm.nb(
  n_cases ~ offset(ln_population) + pm2p5 + greenness + n_raindays + ns(time_ind, df=degfree),
  data=df_analysis_siha
)

# Validate
par(mfrow = c(2,2))
plot(mod, ask=FALSE)
par(mfrow = c(1,1))

# Check coefficients
summary(mod)

mod_tbl <- confint(mod, level=0.95) |>
  as.data.frame() |> 
  rownames_to_column("term") |> 
  as_tibble() |> 
  mutate(
    exp_estimate = exp(mod$coefficients),
    exp_conf.low = exp(`2.5 %`),
    exp_conf.high = exp(`97.5 %`),
  )

mod_tbl

# Effect plots

predict_response(mod, c("time_ind"),
                 condition = c(ln_population=log(1e3))) |>
  plot(show_data=FALSE)

# Time

resp_df <- predict_response(mod, c("time_ind"),
                            condition = c(ln_population=log(1e3)))

p <- df_analysis_siha |> 
  right_join(resp_df, by=c("time_ind"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=time, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=time, y=predicted, color=district),
    linewidth=1
  ) +
  geom_point(
    aes(x=time, y=n_cases/population*1e3, color=district),
    size=2.5
  ) +
  scale_x_date(date_breaks="year", date_labels = "%Y")+
  geom_vline(xintercept = unique(floor_date(df_analysis$time, "year")),
             color="gray70", linetype=2, linewidth=0.8) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Month",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


# PM2.5

resp_df <- predict_response(mod, c("pm2p5"),
                            condition = c(ln_population=log(1e3)))

resp_df |> 
  plot(show_data=FALSE)


df_analysis_siha |> 
  right_join(resp_df, by=c("pm2p5"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=pm2p5, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=pm2p5, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=pm2p5, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="PM2.5",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )

# No. Rain days

resp_df <- predict_response(mod, c("n_raindays"),
                            condition = c(ln_population=log(1e3)))


resp_df |> 
  plot(show_data=FALSE)


df_analysis_siha |> 
  right_join(resp_df, by=c("n_raindays"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=n_raindays, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=n_raindays, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=n_raindays, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="No. rain days",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


# Greenness

resp_df <- predict_response(mod, c("greenness"),
                            condition = c(ln_population=log(1e3)))

resp_df |> 
  plot(show_data=FALSE)


df_analysis_siha |> 
  right_join(resp_df, by=c("greenness"="x")) |> 
  ggplot() +
  geom_ribbon(
    aes(x=greenness, ymin=conf.low, ymax=conf.high, fill=district),
    alpha=0.4, show.legend = FALSE
  ) +
  geom_line(
    aes(x=greenness, y=predicted, color=district),
    linewidth=1
  ) +
  # geom_point(
  #   aes(x=greenness, y=n_cases/population*1e3, color=district),
  #   size=2.5
  # ) +
  scale_color_okabe_ito() +
  scale_fill_okabe_ito() +
  labs(
    x="Greenness",
    y=glue("{var} cases per 1,000 people"),
    color="District"
  ) +
  theme_bw(
    base_size=20
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold")
  )


































##############################################################333 




predict_response(mod, c("time_ind")) |>
  plot(show_data=TRUE)

predict_response(mod, c("time_ind")) |>
  plot(residuals=TRUE)


predict_response(mod, c("district")) |>
  plot(residuals=FALSE)


tmp <- predict_response(mod)


mod <- MASS::glm.nb(
  n_cases ~ offset(ln_population) + district + pm2p5 + greenness + n_raindays + district*ns(time_ind, df=35),
  data=df_analysis
)




mod <- glm(
  n_cases ~ offset(ln_population) + district + pm2p5 + greenness + n_raindays + district*ns(time_ind, df=35),
  data=df_analysis,
  family=quasipoisson(link="log")
)


summary(mod)


  
par(mfrow = c(2,2))
plotcontrolplot(mod, ask=FALSE)
par(mfrow = c(1,1))



predict_response(mod, c("pm2p5", "district")) |>
  plot(show_data=FALSE)

predict_response(mod, c("district")) |>
  plot(show_data=FALSE)

predict_response(mod, c("time_ind")) |>
  plot(show_data=FALSE)

predict_response(mod, c("time_ind", "district")) |>
  plot(show_data=TRUE)



predict_response(mod, c("time_ind", "ln_population", "district")) |>
  plot(show_data=TRUE)


predict(mod, df_analysis, "response")

df_analysis |> 
  add_predictions(mod, type="response") |> 
  ggplot() +
  geom_line(
    aes(x=time_ind, y=pred/population*1e3, color=district)
  ) +
  geom_point(
    aes(x=time_ind, y=n_cases/population*1e3, color=district)
  )

df_analysis |> 
  add_predictions(mod, type="response") |> 
  ggplot() +
  geom_line(
    aes(x=time_ind, y=pred, color=district)
  ) +
  geom_point(
    aes(x=time_ind, y=n_cases, color=district)
  )




# ---





df_analysis |> 
  ggplot() +
  geom_point(
    aes(x=as.POSIXct(time), y=n_cases, color=district),
    size=2
  ) +
  geom_line(
    aes(x=as.POSIXct(time), y=n_cases, color=district),
    linewidth=0.9
  ) +
  scale_x_datetime(
    breaks = "1 year", date_labels = "%Y"
  ) +
  scale_color_okabe_ito() +
  theme_bw(
    base_size=20
  )

df_analysis <- df_analysis |> 
  select(time, district, n_cases, population, pm2p5, greenness, n_raindays, temp_mean)

df_analysis <- df_analysis |> 
  mutate(
    year = year(time),
    month = month(time),
    time_ind = (year - min(year))*12 + month
  ) |> 
  mutate(
    district = factor(district, levels=c("Siha", "Moshi"))
  )



df_analysis



ggpairs(
  df_analysis,
  columns = c(3:8),
  mapping = aes(color=district),
  upper = list(continuous = wrap("cor", method = "kendall"))
)

library(mgcv)


# df_analysis <- df_analysis |> 
#   mutate(
#     district = factor(district, levels=c("Moshi", "Siha")),
#     month = factor(month(time)),
#     year = factor(year(time))
#     )

crosspred()


library(dlnm)
onebasis()

library(gnm)
gnm()

library(splines)

ns.temp_mean <- ns(df_analysis$temp_mean, df=6)



# TO DO
# diagnostics for overdispersion
# encode time
# select models by AIC
# effect plots
# interpret coefs?
# write down model

mod <- gam(
  n_cases ~ district + population + population:district + s(pm2p5, bs="cr"),
  data=df_analysis,
  # family=poisson(link="log")
  family=nb(link="log")
  )

mod


df_analysis <- df_analysis |> 
  mutate(ln_population=log(population))

mod <- gam(
  # n_cases ~ district + s(year, bs="cr", k=4),
  # n_cases ~ district + s(year, bs="cr", k=4) + s(month, bs="cr", k=4),
  # n_cases ~ district + population + population:district,
  # n_cases ~ pm2p5 + district + population + year,
  # n_cases ~ pm2p5 + district + population + year*district,
  # n_cases ~ district + s(time_ind, bs="cr", k=35) + pm2p5 + temp_mean + greenness + n_raindays,
  # n_cases ~ district + s(time_ind, bs="cr", k=35),
  # n_cases ~ district + s(time_ind, bs="cr", k=35) + pm2p5 + offset(log(population)),
  n_cases ~ district + s(time_ind, bs="cr", k=35) + pm2p5 + offset(ln_population),
  # n_cases ~ district + s(time_ind, bs="cr", k=35) + pm2p5,
  # n_cases ~ district + pm2p5,
  # n_cases ~ district + pm2p5 + population + population:district,
  # n_cases ~ pm2p5 + district + year,
  data=df_analysis,
  # family=poisson(link="log")
  family=nb(link="log")
  # family=quasipoisson(link="log")
)

mod <- glm(
  n_cases ~ offset(ln_population) + district + pm2p5 + greenness + n_raindays + ns(time_ind, df=35),
  data=df_analysis,
  family=quasipoisson(link="log"),
)

mod <- MASS::glm.nb(
  n_cases ~ offset(ln_population) + district + pm2p5 + greenness + n_raindays + ns(time_ind, df=35),
  data=df_analysis
)

summary(mod)

mod$coefficients

plot(mod)


Epi::ci.exp(mod)

acf(mod$residuals)


predict_response(mod, c("pm2p5", "district")) |>
  plot(show_data=FALSE)

predict_response(mod, c("district")) |>
  plot(show_data=FALSE)

predict_response(mod, c("time_ind", "district")) |>
  plot(show_data=TRUE)

predict_response(mod, c("time_ind", "ln_population")) |>
  plot(show_data=TRUE)

tmp <- predict_response(mod, c("time_ind", "district", "ln_population"))

levels(tmp$facet) <- levels(tmp$facet) |> 
  as.numeric() |> 
  exp() |> 
  as.character()

tmp |> 
  plot(show_data=FALSE)


predict(mod, newdata=df_analysis, type="link") |> exp()

confint(mod)

mydf <- predict_response(mod, c("time_ind", "district"))

mydf |> print(n=Inf)

cptime <- crosspred(basis="time_ind", mod)

cptime |> plot()



df_analysis |> 
  add_predictions(mod, type="response") |> 
  ggplot() +
  geom_line(
    aes(x=time_ind, y=pred, color=district)
  ) +
  geom_point(
    aes(x=time_ind, y=n_cases, color=district)
  )



predict_response(mod, c("time_ind")) |>
  plot(show_data=TRUE)

predict_response(mod, c("population", "district")) |>
  plot(show_data=TRUE)

predict_response(mod, c("year", "district")) |>
  plot(show_data=TRUE)


predict_response(mod, c("temp_mean", "district")) |>
  plot(show_data=TRUE)

predict_response(mod, c("greenness", "district")) |>
  plot(show_data=TRUE)

predict_response(mod, c("n_raindays", "district")) |>
  plot(show_data=TRUE)


predict_response(mod, c("population", "district")) |>
  plot(show_data=TRUE)

predict_response(mod, c("year", "district")) |>
  plot(show_data=TRUE)



plot(mod, pages = 1, residuals=TRUE, se=TRUE, cex=3)
gam.check(mod)



vis.gam(mod, view=c("population", "pm2p5"), plot.type = "persp", theta=-30, phi=30)
vis.gam(mod, view=c("district", "population"), plot.type = "persp", theta=-30, phi=30)
vis.gam(mod, view=c("district", "pm2p5"), plot.type = "persp", theta=-30, phi=30)

vis.gam(mod, view=c("population", "pm2p5"), type="response", plot.type = "persp", theta=-30, phi=30)



library(ggeffects)
mydf <- predict_response(mod, terms=c("district", "population"))

mydf <- predict_response(mod, terms=c("population", "district"))
mydf <- predict_response(mod, terms=c("pm2p5", "district"))
mydf <- predict_response(mod, terms=c("pm2p5", "population"))

mydf <- predict_response(mod, terms=c("year", "district"))
mydf <- predict_response(mod, terms=c("population", "district"))
mydf <- predict_response(mod, terms=c("month"))
mydf <- predict_response(mod, terms=c("year", "month"))
plot(mydf, show_data = TRUE) +
  scale_color_okabe_ito()
plot(mydf, facets = TRUE, show_data = TRUE)

plot(ggeffects::ggpredict(mod), facets = TRUE)

# Think about DAG


# UNCHARTED TERRITORY

mod <- glm(
  n_cases ~ district + population,
  family = "poisson",
  data = df_analysis
)


summary(mod)


mydf <- predict_response(mod, terms=c("population", "district"))

mydf |> as_tibble()

mydf |> 
  ggplot() +
  geom_line(
    aes(x=x, y=predicted, color=group)
  )

plot(mydf)


df_analysis |> 
  add_predictions(mod) |> 
  ggplot() +
  geom_line(
    aes(x=time_ind, y=pred, color=district)
  )

ggpredict(mod)


y_hat <- ggpredict(mod, terms = c("Year", "District", "Population"))

y_hat$facet

df_model |> 
  ggplot() +
  geom_line(data=y_hat,
            aes(x=x, y=predicted, group=group, color=group)) +
  geom_ribbon(data=y_hat,
              aes(x=x, y=predicted, ymin = conf.low, ymax = conf.high, group=group), alpha=0.3) +
  geom_point(aes(x=Year, y=n_cases, color=District))
