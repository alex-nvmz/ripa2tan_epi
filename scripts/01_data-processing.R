#' ---
#' title: "01: Data processing"
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
#' # knitr:
#' #   opts_chunk:
#' #     out.width: "100%"
#' ---
#' 
#' Notebook to clean up all the raw data.
#' 
#' # Setup
#' 
## -------------------------------------------------------------------------------------------
library(tidyverse)
theme_set(theme_bw(base_size = 16))
library(readxl)
library(ggokabeito)
library(ggh4x)  # facet_nested

#' 
#' 
## -------------------------------------------------------------------------------------------
base_path <- ".."
data_path <- file.path(base_path, "data")
res_path <- file.path(base_path, "results")


# data_path <- "local_data_path"

#' 
#' # Data processing
#' 
#' 
#' ## Population data
#' 
## -------------------------------------------------------------------------------------------

# Get yearly population
pop <- read_excel(
  file.path(data_path, "raw", "population", "pop_millions.xlsx"),
  range = cell_cols("B:F")
  )

# Clean
pop <- pop |> 
  select(- c(mil, Year)) |> 
  rename(
    date = Date,
    district = District,
    population = Population
  ) |> 
  mutate(
    district = case_match(
      district,
      .default = district,
      "Moshi_MC" ~ "Moshi"
    ),
    date = as.Date(date)
  ) |> 
  relocate(district, date) |> 
  arrange(district, date)

pop

write_csv(
  pop,
  file.path(data_path, "processed", "population.csv")
)


#' 
#' ## Environmental data
#' 
#' The environmental data is scattered in different files.
#' 
#' ### PM2.5
#' 
#' For PM2.5, originally we received the file 'avgpm25_monthly.csv'. After that, we received 'avg_pm25_monthly_siha_np.csv', with the values for Siha calculated without considering the National Park area (this is better). We will substitute the values of the second file onto the first one.
#' 
#' We also have the monthly data for 2012 and 2013 in separate files.
#' 
#' 
## -------------------------------------------------------------------------------------------

pm2p5 <- read_csv(
  file.path(data_path, "raw", "environmental", "main", "avgpm25_monthly.csv"),
  col_select = -1
)

pm2p5

# New PM2.5 data for Siha (without considering National Park area)
pm2p5_siha <- read_csv(
  file.path(data_path, "raw", "environmental", "siha_correction", "avg_pm25_monthly_siha_np.csv"),
  col_select = -1
)

pm2p5_siha

# Substitute Siha observations in main pm2p5 dataframe
# New PM2.5 data for Siha (without considering National Park area)
pm2p5_siha <- pm2p5_siha |> 
  mutate(
    District = case_match(
      District,
      .default = District,
      "Siha_without_nationalpark" ~ "Siha"
    )
  )

pm2p5 <- pm2p5 |> 
  left_join(pm2p5_siha, by=c("District", "year", "month")) |> 
  mutate(
    avg_pm2.5_monthly = if_else(
      District == "Siha",
      avg_pm2.5_sihanp_monthly,
      avg_pm2.5_monthly
    )
  ) |> 
  select(- avg_pm2.5_sihanp_monthly)


# Clean
pm2p5 <- pm2p5 |> 
  mutate(
    date = ym(paste0(year, month))
  ) |>
  select(- c(year, month)) |> 
  rename(
    district=District,
    pm2p5=avg_pm2.5_monthly
  ) |> 
  relocate(district, date)

pm2p5

# Get 2012 and 2013 data
pm2p5_2012 <- read_csv(
  file.path(data_path, "raw", "environmental", "main", "2012_pm2_5.csv"),
  col_select = -1
)
pm2p5_2012

pm2p5_2013 <- read_csv(
  file.path(data_path, "raw", "environmental", "main", "2013_pm2_5.csv"),
  col_select = -1
)
pm2p5_2013

# Clean
pm2p5_2012 <- pm2p5_2012 |> 
  mutate(
    date = ym(paste0(year, month))
  ) |>
  select(- c(year, month)) |> 
  rename(
    district=District,
    pm2p5=avg_pm2.5_monthly
  ) |> 
  relocate(district, date)

pm2p5_2013 <- pm2p5_2013 |> 
  mutate(
    date = ym(paste0(year, month))
  ) |>
  select(- c(year, month)) |> 
  rename(
    district=District,
    pm2p5=avg_pm2.5_monthly
  ) |> 
  relocate(district, date)

# Merge
pm2p5 <- pm2p5 |> 
  full_join(pm2p5_2013) |> 
  full_join(pm2p5_2012) |> 
  arrange(district, date)

pm2p5

#' 
#' ### Greenness
#' 
## -------------------------------------------------------------------------------------------
green <- read_csv(
  file.path(data_path, "raw", "environmental", "main", "monthwise_greenness.csv")
)

green

green <- green |> 
  pivot_longer(
    cols = 2:last_col(),
    names_to = c("district", "month"),
    values_to = "greenness",
    names_sep = "_"
  )

green <- green |> 
  mutate(
    date = ym(paste0(year, month))
  ) |> 
  select( - c(year, month)) |> 
  mutate(
    district = case_match(
      district,
      "moshi" ~ "Moshi",
      "siha" ~ "Siha"
      )
  ) |> 
  relocate(district, date)

green

#' 
#' ### Rain
#' 
#' Total rainfall (mm) and No. rain days.
#' 
## -------------------------------------------------------------------------------------------
rain <- read_csv(
  file.path(data_path, "raw", "environmental", "main", "rc_month_rain_satellite.csv"),
  col_select = -1
)

rain

rain <- rain |> 
  mutate(
    date = ym(paste0(year, month))
  ) |> 
  select( - c(year, month)) |> 
  relocate(district, date) |> 
  rename(total_rainfall = "Monthly Total Rainfall",
         n_raindays = "Number of Raindays")

rain

#' 
#' ### Temperature
#' 
#' Minimum, mean and maximum temperatures.
#' 
## -------------------------------------------------------------------------------------------
temp <- read_csv(
  file.path(data_path, "raw", "environmental", "main", "sat_temp.csv"),
  col_select = -1
)

temp

temp <- temp |> 
  mutate(
    date = ym(paste0(year, month))
  ) |> 
  select( - c(year, month)) |> 
  relocate(district, date)

temp <- temp |> 
  pivot_wider(
    names_from=variable,
    values_from=value
  ) |> 
  rename(
    temp_mean = "Monthly mean temperature",
    temp_min = "Monthly mean minimum temperature",
    temp_max = "Monthly mean maximum temperature"
  ) |> 
  relocate(district, date, temp_min, temp_mean, temp_max)

temp

#' 
#' 
#' ### Heat index
#' 
## -------------------------------------------------------------------------------------------
heat_moshi <- read_csv(
  file.path(data_path, "raw", "environmental", "main", "moshi_utci_2012_oct_2022.csv"),
  col_select = -1
)

heat_moshi

heat_siha <- read_csv(
  file.path(data_path, "raw", "environmental", "main", "siha_utci_2012_oct_2022.csv"),
  col_select = -1
)

heat <- full_join(heat_moshi, heat_siha)

heat

heat <- heat |> 
  mutate(
    date = ym(paste0(Year, Month))
  ) |> 
  select(- c(Year, Month)) |> 
  rename(
    district = District,
    utci = MeanValue
  ) |> 
  relocate(district, date)

heat

#' 
#' 
#' ### Merge
#' 
## -------------------------------------------------------------------------------------------
df_environ <- pm2p5 |>
  full_join(green, by=c("date", "district")) |> 
  full_join(rain, by=c("date", "district")) |> 
  full_join(temp, by=c("date", "district")) |> 
  full_join(heat, by=c("date", "district"))

df_environ <- df_environ |> 
  arrange(district, date)

df_environ |> 
  print(width=Inf)

# Save
write_csv(
  df_environ,
  file.path(data_path, "processed", "environmental.csv")
)

#' 
#' 
#' ### Other air quality variables
#' 
#' There is data on other air quality variables from 2018 onward, but I do not understand the format (what is value.ID? Why does one place and time have multiple measurements?).
#' 
#' This data is NO2, O3, SO2 and AOD
#' 
## -------------------------------------------------------------------------------------------
# Additional air quality variables (2018-) -----------------------------------------------

# NO2

no2 <- read_csv(
  file.path(data_path, "raw", "environmental", "other_air-quality", "no2_monthly_2018_2021.csv"),
  col_select = -1
)

no2

# O3
# file.path(data_path, "raw", "environmental", "other_air-quality", "ozone_monthly_2018_2021.csv")

# SO2
# file.path(data_path, "raw", "environmental", "other_air-quality", "so2_monthly_2018_2021.csv")

# AOD
# file.path(data_path, "raw", "environmental", "other_air-quality", "AOD_monthly_2018_2021.csv")


#' 
#' 
#' ## Disease data
#' 
#' ### Moshi
#' 
## -------------------------------------------------------------------------------------------
# Moshi

moshi <- read_csv(
  file.path(data_path, "raw", "disease_v2", "Moshi_monthlydata_cleaned_june_2024.csv"),
  col_select = -1
)

moshi |> 
  print(width = Inf)

moshi <- moshi |> 
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "n_cases"
  ) 

moshi

moshi <- moshi |> 
  mutate(
    date = ym(paste0(Year, month))
  ) |> 
  select(- c(Year, month)) |> 
  mutate(
    district = "Moshi"
  ) |> 
  rename(
    disease = Diseases,
    disease_group = Subcategory,
    disease_communicable = Category
  ) |> 
  relocate(district, date, disease, disease_group, disease_communicable, n_cases)
  
moshi

#' 
#' ### Siha
#' 
## -------------------------------------------------------------------------------------------
# Siha

siha <- read_csv(
  file.path(data_path, "raw", "disease_v2", "Siha_monthlydata_cleaned_june_2024_august_updated.csv"),
  # file.path(data_path, "raw", "disease_v2", "Siha_monthlydata_cleaned_june_2024.csv"),
  col_select = -1
)

siha |> 
  print(width = Inf)

siha <- siha |> 
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "n_cases"
  ) 

siha

siha <- siha |> 
  mutate(
    date = ym(paste0(Year, month))
  ) |> 
  select(- c(Year, month, District)) |> 
  mutate(
    district = "Siha"
  ) |> 
  rename(
    disease = Diseases,
    disease_group = Subcategory,
    disease_communicable = Category
  ) |> 
  relocate(district, date, disease, disease_group, disease_communicable, n_cases)
  
siha

#' 
#' 
#' ### Merge
#' 
## -------------------------------------------------------------------------------------------
df_disease <- full_join(moshi, siha, by=colnames(moshi))

df_disease <- df_disease |> 
  arrange(district, disease, date)

df_disease

#' 
#' 
## -------------------------------------------------------------------------------------------
df_disease$disease |> table()

#' 
#' Diseases differ in the number of observations. Snake And Insect Bites repeated.
#' 
## -------------------------------------------------------------------------------------------
df_disease$disease_group |> table()

df_disease$disease_communicable |> table()

#' 
#' 
## -------------------------------------------------------------------------------------------
# Fix category
df_disease <- df_disease |> 
  mutate(
    disease = case_match(
      disease,
      .default = disease,
      "Snake And Insect Bites" ~ "Snake and Insect Bites"
    )
  )

#' 
#' The 'disease' category is the most important one for the analyses. Categories 'disease_group' and 'disease_communicable' are relevant for the discussion. The groupings will be changed according to Chrysanthi and Harald's notes.
#' 
#' 
## -------------------------------------------------------------------------------------------
# Save after checking missings
# write_csv(
#   df_disease,
#   file.path(data_path, "processed", "disease.csv")
# )

#' 
#' 
#' 
#' ## Missing data
#' 
#' Evaluate missingness with respect to time.
#' 
## -------------------------------------------------------------------------------------------
pop$date |> range()

df_disease$date |> range()

df_environ$date |> range()

total_range <- c(pop$date, df_disease$date, df_environ$date) |>
  range()
total_range

#' 
#' The most important range is the one of the disease data, which is the one that is relevant for the regression analysis: 2014-01-01 to 2022-12-01.
#' 
#' If we have missings on any of the regressors, we will not be able to include that observation in the regression.
#' 
#' We are missing the population for 2022, and this is vital for the analysis. We should impute it, at least to explore the incidence rate for 2022.
#' 
#' Let us evaluate the missingness for each variable.
#' 
#' ### Population
#' 
## -------------------------------------------------------------------------------------------
# Visualize population
pop |> 
  ggplot() +
  geom_point(
    aes(x=date, y=population, color=district)
  )

#' 
#' 
#' Population has been increasing linearly each year. We could impute the population of 2022 with a prediction from a linear model.
#' 
#' ### Environmental
#' 
## -------------------------------------------------------------------------------------------
# Visualize missingness

var_order <- colnames(df_environ)[3:10]

df_environ |>
  pivot_longer(
    cols = pm2p5:utci,
    names_to="variable",
    values_to="value"
  ) |> 
  mutate(
    variable = factor(variable, levels=var_order)
  ) |>
  ggplot() +
  geom_tile(
    aes(x=date, y=variable, fill=is.na(value)),
    color="black",
    width=31
  ) +
  scale_fill_manual(
    values = c("steelblue3", "gray20"),
    labels = c("No", "Yes")
  ) +
  labs(
    fill = "Missing",
    x = "Date (month)",
    y = ""
  ) +
  facet_wrap(
    ~ district,
    ncol=2
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    strip.text = element_text(face="bold")
  )

ggsave(
  file.path(res_path, "plots", "environmental_missing.png"),
  width=16, height=16*0.618
)

# map(
#   paste0(file.path(res_path, "plots", "environmental_missing."), c("svg", "png")),
#   ~ ggsave(.x, width=16, height=16*0.618)
# )

#' 
#' 
#' All variables except PM2.5 and UTCI (partially) are missing for 2022. We should exclude 2022 from the regression.
#' 
#' Greenness and UTCI have some missing values in the middle of their time series, so it could be reasonable to impute them.
#' 
#' Rain is missing for Siha in 2021. I think we should forecast these values, so as to be able to include the observations from 2021 in the regression.
#' 
#' 
#' ### Disease
#' 
#' 
## -------------------------------------------------------------------------------------------
# Missingness in no. of cases

df_disease |> 
  # More space
  mutate(
    disease_group = recode(
      disease_group,
      `Diarrheal disease/Gastrointesntinal infections`="Diarrheal disease/\nGastrointesntinal infections",
      `Other Communicable Diseases/Gastrointestinal infections`="Other Communicable Diseases/\nGastrointestinal infections"
      )
  ) |>
  ggplot() +
  geom_tile(
    aes(x=date, y=disease, fill=is.na(n_cases)),
    color="black",
    width=31
  ) +
  facet_nested(
    cols = vars(district),
    rows = vars(disease_communicable, disease_group),
    scales = "free_y",
    space = "free_y",
    # strip = strip_vanilla(size = "variable", clip = "off"),
    strip = strip_nested(size = "variable", clip="off", bleed = FALSE),
    solo_line = TRUE,
    nest_line = TRUE,
    # switch = "y"
  ) +
  scale_fill_manual(
    values = c("steelblue3", "gray20"),
    labels = c("No", "Yes")
  ) +
  labs(
    fill = "Missing",
    x = "Date (month)",
    y = ""
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    strip.text = element_text(face="bold"),
    # strip.background = element_rect(fill="white", color="gray"),
    strip.background = element_blank(),
    # ggh4x.facet.nestline = element_line(colour = "black"),
    strip.text.y = element_text(angle=0, size=rel(1)),
    strip.text.x = element_text(angle=0, size=rel(1.5)),
    legend.text = element_text(face="bold", size=rel(1.0))
  )


#' 
#' There are missing observations which are not coded as missing
#' 
## -------------------------------------------------------------------------------------------
# Fix and repeat plot

df_disease <- df_disease |> 
  complete(district, date, nesting(disease, disease_group, disease_communicable))

df_disease |> 
  # More space
  mutate(
    disease_group = recode(
      disease_group,
      `Diarrheal disease/Gastrointesntinal infections`="Diarrheal disease/\nGastrointesntinal infections",
      `Other Communicable Diseases/Gastrointestinal infections`="Other Communicable Diseases/\nGastrointestinal infections"
      )
  ) |>
  ggplot() +
  geom_tile(
    aes(x=date, y=disease, fill=is.na(n_cases)),
    color="black",
    width=31
  ) +
  facet_nested(
    cols = vars(district),
    rows = vars(disease_communicable, disease_group),
    scales = "free_y",
    space = "free_y",
    # strip = strip_vanilla(size = "variable", clip = "off"),
    strip = strip_nested(size = "variable", clip="off", bleed = FALSE),
    solo_line = TRUE,
    nest_line = TRUE,
    # switch = "y"
  ) +
  scale_fill_manual(
    values = c("steelblue3", "gray20"),
    labels = c("No", "Yes")
  ) +
  labs(
    fill = "Missing",
    x = "Date (month)",
    y = ""
  ) +
  theme(
    legend.title = element_text(face="bold"),
    axis.title = element_text(face="bold"),
    strip.text = element_text(face="bold"),
    # strip.background = element_rect(fill="white", color="gray"),
    strip.background = element_blank(),
    # ggh4x.facet.nestline = element_line(colour = "black"),
    strip.text.y = element_text(angle=0, size=rel(1)),
    strip.text.x = element_text(angle=0, size=rel(1.5)),
    legend.text = element_text(face="bold", size=rel(1.0))
  )

ggsave(
  file.path(res_path, "plots", "disease_missing.png"),
  width=18, height=18*0.700
)

#' 
#' We have complete observations for many diseases of interest.
#' 
#' 
## -------------------------------------------------------------------------------------------
# Save
write_csv(
  df_disease,
  file.path(data_path, "processed", "disease.csv")
)

#' 
