# It processes raw data of population, environment and disease from /data/raw
# It creates initial version of dataset in /data/processed/initial

# setup
source("00_source.R")

library(readxl) # read excel

# Population data ------------------------------------------------------------------------

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
  file.path(data_path, "processed", "initial", "population.csv")
)


# Environmental data --------------------------------------------------------------------

# Scattered across different files

## PM2.5 -----------------------------------

# Originally we had file 'avgpm25_monthly.csv'
# Then we received 'avg_pm25_monthly_siha_np.csv', with the values for Siha calculated
# without considering the National Park area (better)

# We substitute the values from the second file into the first one

# We also received separately files with data for 2012 and 2013:
# 2012_pm2_5.csv
# 2013_pm2_5.csv

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


## Greenness ------------------------------------

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

## Rain -----------------------------------

# Total rainfall (mm) and No. rain days

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

## Temperature -----------------------------

# Minimum, mean and maximum temperatures

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


# Use new temperature values provided by Netra's collaborators (28-08-24)
rm(temp)

temp <- read_excel(
  file.path(data_path, "raw", "environmental",
            "temperature_correction", "environmental_v2.xlsx"),
)

temp <-
  temp |> 
  select(
    district, date, temp_min_new, temp_mean_new, temp_max_new
  ) |> 
  mutate(
    date = as.Date(date)
  ) |> 
  rename(
    temp_min = temp_min_new,
    temp_mean = temp_mean_new,
    temp_max = temp_max_new
  )

temp

## Heat index ----------------------------------

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

## Merge --------------------------------------

envir <- pm2p5 |>
  full_join(green, by=c("date", "district")) |> 
  full_join(rain, by=c("date", "district")) |> 
  full_join(temp, by=c("date", "district")) |> 
  full_join(heat, by=c("date", "district"))

envir <- envir |> 
  arrange(district, date)

envir |> 
  print(width=Inf)

# Save
write_csv(
  envir,
  file.path(data_path, "processed", "initial", "environmental.csv")
)

## Other air quality variables ----------------------------------

# Data on other air quality variables from 2018 onward, 
# the format of which I do not understand

# They will not be processed

# The data is NO2, O3, SO2 and AOD

# NO2

# no2 <- read_csv(
#   file.path(data_path, "raw", "environmental", "other_air-quality", "no2_monthly_2018_2021.csv"),
#   col_select = -1
# )
# no2

# O3
# file.path(data_path, "raw", "environmental", "other_air-quality", "ozone_monthly_2018_2021.csv")

# SO2
# file.path(data_path, "raw", "environmental", "other_air-quality", "so2_monthly_2018_2021.csv")

# AOD
# file.path(data_path, "raw", "environmental", "other_air-quality", "AOD_monthly_2018_2021.csv")


# Disease data ---------------------------------------------------------------------------

## Moshi -------------------------

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

## Siha -------------------------

siha <- read_csv(
  file.path(data_path, "raw", "disease_v2", "Siha_monthlydata_cleaned_june_2024_august_updated.csv"),
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


## Merge --------------------

dis <- full_join(moshi, siha, by=colnames(moshi))

dis <- dis |> 
  arrange(district, disease, date)

dis

# Diseases differ in the number of observations. Snake And Insect Bites repeated.
dis$disease |> table()

dis$disease_group |> table()
dis$disease_communicable |> table()


# Fix category
dis <- dis |> 
  mutate(
    disease = case_match(
      disease,
      .default = disease,
      "Snake And Insect Bites" ~ "Snake and Insect Bites"
    )
  )

# Fix typos in groups
dis <- dis |> 
  mutate(
    disease_group = case_match(
      disease_group,
      .default = disease_group,
      "Diarrheal disease/Gastrointesntinal infections" ~ "Diarrheal disease/Gastrointestinal infections"
    )
  )

# Rename Bronchial Asthma to Chronic Respiratory Disease (doctor criterion)
dis <- dis |> 
  mutate(
    disease = case_match(
      disease,
      "Bronchial Asthma" ~ "Chronic Respiratory Disease",
      .default = disease
    )
  )


dis

# The 'disease' category is the most important one for the analyses.
# Categories 'disease_group' and 'disease_communicable' are relevant for the discussion.
# The groupings will be changed according to Chrysanthi and Harald's notes.

# Save
write_csv(
  dis,
  file.path(data_path, "processed", "initial", "disease.csv")
)
