
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
theme_set(theme_bw())
library(ggokabeito)
library(readxl)
library(modelr)


base_path <- ".."
data_path <- file.path(base_path, "data")


# Population data ------------------------------------------------------------------------

# Get yearly population

pop <- read_excel(
  file.path(data_path, "raw", "disease_burden_yearly.xlsx"),
  sheet = "Sheet1"
  )

pop <- pop |> 
  group_by(District, Year) |> 
  summarise(Population = mean(Population)) |> 
  ungroup() |> 
  rename(
    district=District,
    year=Year,
    population=Population
  )


# Get population for 2022 by linear interpolation

pop |> 
  ggplot() +
  geom_smooth(
    aes(x=year, y = population, color=district),
    method="lm"
  ) +
  geom_point(
    aes(x=year, y=population)
  )

fit <- lm(population ~ year + district + year:district, data=pop)

newdata <- data.frame(
  district = c("Moshi", "Siha"),
  year = c(2022, 2022)
) |> 
  add_predictions(fit, var="population") |> 
  mutate(population = round(population))

pop <- pop |> 
  bind_rows(newdata)

pop |> 
  ggplot() +
  geom_smooth(
    aes(x=year, y = population, color=district),
    method="lm"
  ) +
  geom_point(
    aes(x=year, y=population)
  )


# Environmental data ---------------------------------------------------------------------

pm2p5 <- read_csv(
  file.path(data_path, "raw", "avgpm25_monthly.csv"),
  col_select = -1
)

# New PM2.5 data for Siha (without considering National Park area)
pm2p5_siha <- read_csv(
  file.path(data_path, "raw", "avg_pm25_monthly_siha_np.csv"),
  col_select = -1
)

# Substitute Siha observations in main pm2p5 dataframe
# New PM2.5 data for Siha (without considering National Park area)
pm2p5_siha <- pm2p5_siha |> 
  mutate(
    District = case_when(
      .default = District,
      District == "Siha_without_nationalpark" ~ "Siha"
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


# Regular formatting
pm2p5 <- pm2p5 |> 
  mutate(
    time = ym(paste0(year, month))
  ) |>
  select(- c(year, month)) |> 
  rename(
    district=District,
    pm2p5=avg_pm2.5_monthly
  ) |> 
  relocate(time)

pm2p5 |> head()


# ---

green <- read_csv(
  file.path(data_path, "raw", "monthwise_greenness.csv")
)


green <- green |> 
  pivot_longer(
    cols = 2:last_col(),
    names_to = c("district", "month"),
    values_to = "greenness",
    names_sep = "_"
  )

green <- green |> 
  mutate(
    time = ym(paste0(year, month))
  ) |> 
  select( - c(year, month)) |> 
  mutate(
    district = recode(district,
                      moshi = "Moshi",
                      siha = "Siha")
  ) |> 
  relocate(time)

green |> head()

# ---

rain <- read_csv(
  file.path(data_path, "raw", "rc_month_rain_satellite.csv"),
  col_select = -1
)


rain <- rain |> 
  mutate(
    time = ym(paste0(year, month))
  ) |> 
  select( - c(year, month)) |> 
  relocate(time) |> 
  rename(total_rainfall = "Monthly Total Rainfall",
         n_raindays = "Number of Raindays")

rain |> head()

# ---

temp <- read_csv(
  file.path(data_path, "raw", "sat_temp.csv"),
  col_select = -1
)


temp <- temp |> 
  mutate(
    time = ym(paste0(year, month))
  ) |> 
  select( - c(year, month)) |> 
  relocate(time, district)

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
  relocate(time, district, temp_mean, temp_min, temp_max)

temp |> head()

# Join

environ_df <- pm2p5 |>
  full_join(green, by=c("time", "district")) |> 
  full_join(rain, by=c("time", "district")) |> 
  full_join(temp, by=c("time", "district"))

environ_df <- environ_df |> 
  arrange(district, time)

# Disease data from 2014
environ_df <- environ_df |> 
  filter(year(time) >= 2014)

# Add yearly population
environ_df <- environ_df |> 
  mutate(year = year(time)) |> 
  left_join(pop, by=c("district", "year")) |> 
  select(- year) |> 
  relocate(time, district, population)

environ_df |> 
  print(n=Inf)

# environ_df |> 
#   filter(district=="Moshi") |> 
#   as.data.frame() |> 
#   is.na() |> 
#   sum()
# 
# environ_df |> 
#   filter(district=="Siha") |> 
#   as.data.frame() |> 
#   is.na() |> 
#   sum()


var_order <- colnames(environ_df)[3:10]

environ_df |>
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
    values = c("steelblue3", "gray20")
  ) +
  # scale_fill_okabe_ito() +
  facet_wrap(
    ~ district,
    ncol=2
  )


environ_df |> 
  ggplot() +
  geom_point(
    aes(x=time, y=greenness, color=district)
  ) +
  geom_line(
    aes(x=time, y=greenness, color=district)
  )


saveRDS(
  environ_df,
  file.path(data_path, "processed", "environmental-variables_moshi-siha_monthly_2014-2022.Rds")
  )

# Additional air quality variables (2018-) -----------------------------------------------

# NO2

no2 <- read_csv(
  file.path(data_path, "raw", "no2_monthly_2018_2021.csv"),
  col_select = -1
)

no2 |> 
  head(20)

# There's many observations per month and I don't understand

# O3

# SO2

# AOD



# Diagnosis data -------------------------------------------------------------------------

# Moshi

moshi <- read_csv(
  file.path(data_path, "raw", "Moshi_monthlydata_cleaned.csv"),
  col_select = -1
)


# Numbers are parsed as character because they have spaces between them
moshi <- moshi |> 
  mutate_at(
    vars(Jan:Dec), function(x) as.double(str_replace(x, " ", ""))
  )

moshi <- moshi |> 
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "n_cases"
  ) 

moshi <- moshi |> 
  mutate(
    time = ym(paste0(Year, month))
  ) |> 
  select(- c(Year, month)) |> 
  mutate(
    district = "Moshi"
  ) |> 
  relocate(time, district, n_cases) |>
  rename(
    disease = Diseases,
    disease_communicable = Category,
    disease_group = Subcategory,
    disease_group_comment = Subcategory_WM
  )
  
moshi


# ---

# Siha

siha <- read_csv(
  file.path(data_path, "raw", "Siha_monthlydata_cleaned.csv"),
  col_select = -1
)



siha <- siha |> 
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "n_cases"
  ) 

siha <- siha |> 
  mutate(
    time = ym(paste0(Year, month))
  ) |> 
  select(- c(Year, month)) |> 
  rename(
    district=District
  ) |> 
  relocate(time, district, n_cases) |>
  rename(
    disease = Diseases,
    disease_communicable = Category,
    disease_group = Subcategory,
    disease_group_comment = Subcategory_WM
  )


# Merge

disease_df <- full_join(moshi, siha, by=colnames(moshi))

disease_df <- disease_df |> 
  arrange(district, disease, time)

# Fix categories
disease_df <- disease_df |> 
  mutate(
    disease = case_when(
      .default = disease,
      disease == "Snake And Insect Bites" ~ "Snake and Insect Bites"
    ),
    disease_group = case_when(
      .default = disease_group,
      disease_group == "diarrheal disease" ~ "Diarrheal Disease",
      disease_group == "Diarrheal disease" ~ "Diarrheal Disease",
      disease_group == "Other vector borne" ~ "Other Vector Borne",
      disease_group == "Mental health Disorders" ~ "Mental Health Disorders",
      disease_group == "Neglected Tropical Disease (Ntd)" ~ "Neglected Tropical Disease (NTD)",
      disease_group == "Other Cd" ~ "Other CD",
      disease_group == "Other Ncd" ~ "Other NCD",
      disease_group == "Trauma And Injuries" ~ "Trauma and Injuries",
    )
  )


disease_df$disease |> table()

disease_df$disease_group |> table()

disease_order <- disease_df |> 
  group_by(disease) |> 
  summarise(n = n()) |> 
  arrange(n) |> 
  pull(disease)


disease_df |> 
  mutate(disease = factor(disease, levels=rev(disease_order))) |>
  ggplot() +
  geom_tile(
    aes(x=time, y=disease, fill=n_cases),
    color="black",
    width=31
  ) +
  scale_fill_viridis_c() +
  facet_wrap(
    ~ district,
    ncol=2
  )


disease_order2 <- disease_df |> 
  group_by(disease_group) |> 
  summarise(n = n()) |> 
  arrange(n) |> 
  pull(disease_group)


disease_df |> 
  mutate(disease_group = factor(disease_group, levels=rev(disease_order2))) |>
  ggplot() +
  geom_tile(
    aes(x=time, y=disease_group, fill=n_cases),
    color="black",
    width=31
  ) +
  scale_fill_viridis_c() +
  facet_wrap(
    ~ district,
    ncol=2
  )


saveRDS(
  disease_df,
  file.path(data_path, "processed", "disease-cases_moshi-siha_monthly_2014-2022.Rds")
)

