# It creates descriptive plots of the initial dataset in /results/plots/initial-description
# 
# Creates visualizations of different classifications of the diseases,
# and saves the final disease groups in `/data/processed/initial/disease-grouping.csv`

# setup
source("00_source.R")

library(ggh4x)  # facet_nested

# Population -----------------------------------------------------------------------------

# load
pop <- read_csv(file.path(data_path, "processed", "initial", "population.csv"))

pop <- pop |> 
  mutate(district = factor(district, levels=c("Moshi", "Siha")))

# visualize population
pop |>
  ggplot() +
  geom_vline(
    aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
  ) +
  geom_vline(
    aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
  ) +
  geom_line(
    aes(x=date, y=population, color=district), linewidth=0.8
  ) +
  geom_point(
    aes(x=date, y=population, color=district), size=3
  ) +
  scale_y_continuous(labels = comma, n.breaks = 6) +
  scale_x_date(
    date_breaks="year", date_labels = "%Y",
  ) +
  labs(
    color = "District",
    x = "Year",
    y = "Population"
  )

ggsave(
  file.path(res_path, "plots", "initial-description", "population.pdf"),
  width=20*0.8, height=10.6*0.7
)

# data from 2012 to 2021, yearly
# will have to impute year 2022


# Environmental --------------------------------------------------------------------------

# load
envir <- read_csv(file.path(data_path, "processed", "initial", "environmental.csv"))

envir <- envir |> 
  mutate(district = factor(district, levels=c("Moshi", "Siha")))

envir |> 
  print(width=Inf, n=13)


## plot time series --------------------
df_iter <- tribble(
  ~var, ~label, ~ units,
  "pm2p5", "PM2.5", "($\\mu$g/m$^3$)",
  "greenness", "Greenness", "(NDVI)",
  "total_rainfall", "Rainfall", "(mm)",
  "n_raindays", "No. rain days", "",
  "temp_min", "Min. temp.", "(ºC)",
  "temp_mean", "Mean temp.", "(ºC)",
  "temp_max", "Max. temp.", "(ºC)",
  "utci", "UTCI", ""
)

pmap(
  df_iter,
  function(var, label, units, ...) {
    envir |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date, y=.data[[var]], color=district), linewidth=0.8
      ) +
      geom_point(
        aes(x=date, y=.data[[var]], color=district), size=2.5
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = TeX(glue("{label} {units}"))
      )
  }
) |> 
  wrap_plots(
    ncol=1,
    guides="collect",
  ) +
  plot_layout(axis_titles="collect")

ggsave(
  file.path(res_path, "plots", "initial-description", "environmental.pdf"),
  width=20*1, height=10.6*2.2
)

# data from 2012 to 2021 for most variables. Up to 2022 for pm2p5 and utci.

# some missings for greenness
# missing rain for Siha in 2021
# one month missing for utci


## plot missingness ---------

var_order <- colnames(envir)[3:10]

envir |>
  pivot_longer(
    cols=pm2p5:utci,
    names_to = "var"
  ) |> 
  mutate(
    var = factor(var, levels = var_order)
  ) |>
  ggplot() +
  geom_tile(
    aes(x=date, y=var, fill=is.na(value)),
    color="black",
    width=31
  ) +
  scale_fill_manual(
    values = c("steelblue3", "gray20"),
    labels = c("No", "Yes")
  ) +
  facet_wrap(
    ~ district,
    ncol=2
  ) +
  labs(
    fill = "Missing",
    x = "Date (monthly)",
    y = ""
  )

ggsave(
  file.path(res_path, "plots", "initial-description", "environmental_missing.pdf"),
  width=20*1, height=10.6*1
)

# same conclusions as above


# Disease --------------------------------------------------------------------------------

dis <- read_csv(file.path(data_path, "processed", "initial", "disease.csv"))


## plot missingness ------------------------

# a lot of data is not even coded as missing, fix that
dis <- dis |> 
  complete(district, date, nesting(disease, disease_group, disease_communicable))


dis |>
  ggplot() +
  geom_tile(
    aes(x=date, y=disease, fill=is.na(n_cases)),
    color="black",
    width=31
  ) +
  scale_fill_manual(
    values = c("steelblue3", "gray20"),
    labels = c("No", "Yes")
  ) +
  facet_nested(
    cols = vars(district),
    rows = vars(disease_communicable, disease_group),
    scales = "free_y",
    space = "free_y",
    strip = strip_nested(size = "variable", clip="off", bleed = FALSE),
    solo_line = TRUE,
    nest_line = TRUE,
    # switch = "y"
  ) +
  labs(
    fill = "Missing",
    x = "Date (monthly)",
    y = ""
  ) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(angle=0),
  )

ggsave(
  file.path(res_path, "plots", "initial-description", "disease_missing.pdf"),
  width=20*1.5, height=10.6*1.6
)


## visualize proposed grouping changes ----------------

# The researchers proposed different groupings of diseases
# we will visualize the data according to them and will save the definitive grouping

# Type manually :(

### Renz groups ------------

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


### Skevaki groups ----------------------

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
    "Mild/Moderate Anemia",
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


### Final grouping (after discussion) ----------------------

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
    "Mild/Moderate Anemia",
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

### Plot groupings ---------------------------

# Add new disease groupings to new dataframe
dis_ex <- dis |> 
  left_join(ref_renz) |>
  left_join(ref_skevaki) |> 
  left_join(ref_final)

dis_ex

# Function to plot heatmaps of no. of cases
plot_disease_table <- function(df, group_over, group_under) {
  df |> 
    ggplot() +
    geom_tile(
      aes(x=date, y=disease, fill=n_cases),
      color="black",
      width=31
    ) +
    scale_fill_viridis_c(
      transform = "log1p",
      breaks = c(0,10,100,1000,10000),
      labels = comma
      # breaks = c(0,10,100,1000,6000),
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
        # title.vjust = 2,
      )
    ) +
    labs(
      fill = "No. of cases",
      x = "Date (monthly)",
      y = ""
    ) +
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(angle=0),
    )
}


# original groups
dis_ex |> 
  plot_disease_table(group_over = disease_communicable, group_under = disease_group)

ggsave(
  file.path(res_path, "plots", "initial-description", "disease_original-groups.pdf"),
  width=20*1.5, height=10.6*1.6
)


# original groups again, but remove category with many missings
dis_ex |> 
  filter(disease_group != "Other vector borne diseases") |> 
  plot_disease_table(group_over = disease_communicable, group_under = disease_group)

ggsave(
  file.path(res_path, "plots", "initial-description", "disease_original-groups_minus-missings.pdf"),
  width=20*1.5, height=10.6*1.6
)

# Skevaki groups
dis_ex |> 
  filter(
    !is.na(disease_group_skevaki)
  ) |>
  plot_disease_table(group_over = disease_block_skevaki, group_under = disease_group_skevaki)

ggsave(
  file.path(res_path, "plots", "initial-description", "disease_skevaki-groups.pdf"),
  width=20*1.5, height=10.6*1.6
)

# Renz groups
dis_ex |> 
  filter(
    !is.na(disease_group_renz)
  ) |>
  plot_disease_table(group_over = disease_block_skevaki, group_under = disease_group_renz)

ggsave(
  file.path(res_path, "plots", "initial-description", "disease_renz-groups.pdf"),
  width=20*1.5, height=10.6*1.6
)

# Final groups
dis_ex |> 
  filter(
    !is.na(disease_group_final)
  ) |>
  plot_disease_table(group_over = disease_block_final, group_under = disease_group_final)

ggsave(
  file.path(res_path, "plots", "initial-description", "disease_final-groups.pdf"),
  width=20*1.5, height=10.6*1.6
)

### Save grouping reference --------

ref_final |> 
  write_csv(
    file.path(data_path, "processed", "initial", "disease-grouping.csv")
  )


## plot n_cases time series ---------------------

# keep the final grouping to help classify the files

dis_ex |> print(width=Inf)

dis_ex <-
  dis_ex |> 
  select(- c(disease_group, disease_communicable, disease_group_renz,
             disease_block_skevaki, disease_group_skevaki)) |> 
  rename(
    disease_block = disease_block_final,
    disease_group = disease_group_final
  )
  
df_iter <-
  dis_ex |> 
    select(disease, disease_block, disease_group) |> 
    distinct() |> 
    arrange(disease_block, disease_group)


pmap(
  df_iter,
  function(disease, disease_block, disease_group, ...) {
    
    plot_dat <-
      dis_ex |> 
      filter(disease == {{disease}})
    
    plot_dat |>
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date, y=n_cases, color=district), linewidth=0.9
      ) +
      geom_point(
        aes(x=date, y=n_cases, color=district), size=2.5, alpha=0.7
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = "No. of cases",
        title = glue("{disease_block} | {disease_group} | {disease}")
      ) +
      theme(
        title = element_text(size = rel(0.9))
      )
  }
) |>
  wrap_plots(
    ncol=1,
    guides="collect",
  ) +
  plot_layout(axis_titles="collect")

ggsave(
  file.path(res_path, "plots", "initial-description", "disease.pdf"),
  width=20*1.0, height=10.6*13,
  limitsize = FALSE
)

# data from 2014 to 2022
# some diseases with many missings


# conclusion:
# modeling period: 2015 to 2021 (both inclusive)
# obtain imputed datasets (environment) for this period

