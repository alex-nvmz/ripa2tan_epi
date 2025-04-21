# It creates descriptive plots of the final dataset in `/results/plots/final-description`
# now with incidence rates for the diseases


# setup
source("00_source.R")

library(ggh4x)  # facet_nested

# Population -----------------------------------------------------------------------------

# modeling period (2015 to 2021)

# load
pop <- read_csv(file.path(data_path, "processed", "final", "population_final.csv"))

pop <- pop |> 
  mutate(district = factor(district, levels=c("Moshi", "Siha")))

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
  file.path(res_path, "plots", "final-description", "population_2015-2021.pdf"),
  width=20*0.8, height=10.6*0.7
)


# Environment ----------------------------------------------------------------------------

# Plot time series for modeling period (2015 to 2021)

# load
envir <- read_csv(file.path(data_path, "processed", "final", "environmental_final.csv"))

envir <- envir |> 
  mutate(district = factor(district, levels=c("Moshi", "Siha")))

envir |> 
  print(width=Inf, n=13)

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
  file.path(res_path, "plots", "final-description", "environmental_2015-2021.pdf"),
  width=20*0.9, height=10.6*2.2
)


# Disease --------------------------------------------------------------------------------

# incidence rates per 1e5 people

## All available data (2014 to 2022) ----------------------------------

dis_all <-
  read_csv(
    file.path(data_path, "processed", "interim", "disease_final_complete.csv")
  )

# population with 2022 imputation

pop_imp <- read_csv(file.path(data_path, "processed", "interim", "population_2022-imputed.csv"))

# add population to disease df
dis_all <-
  dis_all |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop_imp, year=year(date)), join_by(year, district)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

dis_all <- dis_all |> 
  mutate(district = factor(district, levels=c("Moshi", "Siha")))

dis_all

# get empirical case rate per 1e5 people
dis_all <-
  dis_all |>
  mutate(
    case_rate_1e5 = n_cases / population * 1e5
  )

### Heatmap ----------

# Function to plot heatmaps of incidence rates per 1e5 people
plot_disease_table <- function(df, group_over, group_under) {
  df |> 
    ggplot() +
    geom_tile(
      aes(x=date, y=disease, fill=case_rate_1e5),
      color="black",
      width=31
    ) +
    scale_fill_viridis_c(
      transform = "log1p",
      breaks = c(0,10,100,1000,5000),
      labels = comma
    #   # breaks = c(0,10,100,1000,6000),
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
      fill = "Incidence rate\nper 100,000 people",
      x = "Date (monthly)",
      y = ""
    ) +
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(angle=0),
    )
}

dis_all |> 
  plot_disease_table(group_over = disease_block, group_under = disease_group)


ggsave(
  file.path(res_path, "plots", "final-description", "disease_final-groups_post-processing_all-time.pdf"),
  width=20*1.4, height=10.6*1.6
)

### Time series --------

df_iter <-
  dis_all |> 
  select(disease, disease_block, disease_group) |> 
  distinct() |> 
  arrange(disease_block, disease_group)


pmap(
  df_iter,
  function(disease, disease_block, disease_group, ...) {
    
    plot_dat <-
      dis_all |> 
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
        aes(x=date, y=case_rate_1e5, color=district), linewidth=0.9
      ) +
      geom_point(
        aes(x=date, y=case_rate_1e5, color=district), size=2.5, alpha=0.7
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = "Incidence rate\nper 100,000 people",
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
  file.path(res_path, "plots", "final-description", "disease_incidence-rate-1e5_all-time.pdf"),
  width=20*1.0, height=10.6*10,
  limitsize = FALSE
)


## Modeling period (2015 to 2021) -------------------------------------

dis <-
  read_csv(
    file.path(data_path, "processed", "final", "disease_final.csv")
  )

# add population to disease df
dis <-
  dis |> 
  mutate(year = year(date)) |> 
  left_join(
    mutate(pop, year=year(date)), join_by(year, district)
  ) |> 
  select(- c(year, date.y)) |> 
  rename(date = date.x)

dis <- dis |> 
  mutate(district = factor(district, levels=c("Moshi", "Siha")))

dis

# get empirical case rate per 1e5 people
dis <-
  dis |>
  mutate(
    case_rate_1e5 = n_cases / population * 1e5
  )

### Heatmap ----------

dis |> 
  plot_disease_table(group_over = disease_block, group_under = disease_group)

ggsave(
  file.path(res_path, "plots", "final-description", "disease_final-groups_post-processing_2015-2021.pdf"),
  width=20*1.4, height=10.6*1.6
)

### Time series --------


df_iter <-
  dis |> 
  select(disease, disease_block, disease_group) |> 
  distinct() |> 
  arrange(disease_block, disease_group)


pmap(
  df_iter,
  function(disease, disease_block, disease_group, ...) {
    
    plot_dat <-
      dis |> 
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
        aes(x=date, y=case_rate_1e5, color=district), linewidth=0.9
      ) +
      geom_point(
        aes(x=date, y=case_rate_1e5, color=district), size=2.5, alpha=0.7
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = "Incidence rate\nper 100,000 people",
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
  file.path(res_path, "plots", "final-description", "disease_incidence-rate-1e5_2015-2021.pdf"),
  width=20*0.9, height=10.6*10,
  limitsize = FALSE
)

