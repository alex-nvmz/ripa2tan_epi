
# setup
source("00_source.R")
library(readxl)

pop <- read_xlsx(
  file.path(data_path, "netra_figs_data", "population.xlsx"),
  sheet = 1
  )
pop <- pop[,1:3]
summary(pop)
pop <- pop %>%
  mutate(Date = as.Date(ISOdate(Year, 1, 1)))

pop <-  pop %>% 
  pivot_longer(
    cols = 2:3, 
    names_to = "District",
    values_to = "Population"
  )

library(ggplot2)

p_pop <-
  ggplot(pop, aes(x = Date, y = Population / 1e6, color = District)) +
  # Plot lines and points with the same color mapping
  geom_line(linewidth = 2, show.legend = TRUE) +
  geom_point(size = 4, show.legend = TRUE) +
  
  # Use one scale for both color and fill, forcing the points to be filled
  # with the same color used for the lines
  scale_color_discrete(
    aesthetics = c("color", "fill"),             # apply same colors to color & fill
    name       = "District",                     # single legend title
    labels     = c("Moshi", "Siha"),             # legend labels
  ) +
  # Adjust y-axis limits and labels
  scale_y_continuous(limits = c(0.1, 0.257715)) +
  xlab("Year") +
  ylab("Population (in millions)") +
  scale_x_date(
    date_breaks="year", date_labels = "%Y"
  ) +
  theme(
    panel.grid.minor.x = element_blank()
  )
  

p_pop


## figures for nature communications
library(wesanderson)

### figure 1 with change in % population of age-groups

pop_df <- read_xlsx(
  file.path(data_path, "netra_figs_data", "pop_agegroup_2012_2022.xlsx")
)
pop_df$Age_group <- as.factor(pop_df$Age_group)

library(dplyr)

pop_2012 <- pop_df %>%
  filter(Year == 2012) %>%
  select(Age_group, Gender, Moshi_2012 = Moshi_MC, Siha_2012 = Siha)

pop_2022 <- pop_df %>%
  filter(Year == 2022) %>%
  select(Age_group, Gender, Moshi_2022 = Moshi_MC, Siha_2022 = Siha)

# Join on Age_group + Gender
pop_joined <- pop_2012 %>%
  inner_join(pop_2022, by = c("Age_group", "Gender"))

# Calculate percentage change:
# (Pop_2022 - Pop_2012) / Pop_2012 * 100
pop_change <- pop_joined %>%
  mutate(
    Moshi_pct_change = (Moshi_2022 - Moshi_2012) / Moshi_2012 * 100,
    Siha_pct_change  = (Siha_2022 - Siha_2012) / Siha_2012 * 100
  )

library(tidyr)

pop_long <- pop_change %>%
  select(Age_group, Gender, Moshi_pct_change, Siha_pct_change) %>%
  pivot_longer(
    cols = ends_with("pct_change"),
    names_to = "District",
    values_to = "Pct_Change"
  ) %>%
  # Make district names nicer
  mutate(District = ifelse(District == "Moshi_pct_change", "Moshi", "Siha"))


ggplot(pop_long, aes(x = Age_group, y = Pct_Change, fill = District)) +
  geom_col(
    position = position_dodge(),
    width=0.7
    ) +
  facet_wrap(~ Gender) +
  labs(
    x = "Age Group",
    y = "Percentage Change (2012 to 2022)",
    title = "Population Change by Age Group, Gender, and District"
  )


pop_pyramid <- pop_long %>%
  mutate(
    Pct_Change_mod = ifelse(District == "Moshi", -Pct_Change, Pct_Change)
  )

pop_pyramid$Age_group <- factor(pop_pyramid$Age_group, 
                                levels = c("<1", "1-4", "5-9", "10-14", "15-49", "50-60", "60+"))


# ------------------------------------------------------------------------------
# 2. Define Custom Colors from wesanderson
# ------------------------------------------------------------------------------
moshi_pal <- wes_palette("FantasticFox1", 2, type = "discrete")[1]
siha_pal  <- wes_palette("Chevalier1",   4, type = "discrete")[1]
pop_pyramid$Gender <- recode(pop_pyramid$Gender, "Male" = "Males", "Female" = "Females")
# ------------------------------------------------------------------------------
# 3. Plot the Population Pyramid with Labels
# ------------------------------------------------------------------------------

p <-
  ggplot(pop_pyramid, aes(x = Age_group, y = Pct_Change_mod, fill = District)) +
  # Use narrower bars
  geom_col(width = 0.6) +
  
  # Flip coordinates for pyramid style
  coord_flip() +
  
  # Facet by Gender with facet labels outside and extra bottom margin
  facet_wrap(~ Gender, strip.position = "top") +
  
  # Remove extra space around x-axis (flipped y-axis)
  # scale_x_discrete(expand = c(0, 0)) +
  
  # Display absolute values on the y-axis (removes negative sign for Moshi)
  scale_y_continuous(labels = function(x) abs(x)) +
  
  # Apply custom fill colors
  scale_fill_manual(values = c("Moshi" = moshi_pal, "Siha" = siha_pal)) +
  
  # Add percentage labels (set size smaller than axis & legend text)
  geom_text(
    aes(
      label = paste0(round(Pct_Change, 0), "%"),
      y = ifelse(District == "Moshi", Pct_Change_mod - 8, Pct_Change_mod + 8)
    ),
    color    = "black",
    size     = 8,
  ) +
  
  # Axis labels (no plot title)
  labs(
    x = "Age Groups",
    y = "Percentage Change (2012 to 2022)"
  )

# Print the plot
print(p)


ggsave(
  # "D:/github/RIPATAN_EPIDEMIOLOGY/submission/Population_Pyramid.tif",
  file.path(
    res_path, "plots", "figures", "Population_Pyramid.pdf"
  ),
  plot = p,
  # dpi = 300,
  width = 19, height = 8,
  # bg = "white"
)
