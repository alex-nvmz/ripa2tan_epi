library(readxl)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(stringr)

# Load your assets data (adjust file path as needed)
assets_df <- read_xlsx("D:/github/RIPATAN_EPIDEMIOLOGY/data_for_figures/all_assets.xlsx")

# Define classification vectors (converted to sentence case)
modern_urban <- c("Telephone (land line)", "Mobile phone", "Motovehicle", "Motorcycle/Vespa",
                  "Trimotocycle/Bajaji", "Television", "Electric Iron", "Refrigerator/Freezer",
                  "Cooker Electric or Gas", "Computer or Laptop", "House Internet Facility") %>% 
  str_to_sentence()
traditional_rural <- c("Charcoal Iron", "Plough", "Power tiller", "Hand hoe", "Wheelbarroe",
                       "Oxen", "Donkey or Camel", "Land or farm", "Bicycle","Tricycle/guta","Radio") %>% 
  str_to_sentence()
general <- c("House", "Bicycle", "Tricycle/Guta", "Radio") %>% 
  str_to_sentence()

# Convert asset names in your data to sentence case.
# For "donkey" or "camel", force the entire string to uppercase.
assets_df <- assets_df %>%
  mutate(Ownership_of_assets = str_to_sentence(Ownership_of_assets))
         
# Add a Category column based on the classification vectors
assets_df <- assets_df %>%
  mutate(Category = case_when(
    Ownership_of_assets %in% modern_urban      ~ "Modern/Urban",
    Ownership_of_assets %in% traditional_rural ~ "Traditional/Rural",
    Ownership_of_assets %in% general            ~ "General",
    TRUE                                       ~ "Unclassified"
  ))

# Compute percentage change (2022 - 2012)
assets_df <- assets_df %>%
  mutate(Percent_Change = Percentage_2022 - Percentage_2012)

# Filter to keep only Modern/Urban and Traditional/Rural
assets_df <- assets_df %>% filter(Category %in% c("Modern/Urban", "Traditional/Rural"))

# Define custom color palettes using wesanderson
moshi_pal <- wes_palette("FantasticFox1", 2, type = "discrete")[1]
siha_pal  <- wes_palette("Chevalier1", 4, type = "discrete")[1]

p1 <- ggplot(assets_df, aes(x = reorder(Ownership_of_assets, Percent_Change),
                            y = Percent_Change,
                            fill = District)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  coord_flip() +
  # Use free y scales and shift the facet strip to the right,
  # with each facet displaying its own y axis.
  facet_wrap(~ Category, scales = "free_y", ncol = 1, switch = "y", strip.position = "bottom") +
  labs(
    x = NULL,
    y = "Percentage change (2012 to 2022)",
    fill = "District"
    # No plot title
  ) +
  scale_fill_manual(values = c("Moshi" = moshi_pal, "Siha" = siha_pal)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # Remove panel border and add a box around the facet label (strip)
    strip.background = element_rect(color = "black", fill = "white", size = 1),
    # Increase the facet label (strip text) size and add bottom margin
    strip.text = element_text(size = 28, face = "bold", color = "black", margin = margin(b = 10)),
    # Increase axis titles and add extra margin between the title and axis labels
    axis.title.x = element_text(size = 28, face = "bold", color = "black", margin = margin(t = 10, b = 10)),
    axis.title.y = element_text(size = 28, face = "bold", color = "black", margin = margin(r = 10, l = 10)),
    axis.text.x  = element_text(size = 28, face = "bold", color = "black"),
    axis.text.y  = element_text(size = 26, color = "black"),
    legend.title = element_text(size = 28, face = "bold", color = "black"),
    legend.text  = element_text(size = 28, face = "bold", color = "black"),
    panel.spacing = unit(0.2, "lines"),
    strip.placement = "outside",
    legend.position = "none"
  )
p1

ggsave("D:/github/RIPATAN_EPIDEMIOLOGY/submission/ownership_assets.tif", plot = p1, dpi = 300, width = 13, height = 14,
       bg = "white")
