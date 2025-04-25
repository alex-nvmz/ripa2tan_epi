# Load libraries -------------------------------------------------------------------------

## General use -------
library(tidyverse)
library(glue)
library(patchwork)
library(scales)
library(colorspace)
library(latex2exp)

# ggplot2 options ------------------------------------------------------------------------

# custom discrete palette (Moshi - orange, Siha - green)
pal <- c("#dd8d29ff", "#446455ff")

full_pal <- c(
  pal,
  ggokabeito::palette_okabe_ito(c(2,4:9))
)

options(
  # Set colorblind-friendly palettes for ggplot
  ggplot2.discrete.colour = full_pal,
  ggplot2.discrete.fill = full_pal,
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
theme_set(
  theme_bw(base_size = 22)
)

# Set paths and environment variables ----------------------------------------------------

base_path <- file.path("..")
data_path <- file.path(base_path, "data")
res_path <- file.path(base_path, "results")

## subset of diseases --------------------------

main_dis <- c(
  # non-communicable
  "Chronic Respiratory Disease",
  "Hypertension",
  "Diabetes Mellitus",
  # communicable
  ## Respiratory Infections
  "Upper Respiratory Infections",
  ## Gastrointestinal Infections
  "Diarrhea",
  ## Urinary Infections
  "Urinary Tract Infections",
  ## Vector−borne Infections
  "Malaria",
  ## Infectious Eye Disease
  "Infectious Eye Disease"
)

# interesting candidates
alt_dis <- c(
  # communicable
  ## Respiratory Infections
  "Pneumonia, Severe",
  "Tuberculosis",
  ## Gastrointestinal Infections
  ##  Other Communicable Diseases
  "Skin Infection - Fungal",
  # non-communicable
  "Malnutrition",
  ## Other Non−Communicable
  "Peptic Ulcers",
  ## Neurological
  "Epilepsy",
  "Neuroses",
  "Psychoses"
)

# other diseases
other_dis <- c(
  # communicable
  ## Gastrointestinal Infections
  "Dysentery",
  "Intestinal Worms",
  # non-communicable
  "Neoplasms/Cancer",
  "Other Cardiovascular Diseases",
  ## Other Non−Communicable
  "Mild/Moderate Anemia",
  "Caries",
  "Poisoning",
  "Snake and Insect Bites",
  "Substance Abuse",
  ## Trauma
  "Fractures",
  "Road Traffic Accidents"
)

# unreliable diseases
unre_dis <- c(
  # communicable
  ## Gastrointestinal Infections
  "Typhoid",
  ##  Other Communicable Diseases
  "Leprosy",
  "Schistosomiasis"
)


## subset of exposures ------------

all_env <- c(
  "pm2p5",
  "total_rainfall",
  "n_raindays",
  "temp_min",
  "temp_mean",
  "temp_max",
  "utci",
  "greenness"
)

main_env <- c(
  "pm2p5",
  "total_rainfall",
  "n_raindays",
  "temp_max",
  "utci",
  "greenness"
)

# Functions ------------------------------------------------------------------------------

# glue alias
g <- glue::glue

# function that transforms tibble to data.frame and removes cache of rvars
# necessary to save rvars in .Rds with small file size
# see: https://github.com/stan-dev/posterior/issues/307
rvar_tib_postproc <-
  function(tib) {
    df <- as.data.frame(tib)
    rvar_i <- sapply(df, is_rvar)
    df[, rvar_i] <- lapply(df[, rvar_i, drop=FALSE], posterior:::invalidate_rvar_cache)
    return(df)
  }

# standardize a normal variable
standardize <- function(vec, na.rm=FALSE) {
  (vec - mean(vec, na.rm=na.rm))/sd(vec, na.rm=na.rm)
}

# undo standardization
destandardize <- function(vec, org_vec, na.rm=FALSE) {
  vec*sd(org_vec, na.rm=na.rm) + mean(org_vec, na.rm=na.rm)
}

# create a string of a credibility interval from an rvar object, with an optional star
# and possibility to bolden text with markdown notation
cint_str <- function(rvar, qlow, qhigh, acc, star=FALSE, star_val=0, star_bold=FALSE) {
  
  # if star should be added, get positions where
  if (star) {
    star_flags <-
      ifelse(
      (star_val >= quantile(rvar, qlow)) & (star_val <= quantile(rvar, qhigh)),
      FALSE,
      TRUE
    )
  }
  
  # {**}{median}({qlow}, {qhigh}){*}{**}
  
  glue::glue(
    "{
      if (star & star_bold) {
        ifelse(star_flags, '**', '')
      } else {''}
    }{
      rvar |> median() |> scales::number(acc)
    } ({
      rvar |> quantile(qlow) |> scales::number(acc)
    }, {
      rvar |> quantile(qhigh) |> scales::number(acc)
    }){
    if (star) {
        ifelse(star_flags, '*', '')
      } else {''}
    }{
      if (star & star_bold) {
          ifelse(star_flags, '**', '')
        } else {''}
    }"
  )
}
