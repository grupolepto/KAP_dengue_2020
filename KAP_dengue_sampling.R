### Data analysis for the manuscript: Unrecognized dengue transmission in socially
### vulnerable periurban neighborhoods of a temperate Argentine city: integrating
### serology with knowledge, attitudes and practices
### Author: Tamara Ricardo
# Última modificación: 19-06-2026 14:11

# Load packages ----------------------------------------------------------
pacman::p_load(
  epiR,
  rio,
  janitor,
  tidyverse
)

# Sample size calculation ------------------------------------------------
## Chalet -----
epi.sssimpleestb(N = 277, epsilon = .2, Py = .5, se = 1, sp = 1)

## Colastiné Sur -----
epi.sssimpleestb(N = 314, epsilon = .2, Py = .5, se = 1, sp = 1)


## Vuelta del Paraguayo -----
epi.sssimpleestb(N = 99, epsilon = .2, Py = .5, se = 1, sp = 1)


# Dengue incidence -------------------------------------------------------
## Load/clean dataset -----
data_msal_2020 <- import(
  "raw/informacion-publica-dengue-zika-nacional-hasta-20201231_1.xlsx"
) |>
  ## Remove id columns
  select(-ends_with("_id")) |>

  ### Select cases from Santa Fe
  filter(provincia_nombre == "Santa Fe") |>

  # Clean NAs
  mutate(across(
    .cols = where(is.character),
    .fns = ~ if_else(
      .x %in% c("*sin dato*", "++++++++++++", "#¡VALOR!", "(en blanco)"),
      NA,
      .x
    )
  )) |>

  ### Group data
  count(año, semanas_epidemiologicas, wt = cantidad_casos)


## Cases of dengue in Santa Fe (2020) -----
sum(data_msal_2020$n)


# Incidence of dengue in Santa Fe (2020) -----
epi.prev(
  pos = sum(data_msal_2020$n),
  tested = 572265,
  sp = 1,
  se = 1,
  units = 1e4
)


# Clean working environment ----------------------------------------------
rm(list = ls())
