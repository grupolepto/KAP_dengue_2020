### KAP dengue: estadísticas nacionales 2020 y cálculo tamaño muestral
### Autora: Tamara Ricardo
### Fecha modificación: # 2025-10-28 12:11:13

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  epiR,
  tidyverse
)


# Cargar datos MSAL 2020 --------------------------------------------------
datos_msal_2020 <- import(
  "raw/informacion-publica-dengue-zika-nacional-hasta-20201231_1.xlsx"
) |>

  # Descartar columnas innecesarias
  select(-ends_with("_id")) |>

  # Seleccionar casos de Santa Fe
  filter(provincia_nombre == "Santa Fe") |>

  # Definir NAs
  mutate(across(
    .cols = where(is.character),
    .fns = ~ if_else(
      .x %in% c("*sin dato*", "++++++++++++", "#¡VALOR!", "(en blanco)"),
      NA,
      .x
    )
  )) |>

  # Agrupar por semana epidemiológica
  count(año, semanas_epidemiologicas, wt = cantidad_casos)


## Análisis exploratorio
# Total casos dengue Santa Fe semanas 1-52 año 2020
sum(datos_msal_2020$n)

# Incidencia Santa Fe 2020
attack_rate(
  cases = sum(datos_msal_2020$n),
  population = 572265,
  multiplier = 10^4,
  mergeCI = T,
  digits = 1
)

# Limpia environment
rm(list = ls())


# Calcular tamaño muestral -----------------------------------------------
## Chalet
epi.sssimpleestb(N = 277, epsilon = .2, Py = .5, se = 1, sp = 1)

# Ajustar x sensibilidad y especificidad test
epi.sssimpleestb(N = 277, epsilon = .2, Py = .5, se = .96, sp = .93)

## Colastiné Sur
epi.sssimpleestb(N = 314, epsilon = .2, Py = .5, se = 1, sp = 1)

# Ajustar x sensibilidad y especificidad test
epi.sssimpleestb(N = 314, epsilon = .2, Py = .5, se = .96, sp = .93)

## Vuelta del Paraguayo
epi.sssimpleestb(N = 99, epsilon = .2, Py = .5, se = 1, sp = 1)

# Ajustar x sensibilidad y especificidad test
epi.sssimpleestb(N = 99, epsilon = .2, Py = .5, se = .96, sp = .93)
