### Data analysis for the manuscript: Unrecognized dengue transmission in socially
### vulnerable periurban neighborhoods of a temperate Argentine city: integrating
### serology with knowledge, attitudes and practices
### Author: Tamara Ricardo
# Última modificación: 19-06-2026 18:42

# Load packages ----------------------------------------------------------
pacman::p_load(
  glmmTMB,
  easystats,
  # performance,
  # DHARMa,
  gtsummary,
  flextable,
  scales,
  # labelled,
  cols4all,
  rio,
  janitor,
  tidyverse
)


# Load data --------------------------------------------------------------
data_denv <- import("clean/kap_dengue.rds")


# Clean dataset for regression -------------------------------------------
data_glm <- data_denv |>
  # Select columns
  select(
    dengue,
    barrio,
    genero:ocupacion,
    viv_tipo_calle:viv_acum_agua30d,
    viv_fuente_agua,
    con_alguien,
    con_sintomas,
    con_trasmision,
    act_sint6m_alguno,
    prac_cacharros,
    prac_resid_camion,
    starts_with("sc")
  ) |>

  # Remove NAs
  drop_na()


# Exploratory analysis ---------------------------------------------------
## Frequencies by neighborhood -----
tabyl(data_denv, barrio) |>
  adorn_pct_formatting()

## Knowledge score vs storage of water containers
data_denv |>
  mutate(across(
    .cols = c(prac_no_cacharros, con_trasm_cacharros),
    .fns = ~ factor(.x)
  )) |>

  tbl_summary(
    by = prac_no_cacharros,
    include = c(sc_conocimientos, con_trasm_cacharros),
    type = list(
      sc_conocimientos = "continuous"
    ),
    missing = "no"
  ) |>
  add_p()


## Symptoms of febrile illness vs actions taken
data_denv |>
  tbl_summary(
    by = act_sint6m_alguno,
    include = act_sint6m_acciones,
    digits = list(all_categorical() ~ c(0, 1))
  ) |>
  add_overall() |>
  add_p()


## Neighborhood issues ----
tabyl(data_denv$act_problemas_barrio) |>
  adorn_pct_formatting()


# Table 1 ----------------------------------------------------------------
tab1 <- data_denv |>
  # Generar tabla
  tbl_summary(
    by = barrio,
    include = c(
      viv_tipo_calle,
      viv_fuente_agua,
      ends_with("dist"),
      viv_suelo_cubierto,
      viv_techo_impermeable,
      viv_acum_agua,
      contains("30d")
    ),
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1))
  ) |>
  add_overall() |>
  add_p(pvalue_fun = label_style_pvalue(digits = 3)) |>

  # Table layout
  bold_p() |>
  bold_labels() |>
  modify_indent(columns = label, indent = 0)


# Figure 3 ---------------------------------------------------------------
## Data for the plot -----
dat <- data_denv |>
  # select_columns
  select(
    barrio,
    contains("con_sint_"),
    con_trasm_ns:con_trasm_viaje
  ) |>

  # Transform to long dataset
  pivot_longer(!barrio, names_to = "cat") |>

  # Separate columns
  separate_wider_regex(
    cols = cat,
    patterns = c(
      variable = "[^_]+_[^_]+",
      "_",
      category = ".*"
    )
  ) |>

  # Relabel variable
  mutate(
    variable = fct_relabel(
      variable,
      ~ c("Symptoms", "Ways of transmission")
    )
  ) |>

  # Relabel category
  mutate(
    category = fct_relabel(
      category,
      ~ c(
        "Water containers",
        "Fatigue",
        "Headache",
        "Diarrhea",
        "Retro-orbital pain",
        "Fever",
        "Influenza-like symptoms",
        "Malaise",
        "Dizzyness",
        "Myalgia",
        "Mosquito bites",
        "Nausea/Vomiting",
        "Doesn't know",
        "Bleeding",
        "Skin rash",
        "Travel to endemic areas"
      )
    ) |>
      fct_relevel("Doesn't know", after = Inf)
  ) |>

  # Calculate frequencies
  count(barrio, variable, category, wt = value == 1) |>

  # Calculate percentages
  mutate(
    pct = case_when(
      barrio == "CH" ~ n / nrow(data_denv |> filter(barrio == "CH")),
      barrio == "CS" ~ n / nrow(data_denv |> filter(barrio == "CS")),
      barrio == "VP" ~ n / nrow(data_denv |> filter(barrio == "VP"))
    )
  )


## Generate plot -----
fig3 <- dat |>
  ggplot(aes(x = category, y = pct, fill = barrio)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_col(position = "dodge", color = "grey50") +
  scale_fill_discrete_c4a_seq(palette = "blue_fluoride", name = NULL) +
  scale_y_continuous(name = "Frequency", labels = scales::label_percent()) +
  scale_x_discrete(name = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


## Save plot -----
ggsave(fig3, filename = "figure3.png", width = 16, units = "cm")


# Figure 4 ---------------------------------------------------------------
## Plot data -----
dat <- data_denv |>
  select(barrio, starts_with("act_mas")) |>

  # Transform to long format
  pivot_longer(!barrio, names_to = "variable", values_to = "category") |>

  # Relabel variable
  mutate(
    variable = fct_relabel(
      variable,
      ~ c(
        "More prevalent",
        "More risk of contagion",
        "More fear of contagion",
        "More publicity"
      )
    )
  ) |>

  # Relabel category
  mutate(
    category = if_else(
      category %in% c(NA, "None/not sure"),
      "None/doesn't know",
      category
    )
  ) |>

  # Estimate frequencys
  count(barrio, variable, category) |>

  # Estimate percentages
  mutate(
    pct = case_when(
      barrio == "CH" ~ n / nrow(data_denv |> filter(barrio == "CH")),
      barrio == "CS" ~ n / nrow(data_denv |> filter(barrio == "CS")),
      barrio == "VP" ~ n / nrow(data_denv |> filter(barrio == "VP"))
    )
  )


## Generate plot -----
fig4 <- dat |>
  ggplot(aes(x = category, y = pct, fill = barrio)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_col(position = "dodge", color = "grey50") +
  scale_fill_discrete_c4a_seq(palette = "blue_fluoride", name = NULL) +
  scale_y_continuous(name = "Frequency", labels = scales::label_percent()) +
  scale_x_discrete(name = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


## Save plot -----
ggsave(fig4, filename = "figure4.png", width = 16, units = "cm")


# Table 2: Univariate regressions ----------------------------------------
tab2 <- data_glm |>
  tbl_uvregression(
    y = dengue,
    method = glmmTMB::glmmTMB,
    formula = "{y} ~ {x} + (1 | barrio)",
    method.args = list(family = binomial),
    hide_n = TRUE,
    exponentiate = TRUE
  ) |> 
  
  # Table layout
  bold_p() |>
  bold_labels() |>
  modify_indent(columns = label, indent = 0)



# Multivariate regressions -----------------------------------------------
## Neighborhood as a random intercept -----
full_glmm1 <- glmmTMB(
  dengue ~ viv_tipo_calle +
    viv_baldios_dist +
    viv_fuente_agua +
    act_sint6m_alguno +
    prac_cacharros +
    (1 | barrio),
  family = binomial,
  data = data_glm
)

## Neighborhood as a fixed and random effect -----
full_glmm2 <- glmmTMB(
  dengue ~ viv_tipo_calle +
    viv_baldios_dist +
    viv_fuente_agua +
    act_sint6m_alguno +
    prac_cacharros +
    barrio +
    (1 | barrio),
  family = binomial,
  data = data_glm
)


## Neighborhood as a fixed effect -----
full_glm <- glmmTMB(
  dengue ~ viv_tipo_calle +
    viv_baldios_dist +
    viv_fuente_agua +
    act_sint6m_alguno +
    prac_cacharros +
    barrio,
  family = binomial,
  data = data_glm
)

# Compara modelos full
compare_performance(
  full_glmm1,
  full_glmm2,
  full_glm,
  metrics = "common",
  rank = FALSE
)

## Selección de variables a partir de full_glmm1
summary(full_glmm1)

# (-) prac_cacharros
glmm1 <- update(full_glmm1, ~ . - prac_cacharros)

# (-) síntomas febriles
glmm2 <- update(full_glmm1, ~ . - act_sint6m_alguno)

# (-) fuente de agua potable
glmm3 <- update(full_glmm1, ~ . - viv_fuente_agua)

# (-) baldíos
glmm4 <- update(full_glmm1, ~ . - viv_baldios_dist)

# (-) tipo de calle
glmm5 <- update(full_glmm1, ~ . - viv_tipo_calle)

# Compara modelos
compare_performance(
  full_glmm1,
  glmm1,
  glmm2,
  glmm3,
  glmm4,
  glmm5,
  metrics = "common",
  rank = TRUE
)


## Selección de variables a partir de glmm5
summary(glmm5)

# (-) tipo de calle
glmm5.1 <- update(glmm5, ~ . - prac_cacharros)

# (-) baldíos
glmm5.2 <- update(glmm5, ~ . - viv_baldios_dist)

# (-) fuente de agua potable
glmm5.3 <- update(glmm5, ~ . - viv_fuente_agua)

# (-) síntomas febriles
glmm5.4 <- update(glmm5, ~ . - act_sint6m_alguno)

# Compara modelos
compare_performance(
  glmm5,
  glmm5.1,
  glmm5.2,
  glmm5.3,
  glmm5.4,
  metrics = "common",
  rank = TRUE
)


## Selección de variables a partir de glmm5.1
summary(glmm5.1)

# (-) baldíos
glmm5.1.1 <- update(glmm5.1, ~ . - viv_baldios_dist)

# (-) fuente de agua
glmm5.1.2 <- update(glmm5.1, ~ . - viv_fuente_agua)

# (-) síntomas febriles
glmm5.1.3 <- update(glmm5.1, ~ . - act_sint6m_alguno)

# Compara modelos
compare_performance(
  glmm5.1,
  glmm5.1.1,
  glmm5.1.2,
  glmm5.1.3,
  metrics = "common",
  rank = TRUE
)


## Selección de variables a partir de glmm5.1.2
summary(glmm5.1.2)

# (-) baldíos
glmm5.1.2a <- update(glmm5.1.2, ~ . - viv_baldios_dist)

# (-) tipo de calle
glmm5.1.2b <- update(glmm5.1.2, ~ . - act_sint6m_alguno)

# Compara modelos
compare_performance(
  glmm5.1.2,
  glmm5.1.2a,
  glmm5.1.2b,
  metrics = "common",
  rank = FALSE
)

# Salida modelo
summary(glmm5.1.2b)

# Coeficientes modelo 5.2.1b
tbl_regression(glmm5.1.2b, exponentiate = TRUE)

# Efecto aleatorio
ranef(glmm5.1.2b)

# R2
r2(glmm5.1.2b)

# Residuales
check_model(glmm5.1.2b)

check_residuals(glmm5.1.2b)


### Limpiar environment
rm(list = ls())
