### Análisis de datos: Seropositivity to Dengue virus DENV in
### three neighborhoods in the periphery of a city with a recent history of
### outbreaks in Argentina: what can we learn from unreported infections?
### Author: Tamara Ricardo
### Last update: # 2025-10-30 13:45:19

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  # Estadísticas
  glmmTMB,
  performance,
  DHARMa,
  gtsummary,
  # Formato tablas
  flextable,
  labelled,
  # Gráficos
  scico,
  # Manejo de datos
  rio,
  janitor,
  tidyverse
)

# Cargar/limpiar datos KAP dengue ----------------------------------------
datos <- import("clean/kap_dengue.rds")


# Crear dataset con observaciones completas ------------------------------
datos_glm <- datos |>
  # Seleccionar columnas relevantes
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

  # Descartar filas con NAs
  drop_na()


# Análisis exploratorio --------------------------------------------------
## Frecuencias x barrio
tabyl(datos, barrio) |>
  adorn_pct_formatting()


## Relación entre conocimientos y acumulación de cacharros
datos |>
  mutate(
    prac_no_cacharros = factor(prac_no_cacharros),
    con_trasm_cacharros = factor(con_trasm_cacharros)
  ) |>
  tbl_summary(
    by = prac_no_cacharros,
    type = list(
      sc_conocimientos = "continuous"
    ),
    include = c(sc_conocimientos, con_trasm_cacharros)
  ) |>
  add_p()


# Síntomas febriles vs acciones
tabyl(datos, act_sint6m_alguno, act_sint6m_acciones) |>
  adorn_totals() |>
  adorn_percentages() |>
  adorn_pct_formatting()


# Problemas barrio
tabyl(datos$act_problemas_barrio)


# Tabla 1 ----------------------------------------------------------------
## Características de la vivienda por barrio
tab1 <- datos |>
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

  # Añadir significancia
  add_overall() |>
  add_p(pvalue_fun = label_style_pvalue(digits = 3)) |>
  bold_p() |>
  bold_labels()


# Guardar tabla
tab1 |>
  as_flex_table() |>
  font(fontname = "Calibri", part = "all") |>
  fontsize(size = 11, part = "all") |>
  line_spacing(space = 1.5, part = "all") |>
  width(width = c(5, rep(2.3, 4), 1.7), unit = "cm") |>
  save_as_docx(path = "tab1.docx")


# Figura 2 ---------------------------------------------------------------
## Datos para el gráfico
datos_plot <- datos |>
  # Seleccionar columnas relevantes
  select(barrio, contains("con_sint_"), con_trasm_ns:con_trasm_viaje) |>

  # Pasar a formato long
  pivot_longer(!barrio, names_to = "categoria") |>

  # Identificador columna
  mutate(
    variable = if_else(
      str_detect(categoria, "con_sint"),
      "Symptoms",
      "Ways of trasmission"
    )
  ) |>

  # Asignar etiquetas
  mutate(
    categoria = var_label(datos)[categoria] |>
      unname() |>
      as.character()
  ) |>

  # Estimar frecuencias
  count(barrio, variable, categoria, wt = value == 1) |>

  # Estimar frecuencias relativas x barrio
  mutate(
    pct = case_when(
      barrio == "CH" ~ n / nrow(datos |> filter(barrio == "CH")),
      barrio == "CS" ~ n / nrow(datos |> filter(barrio == "CS")),
      barrio == "VP" ~ n / nrow(datos |> filter(barrio == "VP"))
    )
  ) |>

  # Unir niveles
  mutate(
    categoria = fct_collapse(
      categoria,
      "Doesn't know" = c("None", "Doesn't know any way of transmission")
    )
  ) |>

  # Enviar "No sé" al final
  mutate(categoria = fct_relevel(categoria, "Doesn't know", after = Inf))


## Generar gráfico
datos_plot |>
  ggplot(aes(x = categoria, y = pct, fill = barrio)) +
  geom_col(position = "dodge", color = "grey50") +
  facet_wrap(~variable, scales = "free_x") +

  # Formato
  scale_fill_scico_d(palette = "lipari") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = NULL, y = "Frequency (%)") +

  # Tema
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, size = 10, face = "bold"),
    strip.text = element_text(size = 11, face = "bold")
  )


# Guarda figura
ggsave("figure2.png", width = 16, height = 14, units = "cm")


# Figura 3 ---------------------------------------------------------------
## Datos para el gráfico
datos_plot <- datos |>

  select(barrio, starts_with("act_mas")) |>

  # Base long
  pivot_longer(!barrio, names_to = "variable", values_to = "categoria") |>

  # Asignar etiquetas variable
  mutate(
    variable = var_label(datos)[variable] |>
      unname() |>
      as.character()
  ) |>

  # Modificar niveles categoría
  mutate(
    categoria = case_when(
      categoria == "Igual" ~ "Both",
      categoria == "Ninguna/no sabe" | is.na(categoria) ~ "None/doesn't know",
      .default = categoria
    ) |>
      fct_relevel("Both", "None/doesn't know", after = 2)
  ) |>

  # Estimar frecuencias
  count(barrio, categoria, variable) |>

  # Estimar frecuencias relativas x barrio
  mutate(
    pct = case_when(
      barrio == "CH" ~ n / nrow(datos |> filter(barrio == "CH")),
      barrio == "CS" ~ n / nrow(datos |> filter(barrio == "CS")),
      barrio == "VP" ~ n / nrow(datos |> filter(barrio == "VP"))
    )
  )


## Generar gráfico
datos_plot |>
  ggplot(aes(x = categoria, y = pct, fill = barrio)) +
  geom_col(position = "dodge", color = "grey50") +
  facet_wrap(~variable, scales = "free_x") +

  # Formato
  scale_fill_scico_d(palette = "lipari") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = NULL, y = "Frequency (%)") +

  # Tema
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, size = 10, face = "bold"),
    strip.text = element_text(size = 11, face = "bold")
  )


# Guarda figura
ggsave("figure3.png", width = 16, height = 14, units = "cm")


# Factores que afectan la seropositividad ---------------------------------
## Barrio como intercepto aleatorio
full_glmm1 <- glmmTMB(
  dengue ~ viv_tipo_calle +
    viv_baldios_dist +
    viv_fuente_agua +
    act_sint6m_alguno +
    prac_cacharros +
    (1 | barrio),
  family = binomial,
  data = datos_glm
)

## Barrio como intercepto aleatorio y efecto fijo
full_glmm2 <- glmmTMB(
  dengue ~ viv_tipo_calle +
    viv_baldios_dist +
    viv_fuente_agua +
    act_sint6m_alguno +
    prac_cacharros +
    barrio +
    (1 | barrio),
  family = binomial,
  data = datos_glm
)

## Barrio como efecto fijo
full_glm <- glmmTMB(
  dengue ~ viv_tipo_calle +
    viv_baldios_dist +
    viv_fuente_agua +
    act_sint6m_alguno +
    prac_cacharros +
    barrio,
  family = binomial,
  data = datos_glm
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
