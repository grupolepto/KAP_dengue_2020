### Análisis de datos: Seropositivity to Dengue virus DENV in
### three neighborhoods in the periphery of a city with a recent history of
### outbreaks in Argentina: what can we learn from unreported infections?
### Author: Tamara Ricardo
### Last update:# 2025-10-28 14:47:44

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  # Estadísticas
  glmmTMB,
  performance,
  DHARMa,
  epikit,
  gtsummary,
  skimr,
  # Formato tablas
  flextable,
  labelled,
  # Gráficos
  scico,
  patchwork,
  GGally,
  # Manejo de datos
  rio,
  janitor,
  tidyverse
)

# Cargar/limpiar datos KAP dengue ----------------------------------------
datos <- import("clean/kap_dengue_clean.xlsx") |>

  # Cambiar etiquetas barrio
  mutate(
    barrio = case_when(
      barrio == "Chalet" ~ "CH",
      barrio == "Colastiné Sur" ~ "CS",
      TRUE ~ "VP"
    )
  ) |>

  # Variable respuesta binomial
  mutate(dengue = if_else(ELISA_dengue == "POS", 1, 0)) |>

  # Cambiar niveles tipo de calle
  mutate(
    viv_tipo_calle = fct_relabel(
      viv_tipo_calle,
      ~ c("Sand", "Paved/semi-paved", "Dirt")
    ) |>
      fct_relevel("Sand", after = Inf)
  ) |>

  # Cambiar niveles agua potable
  mutate(
    viv_fuente_agua = fct_relabel(
      viv_fuente_agua,
      ~ c("Tank truck/other", "Water pipes")
    )
  ) |>

  # Cambiar niveles área alcanzada por el agua
  mutate(
    viv_acum_agua30d_donde = fct_relabel(
      viv_acum_agua30d_donde,
      ~ c("Street", "House", "Front or backyard")
    ) |>
      fct_relevel("House", after = Inf)
  ) |>

  # Cambiar niveles tiempo agua
  mutate(
    viv_acum_agua30d_tiempo = fct_relabel(
      viv_acum_agua30d_tiempo,
      ~ c("Days", "Hours", "Weeks")
    ) |>
      fct_relevel("Hours", after = 0)
  ) |>

  # Cambiar niveles distancia a fuentes ambientales
  mutate(across(
    .cols = ends_with("_dist"),
    .fns = ~ case_when(
      .x == "Entre 25 y 50m" ~ "Between 25 and 50m",
      .x == "Menos de 25m" ~ "Less than 25m",
      .default = "Over 50m"
    ) |>
      fct_relevel("Less than 25m", after = 0)
  )) |>

  # Cambiar niveles variables binarias
  mutate(across(
    .cols = c(viv_suelo_cubierto:viv_acum_agua30d),
    .fns = ~ if_else(.x == "Si", 1, 0)
  )) |>

  # Recategorizar presencia de  cacharros
  mutate(prac_cacharros = if_else(prac_no_cacharros == 1, 0, 1)) |>

  # Categorizar problemas en el barrio
  mutate(
    act_problemas_barrio_cat = if_else(is.na(act_problemas_barrio), 0, 1),
    .after = act_problemas_barrio
  ) |>

  # Asignar etiquetas a nombres de columnas
  set_variable_labels(
    .labels = list(
      genero = "Sex",
      edad = "Age (years)",
      edad_cat = "Age group",
      barrio = "Site",
      viv_tipo_calle = "Street type",
      viv_fuente_agua = "Source of drinking water",
      viv_baldios_dist = "Proximity to vacant lots",
      viv_cunetas_zanjas_dist = "Proximity to roadside channels/ditches",
      viv_basura_acum_dist = "Proximity to dumpyards",
      viv_prox_rio_dist = "Proximity to water bodies",
      viv_suelo_cubierto = "Permeable house floor",
      viv_techo_impermeable = "Impermeable house roof",
      viv_acum_agua = "History of floodings",
      viv_acum_agua30d = "Floodings in the past 30 days",
      viv_acum_agua30d_donde = "Area reached for the flood (past 30 days)",
      viv_acum_agua30d_tiempo = "Time taken for the water to recede (past 30 days)",
      con_sint_ns = "Doesn't know",
      con_sint_fiebre = "Fever",
      con_sint_cefalea = "Headache",
      con_sint_mialgia = "Myalgia",
      con_sint_diarrea = "Diarrhea",
      con_sint_nau_vom = "Nausea/vomiting",
      con_sint_malestar = "Malaise",
      con_sint_cansancio = "Fatigue",
      con_sint_sarpullido = "Skin rash",
      con_sint_sangrado = "Hemorrhage",
      con_sint_gripal = "Influenza-like symptoms",
      con_sint_mareos = "Dizzyness",
      con_sint_dol_ojos = "Retro-orbital pain",
      con_trasm_ns = "Doesn't know",
      con_trasm_mosquito = "Mosquito bites",
      con_trasm_cacharros = "Water containers",
      con_trasm_viaje = "Travel"
    )
  )


# Crear dataset con observaciones completas ------------------------------
datos_glm <- datos |>
  # Seleccionar columnas relevantes
  select(
    dengue,
    barrio,
    genero:ocupacion_cat,
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


## Explorar datos
tabyl(datos, barrio) |>
  adorn_pct_formatting()


# Tabla 1: tipo vivienda x barrio ----------------------------------------
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


## Relación entre conocimientos y acumulación de cacharros ----
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


# Figura 2 ---------------------------------------------------------------
## Síntomas
sint <- datos |>
  select(barrio, contains("con_sint_")) |>

  # Base long
  pivot_longer(!barrio, names_to = "sintomas") |>

  # Modificar niveles síntomas
  mutate(
    sintomas = var_label(datos)[sintomas] |>
      unname() |>
      as.character()
  ) |>

  # Estimar frecuencias absolutas
  count(barrio, sintomas, wt = value == 1) |>

  # Frecuencias relativas por barrio
  mutate(
    freq = case_when(
      barrio == "CH" ~ n / nrow(datos |> filter(barrio == "CH")),
      barrio == "CS" ~ n / nrow(datos |> filter(barrio == "CS")),
      TRUE ~ n / nrow(datos |> filter(barrio == "VP"))
    )
  ) |>

  # Ordenar niveles
  mutate(
    sintomas = fct_infreq(sintomas) |>
      fct_relevel("Doesn't know", after = Inf)
  )


## Transmisión
trasm <- datos |>
  select(barrio, con_trasm_ns:con_trasm_viaje) |>

  # Base long
  pivot_longer(!barrio, names_to = "trasmision") |>

  # Modificar niveles síntomas
  mutate(
    trasmision = var_label(datos)[trasmision] |> unname() |> as.character()
  ) |>

  # Estimar frecuencias absolutas
  count(barrio, trasmision, wt = value == 1) |>

  # Frecuencias relativas por barrio
  mutate(
    freq = case_when(
      barrio == "CH" ~ n / nrow(datos |> filter(barrio == "CH")),
      barrio == "CS" ~ n / nrow(datos |> filter(barrio == "CS")),
      TRUE ~ n / nrow(datos |> filter(barrio == "VP"))
    )
  ) |>

  # Ordenar niveles
  mutate(
    trasmision = fct_infreq(trasmision) |>
      fct_relevel("Doesn't know", after = Inf)
  )


## Unir figuras
# Generar plot síntomas
sint |>
  ggplot(mapping = aes(x = sintomas, y = freq, fill = barrio)) +

  # Generar plot trasmisión
  trasm |>
    ggplot(mapping = aes(x = trasmision, y = freq, fill = barrio)) +

  plot_layout(
    guides = 'collect',
    axes = "collect_y",
    axis_titles = "collect_y"
  ) &

  plot_annotation(tag_levels = "A") &

  # Formato gráfico
  geom_col(
    position = position_dodge2(preserve = "single"),
    color = "grey50"
  ) &

  scale_fill_scico_d(palette = "lipari") &

  scale_y_continuous(name = "Frequency", limits = c(0, 1)) &

  labs(x = NULL, fill = "Site") &

  # Tema
  theme_minimal() &

  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, size = 10, face = "bold")
  )


# Guarda figura
ggsave("figure2.png", width = 16, height = 14, units = "cm")


# Actitudes ---------------------------------------------------------------
# Síntomas febriles vs acciones
tabyl(datos, act_sint6m_alguno, act_sint6m_acciones) |>
  adorn_totals() |>
  adorn_percentages() |>
  adorn_pct_formatting()

# Acciones vs problemas
tabyl(datos, act_sint6m_acciones, act_sint6m_inconvenientes) |>
  adorn_totals() |>
  adorn_percentages() |>
  adorn_pct_formatting()

# Problemas barrio
tabyl(datos$act_problemas_barrio)


# Figura 3 ---------------------------------------------------------------
fig3 <- datos |>
  #filter(conoce_dengue == 1) |>
  select(barrio, starts_with("act_mas")) |>

  # Base long
  pivot_longer(!barrio, names_to = "actitud") |>

  # Modifica niveles síntoma
  mutate(actitud = str_remove(actitud, "act_mas_")) |>

  # Estima frecuencias absolutas
  count(barrio, actitud, value) |>

  # Frecuencias relativas por barrio
  mutate(
    freq = case_when(
      barrio == "CH" ~ n / 66,
      barrio == "CS" ~ n / 79,
      TRUE ~ n / 36
    )
  ) |>

  # Edita etiquetas niveles
  mutate(
    actitud = fct_recode(
      actitud,
      "Prevalence" = "afecta",
      "Risk of infection" = "expuesto",
      "Fear of contagion" = "miedo",
      "Publicity" = "publicidad"
    ),
    value = fct_recode(
      value,
      "Equally" = "Igual",
      "DK/NA" = "Ninguna/no sabe"
    ) |>
      fct_reorder(-n) |>
      fct_relevel("DK/NA", after = Inf)
  ) |>

  ## Genera plot
  ggplot(mapping = aes(x = value, y = freq, fill = barrio)) +

  geom_col(position = position_dodge2(preserve = "single"), color = "grey50") +

  scale_fill_scico_d(palette = "lipari") +

  scale_y_continuous(name = "Frequency", limits = c(0, 1)) +

  labs(x = NULL, fill = "site") +

  # Facets
  facet_wrap(~actitud) +

  # Tema
  theme_minimal() +

  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, size = 10, face = "bold"),
    strip.text = element_text(size = 10, face = "bold")
  )


# Guarda figura
ggsave(fig3, filename = "figure3.png", width = 16, height = 14, units = "cm")

# Limpia environment
rm(fig3)


# Prácticas ---------------------------------------------------------------
## Tabla frecuencias
datos |>
  # Selecciona columnas
  select(barrio, starts_with("prac")) |>

  # Genera tabla
  tbl_summary(
    by = barrio,
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1))
  ) |>

  # Añade significancia
  add_overall() |>
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = T)) |>
  bold_p() |>
  bold_labels()


# Correlación entre KAPs --------------------------------------------------
## Tabla scores KAPS por barrio
datos |>
  select(barrio, starts_with("sc_")) |>

  tbl_summary(by = barrio, type = list(starts_with("sc_") ~ "continuous")) |>

  add_overall() |>

  add_p() |>

  bold_p()


## Correlograma de Kendall
datos |>
  select(starts_with("sc_")) |>
  ggcorr(method = c("pairwise", "kendall"), label = T, label_round = 3)


# Seropositividad ajustada ------------------------------------------------
## Calcula seroprevalencia ajustada
sprev <- tabyl(datos, barrio, ELISA_dengue) |>
  adorn_totals(where = "col") |>

  mutate(
    prev_adj = epi.prev(
      pos = POS,
      tested = Total,
      se = 0.96, # sensibilidad del test
      sp = 0.93, # especificidad del test
      units = 1
    )$tp,

    pos_adj = epi.prev(
      pos = POS,
      tested = Total,
      se = 0.96, # sensibilidad del test
      sp = 0.93, # especificidad del test
      units = 1
    )$true.positiv
  ) |>

  unnest_wider(col = c(prev_adj, pos_adj), names_sep = "_") |>

  mutate(neg_adj_est = Total - pos_adj_est)


# Tabla
sprev |>
  select(barrio, starts_with("prev"), starts_with("pos_"))


# seroprevalencia ajustada x barrio
tibble(
  barrio = c(rep("CH", 66), rep("CS", 79), rep("VP", 39)),
  dengue = c(
    rep(1, 10),
    rep(0, 56),
    rep(1, 3),
    rep(0, 76),
    rep(1, 3),
    rep(0, 36)
  )
) |>

  tbl_cross() |>

  add_p()


# Factores que afectan la seropositividad ---------------------------------
## Regresiones simples
datos_glm |>
  select(-barrio) |>

  mutate(con_sintomas = factor(con_sintomas)) |>

  tbl_uvregression(
    y = dengue,
    method = glm,
    # method = "glmmTMB",
    # formula = "{y} ~ {x} + (1 | barrio)",
    method.args = list(family = "binomial"),
    exponentiate = T
  ) |>

  bold_p() |>

  as_flex_table() |>

  save_as_docx(path = "tab.docx")


## Modelos full
# Barrio como intercepto aleatorio
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

# Barrio como intercepto aleatorio y efecto fijo
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

# Barrio como efecto fijo
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

# Compara modelos
compare_performance(full_glmm1, full_glm, metrics = "common", rank = F)


# Limpia environment
rm(full_glmm2, full_glm)

## Selección de variables a partir de full_glmm1
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
  rank = T
)

# Limpia environment
rm(list = setdiff(ls(), c("datos_glm", "glmm1")))


## Selección de variables a partir de glmm1
# (-) tipo de calle
glmm1.1 <- update(glmm1, ~ . - viv_tipo_calle)

# (-) baldíos
glmm1.2 <- update(glmm1, ~ . - viv_baldios_dist)

# (-) fuente de agua potable
glmm1.3 <- update(glmm1, ~ . - viv_fuente_agua)

# (-) síntomas febriles
glmm1.4 <- update(glmm1, ~ . - act_sint6m_alguno)

# Compara modelos
compare_performance(
  glmm1,
  glmm1.1,
  glmm1.2,
  glmm1.3,
  glmm1.4,
  metrics = "common",
  rank = T
)


# Limpia environment
rm(list = setdiff(ls(), c("datos_glm", "glmm1.1")))


## Selección de variables a partir de glmm1.1
# (-) baldíos
glmm1.1.1 <- update(glmm1.1, ~ . - viv_baldios_dist)

# (-) fuente de agua
glmm1.1.2 <- update(glmm1.1, ~ . - viv_fuente_agua)

# (-) síntomas febriles
glmm1.1.3 <- update(glmm1.1, ~ . - act_sint6m_alguno)

# Compara modelos
compare_performance(
  glmm1.1,
  glmm1.1.1,
  glmm1.1.2,
  glmm1.1.3,
  metrics = "common",
  rank = T
)

# Limpia environment
rm(list = setdiff(ls(), c("datos_glm", "glmm1.1.2")))


## Selección de variables a partir de glmm1.1.2
# (-) baldíos
glmm1.1.2a <- update(glmm1.1.2, ~ . - viv_baldios_dist)

# (-) tipo de calle
glmm1.1.2b <- update(glmm1.1.2, ~ . - act_sint6m_alguno)

# Compara modelos
compare_performance(
  glmm1.1.2,
  glmm1.1.2a,
  glmm1.1.2b,
  metrics = "common",
  rank = F
)

# Coeficientes modelo 1.2.1b
tbl_regression(glmm1.1.2b, exponentiate = T)

# Efecto aleatorio
ranef(glmm1.1.2b)

# R2
r2(glmm1.1.2b)

# Residuales
check_model(glmm1.1.2b)

check_residuals(glmm1.1.2b)
