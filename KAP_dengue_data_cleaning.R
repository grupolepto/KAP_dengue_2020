### Limpieza de datos para el manuscrito: Seropositivity to Dengue virus (DENV) in
### three neighborhoods in the periphery of a city with a recent history of
### outbreaks in Argentina: what can we learn from unreported infections?
### Fecha modificación: # 2025-10-30 12:46:08

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  epikit,
  janitor,
  labelled,
  tidyverse
)


# Cargar datos crudos -----------------------------------------------------
datos_raw <- import("raw/kap_dengue_raw.xlsx")


# Análisis exploratorio
names(datos_raw)

summary(datos_raw$Edad)


# Limpieza de datos -------------------------------------------------------
datos_clean <- datos_raw |>

  # Estandarizar nombres de columnas
  clean_names() |>

  # Renombrar columnas manualmente
  rename(
    id_encuesta = id,
    fecha_encuesta = fecha,
    ELISA_dengue = dengue,
    prox_rio_dist = rio_dist,
    act_sint6m_acciones = que_hizo,
    act_sint6m_inconvenientes = inconvenientes,
    act_problemas_barrio = problemas,
    act_mejoras_barrio = cambio_fav,
    act_mejoras_barrio_desc = cambio_cual,
    prac_no_cacharros = cacharros,
    prac_no_usa_agua_rio = usa_agua_rio,
    prac_resid_camion = resid_camion
  ) |>

  # Renombrar columnas vivienda
  rename_with(.cols = c(tipo_calle:fuente_agua), .fn = ~ paste0("viv_", .x)) |>

  # Renombrar columnas conocimientos
  rename_with(
    .cols = contains("alguien_"),
    .fn = ~ str_replace(.x, "conoce_alguien_dengue", "con_alguien")
  ) |>

  rename_with(
    .cols = starts_with("s_den_"),
    .fn = ~ str_replace(.x, "s_den_", "con_sint_")
  ) |>

  rename_with(
    .cols = starts_with("c_den_"),
    .fn = ~ str_replace(.x, "c_den_", "con_trasm_")
  ) |>

  # Renombrar columnas actitudes
  rename_with(
    .cols = starts_with("mas_"),
    .fn = ~ str_replace(.x, "mas_", "act_mas_")
  ) |>

  rename_with(
    .cols = starts_with("sint_"),
    .fn = ~ str_replace(.x, "sint_", "act_sint6m_")
  ) |>

  # Renombrar columnas prácticas
  rename_with(
    .cols = c(resid_quema:resid_rio),
    .fn = ~ str_replace(.x, "resid_", "prac_resid_no_")
  ) |>

  # Filtrar registros del mismo domicilio
  filter(!id_encuesta %in% c("CS214", "CS175", "CH032")) |>

  # Cambiar formato fecha de encuesta
  mutate(fecha_encuesta = convert_to_date(fecha_encuesta)) |>

  # Recategorizar tipo de calle
  mutate(
    viv_tipo_calle = fct_collapse(
      viv_tipo_calle,
      "Pavimento/mejorado" = c("Pavimento", "Mejorado")
    )
  ) |>

  # Recategorizar distancia a fuentes ambientales
  mutate(across(
    .cols = contains("dist"),
    .fns = ~ case_when(
      .x %in% c("Menos de 10m", "Entre 10 y 25m") ~ "Menos de 25m",
      is.na(.x) ~ "Más de 50m",
      TRUE ~ .x
    )
  )) |>

  # Recategorizar fuentes de agua potable
  mutate(
    viv_fuente_agua = fct_collapse(
      viv_fuente_agua,
      "Camión aguatero/otra" = c("Camión aguatero", "Bomba/Pozo")
    )
  ) |>

  # Recategorizar conoce alguien con dengue
  mutate(across(
    .cols = starts_with("con") &
      !matches("con_alguien_quienes") &
      !matches("con_alguien_comentarios"),
    .fns = ~ if_else(.x == "Si", 1, 0)
  )) |>

  # Recategorizar niveles actitudes
  mutate(across(
    .cols = c(
      act_sint6m_cefalea:act_sint6m_fiebre,
      act_sint6m_inconvenientes,
      act_mejoras_barrio,
      prac_resid_camion
    ),
    .fns = ~ if_else(.x == "Si", 1, 0)
  )) |>

  # Recategorizar niveles prácticas
  mutate(across(
    .cols = c(
      prac_no_cacharros,
      prac_no_usa_agua_rio,
      prac_resid_no_quema:prac_resid_no_rio
    ),
    .fns = ~ if_else(.x == "Si", 0, 1)
  )) |>

  # Recategorizar NAs conocimientos
  mutate(across(
    .cols = starts_with("con_") &
      !matches("con_alguien_quienes") &
      !matches("con_alguien_comentarios"),
    .fns = ~ case_when(
      conoce_dengue == 0 ~ NA,
      conoce_dengue == 1 & is.na(.x) ~ 0,
      TRUE ~ .x
    )
  )) |>

  # Recategorizar NAs actitudes
  mutate(across(
    .cols = starts_with("act_mas"),
    .fns = ~ case_when(
      conoce_dengue == 1 & is.na(.x) ~ "Ninguna/no sabe",
      .x %in% c("Ninguna", "No sé") ~ "Ninguna/no sabe",
      conoce_dengue == 0 ~ NA,
      TRUE ~ .x
    )
  )) |>

  # Recategorizar niveles qué hizo síntomas febriles
  mutate(
    act_sint6m_acciones = fct_other(
      act_sint6m_acciones,
      keep = c("No hizo nada", "Fue al médico", "Tomó medicación"),
      other_level = "Otro/s"
    )
  )


# Dataset para el análisis -----------------------------------------------
datos_kap <- datos_clean |>
  # Recategorizar sexo
  mutate(genero = fct_relabel(genero, ~ c("Female", "Male"))) |>

  # Crear variable para grupo etario
  mutate(
    grupo_edad = age_categories(edad, breakers = c(18, 32, 44, 59)),
    .after = edad
  ) |>

  # Recategorizar nivel educativo
  mutate(
    educacion = fct_collapse(
      educacion,
      "None/incomplete primary school" = c("Ninguna", "Primaria incompleta"),
      "Primary school" = c(
        "Primaria completa",
        "Secundaria incompleta"
      ),
      "High school/university" = c(
        "Secundaria completa",
        "Terciaria/Universitaria incompleta",
        "Terciaria/Universitaria completa"
      )
    )
  ) |>

  # Categorizar ocupación
  mutate(
    ocupacion = fct_collapse(
      ocupacion,
      "Homemaker/student" = c("Ama/o de casa", "Estudiante"),
      "Retired/pensioner" = "Jubilado/a o Pensionado/a",
      "Unemployed/underemployed" = c("Subocupado/a", "Desocupado/a", "Otro"),
      "Employed" = c(
        "Dependiente privado",
        "Dependiente público",
        "Independiente"
      )
    )
  ) |>

  # Recategorizar barrio
  mutate(
    barrio = case_when(
      barrio == "Chalet" ~ "CH",
      barrio == "Colastiné Sur" ~ "CS",
      TRUE ~ "VP"
    )
  ) |>

  # Recategorizar tipo de calle
  mutate(
    viv_tipo_calle = fct_relabel(
      viv_tipo_calle,
      ~ c("Sand", "Paved/semi-paved", "Dirt")
    ) |>
      fct_relevel("Sand", after = Inf)
  ) |>

  # Recategorizar fuente agua potable
  mutate(
    viv_fuente_agua = fct_relabel(
      viv_fuente_agua,
      ~ c("Tank truck/other", "Water pipes")
    ) |>
      fct_rev()
  ) |>

  # Acumulación de agua los 30 días previos
  mutate(
    viv_acum_agua30d = if_else(
      grepl("[Dd]ic|[Úú]lt|mes$|sem|días|[Ss]áb|noche", viv_ult_inund) &
        !grepl("2015", viv_ult_inund),
      1,
      0,
      missing = 0
    ),
    .after = viv_acum_agua
  ) |>

  # Crear variable hasta donde llegó el agua
  mutate(
    viv_acum_agua30d_donde = case_when(
      viv_acum_agua30d == "No" ~ NA,
      viv_donde_llego == "Calle" ~ "Street",
      viv_donde_llego == "Casa" ~ "House",
      viv_donde_llego == "Frente/patio" ~ "Front or backyard"
    ),
    .after = viv_acum_agua30d
  ) |>

  # Crear variable tiempo inundación
  mutate(
    viv_acum_agua30d_tiempo = case_when(
      viv_acum_agua30d == 0 ~ NA,
      viv_tiempo_agua == "Días" ~ "Days",
      viv_tiempo_agua == "Horas" ~ "Hours",
      viv_tiempo_agua == "Semanas" ~ "Weeks",
      .default = NA
    ),
    .after = viv_acum_agua30d_donde
  ) |>

  # # Recategorizar variables Sí/No
  mutate(across(
    .cols = c(
      viv_cunetas_zanjas,
      viv_basura_acum,
      viv_baldios,
      viv_prox_rio,
      viv_suelo_cubierto,
      viv_techo_impermeable,
      viv_acum_agua
    ),
    .fns = ~ if_else(.x == "Si", 1, 0)
  )) |>

  # Recategorizar distancia a fuentes ambientales
  mutate(across(
    .cols = contains("dist"),
    .fns = ~ if_else(.x == "Más de 50m", "Over 50m", "Less than 50m")
  )) |>

  # Recategorizar presencia de  cacharros
  mutate(
    prac_cacharros = if_else(prac_no_cacharros == 1, 0, 1),
    .after = prac_no_cacharros
  ) |>

  ## Operaciones por fila
  rowwise() |>

  # Conoce algún síntoma de dengue
  mutate(
    con_sintomas = case_when(
      conoce_dengue == 0 ~ NA,
      con_sint_fiebre == 1 &
        sum(c(
          con_sint_cefalea,
          con_sint_mialgia,
          con_sint_nau_vom,
          con_sint_diarrea,
          con_sint_sarpullido,
          con_sint_dol_ojos,
          con_sint_sangrado
        )) >
          0 ~ 2,
      con_sint_fiebre == 1 &
        sum(c(
          con_sint_cefalea,
          con_sint_mialgia,
          con_sint_nau_vom,
          con_sint_diarrea,
          con_sint_sarpullido,
          con_sint_dol_ojos,
          con_sint_sangrado
        )) ==
          0 ~ 1,
      .default = 0
    ),
    .before = con_sint_ns
  ) |>

  # Conoce modos de transmisión
  mutate(
    con_trasmision = if_else(
      con_trasm_mosquito == 1 | con_trasm_cacharros == 1 | con_trasm_viaje == 1,
      1,
      0
    ),
    .before = con_trasm_ns
  ) |>

  # Modificar trasmisión no sabe
  mutate(con_trasm_ns = if_else(con_trasmision == 0, 1, con_trasm_ns)) |>

  # Cambiar niveles variables actitudes
  mutate(across(
    .cols = starts_with("act_mas"),
    .fns = ~ case_when(
      .x == "Igual" ~ "Both",
      .x == "Ninguna/no sabe" ~ "None/not sure",
      .default = .x
    )
  )) |>

  # Cambiar niveles acciones
  mutate(
    act_sint6m_acciones = fct_relabel(
      act_sint6m_acciones,
      ~ c("Sought medical care", "Nothing", "Self-medicated", "Other")
    ) |>
      fct_relevel("Self-medicated", after = 1)
  ) |>

  # Categorizar problemas en el barrio
  mutate(
    act_problemas_barrio_cat = if_else(is.na(act_problemas_barrio), 0, 1),
    .after = act_problemas_barrio
  ) |>

  # Algún síntoma febril en los 6 meses previos
  mutate(
    act_sint6m_alguno = if_else(
      any(c_across(act_sint6m_cefalea:act_sint6m_fiebre) == 1),
      1,
      0
    ),
    .before = act_sint6m_cefalea
  ) |>

  # Crear variable para problema dengue
  mutate(
    act_problema_dengue = if_else(
      grepl("Deng|Mosq|Fumi", act_problemas_barrio),
      1,
      0
    ),
    .after = act_problemas_barrio
  ) |>

  # Crear variable para mejoras respecto a dengue
  mutate(
    act_mejoras_dengue = if_else(
      grepl("Deng|Mosq|Fumi", act_mejoras_barrio_desc),
      1,
      0
    ),
    .after = act_mejoras_barrio_desc
  ) |>

  # Validar usa agua del río
  mutate(
    prac_no_usa_agua_rio = case_when(
      grepl(
        "[Bb]ido|[Bb]omba|[Ff]ilt|[Pp]ile|[Mm]ine|[Ll]luvia|Sauce",
        usa_para
      ) ~ 1,
      grepl("[Rr]ío|[Rr]eg|[Rr]efr|[Rr]ecr|const|lavar", usa_para) &
        !grepl("\\(", usa_para) ~ 0,
      .default = prac_no_usa_agua_rio
    )
  ) |>

  # Crear variable usa agua de lluvia
  mutate(
    prac_no_agua_lluvia = if_else(
      grepl("[Ll]uvia", usa_para),
      0,
      1
    ),
    .after = prac_no_usa_agua_rio
  ) |>

  # Dengue previo
  mutate(
    dengue_previo = if_else(
      grepl("[Ee]ncuest", con_alguien_quienes),
      1,
      0
    )
  ) |>

  # Variable respuesta binomial
  mutate(dengue = if_else(ELISA_dengue == "POS", 1, 0)) |>

  # Score de conocimientos
  mutate(
    sc_conocimientos = case_when(
      conoce_dengue == 0 ~ 0,
      conoce_dengue == 1 ~ sum(
        c(conoce_dengue, con_alguien, con_sintomas, con_trasmision),
        na.rm = T
      )
    )
  ) |>

  # Convertir a binomial actitudes vs leptospirosis
  mutate(
    across(
      .cols = starts_with("act_mas"),
      .fns = ~ case_when(
        .x %in% c("Dengue", "Igual") ~ 1,
        is.na(.x) ~ NA,
        TRUE ~ 0
      ),
      .names = "{.col}_bin"
    ),
    .after = act_problema_dengue
  ) |>

  # Score de actitudes
  mutate(
    sc_actitudes = case_when(
      str_detect(act_sint6m_acciones, "Medical") | act_mejoras_dengue == 1 ~
        sum(c_across(act_problema_dengue:act_mas_publicidad_bin), na.rm = T) +
        1,
      TRUE ~ sum(
        c_across(act_problema_dengue:act_mas_publicidad_bin),
        na.rm = T
      )
    )
  ) |>

  # Score de prácticas
  mutate(
    sc_practicas = sum(
      c(
        prac_resid_camion,
        prac_no_cacharros:prac_no_agua_lluvia,
        prac_resid_no_fondo
      ),
      na.rm = T
    )
  ) |>

  # Variables caracter a factor
  mutate(across(
    .cols = where(is.character) & !contains("id"),
    .fns = ~ factor(.x)
  )) |>

  # Seleccionar variables
  select(
    id_encuesta,
    genero:viv_prox_rio_dist,
    viv_fuente_agua,
    viv_suelo_cubierto:viv_acum_agua30d_tiempo,
    conoce_dengue,
    con_alguien:con_alguien_hogar,
    con_sintomas:con_trasm_personas,
    act_mas_miedo:act_sint6m_acciones,
    act_problemas_barrio_cat,
    act_problema_dengue,
    act_mejoras_barrio,
    act_mejoras_dengue:prac_no_agua_lluvia,
    prac_resid_camion:prac_resid_no_rio,
    starts_with("sc_"),
    dengue_previo,
    dengue,
    ELISA_dengue
  ) |>

  # Asignar etiquetas variables
  set_variable_labels(
    .labels = list(
      genero = "Sex",
      edad = "Age (years)",
      grupo_edad = "Age group",
      educacion = "Education",
      ocupacion = "Occupation",
      barrio = "Neighborhood",
      viv_tipo_calle = "Street type",
      viv_cunetas_zanjas = "Roadside channels/ditches",
      viv_cunetas_zanjas_dist = "Proximity to roadside channels/ditches",
      viv_basura_acum = "Dumpyards",
      viv_basura_acum_dist = "Proximity to dumpyards",
      viv_baldios = "Vacant lots",
      viv_baldios_dist = "Proximity to vacant lots",
      viv_prox_rio = "Water bodies",
      viv_prox_rio_dist = "Proximity to water bodies",
      viv_fuente_agua = "Source of drinking water",
      viv_suelo_cubierto = "Permeable house floor",
      viv_techo_impermeable = "Impermeable house roof",
      viv_acum_agua = "History of floodings",
      viv_acum_agua30d = "Floodings in the past 30 days",
      viv_acum_agua30d_donde = "Area reached for the flood (past 30 days)",
      viv_acum_agua30d_tiempo = "Time taken for the water to recede (past 30 days)",
      conoce_dengue = "Is aware of dengue",
      con_alguien = "Knows someone with dengue",
      con_alguien_barrio = "Someone from neighborhood",
      con_alguien_hogar = "Someone from household",
      dengue_previo = "Had dengue themself",
      con_sintomas = "Knows any symptom",
      con_sint_ns = "None",
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
      con_trasmision = "Any way of trasmission",
      con_trasm_ns = "Doesn't know any way of transmission",
      con_trasm_mosquito = "Mosquito bites",
      con_trasm_cacharros = "Water containers",
      con_trasm_viaje = "Travel to endemic areas",
      con_trasm_personas = "Contact with infected people",
      act_mas_miedo = "Most fear of contagion",
      act_mas_expuesto = "More risk of infection",
      act_mas_afecta = "Higher prevalence",
      act_mas_publicidad = "More publicity",
      act_sint6m_alguno = "Any febrile illness symptom",
      act_sint6m_fiebre = "Fever",
      act_sint6m_cefalea = "Headache",
      act_sint6m_mialgia = "Myalgia",
      act_sint6m_vom_diarrea = "Diarrhea/vomiting",
      act_sint6m_sarpullido = "Skin rash",
      act_sint6m_malestar = "Malaise",
      act_sint6m_acciones = "Action taken",
      act_problemas_barrio_cat = "Issues in the neighborhood",
      act_problema_dengue = "Dengue as an issue in the neighborhood",
      act_mejoras_barrio = "Neighborhood improvements",
      prac_no_cacharros = "Storing water containers",
      prac_no_usa_agua_rio = "Using water from rivers/lagoons",
      prac_no_agua_lluvia = "Using rainwater",
      prac_resid_camion = "Disposes in the garbage truck",
      prac_resid_no_quema = "Avoids burning",
      prac_resid_no_fondo = "Avoids storing in the backyard",
      prac_resid_no_rio = "Avoids throwing into water bodies",
      sc_conocimientos = "Knowledge score",
      sc_actitudes = "Attitudes score",
      sc_practicas = "Practices score"
    )
  )


# Exportar datos limpios ---------------------------------------------------
## Dataset dengue
export(datos_clean, file = "clean/kap_dengue_clean.xlsx")

## Dataset para análisis
export(datos_kap, file = "clean/kap_dengue.rds")
