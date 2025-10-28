### Limpieza de datos para el manuscrito: Seropositivity to Dengue virus (DENV) in
### three neighborhoods in the periphery of a city with a recent history of
### outbreaks in Argentina: what can we learn from unreported infections?
### Last update:
# Mon Dec 23 11:27:13 2024 ------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  epikit,
  janitor,
  tidyverse
)


# Cargar datos crudos -----------------------------------------------------
data_raw <- import("raw/kap_dengue_raw.xlsx")


# Análisis exploratorio
names(data_raw)

summary(data_raw$Edad)


# Limpieza de datos -------------------------------------------------------
data_clean <- data_raw |>

  # Estandarizar nombres de columnas
  clean_names() |>
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


# Procesamiento datos -----------------------------------------------------
data_clean_kap <- data_clean |>

  # Categorizar edad
  mutate(
    edad_cat = age_categories(edad, breakers = c(18, 32, 44, 59)),
    .after = edad
  ) |>

  # Categorizar nivel educativo
  mutate(
    educacion_cat = fct_collapse(
      educacion,
      "Ninguno/primaria inc." = c("Ninguna", "Primaria incompleta"),
      "Primaria comp./secundaria inc." = c(
        "Primaria completa",
        "Secundaria incompleta"
      ),
      "Secundaria comp./superior" = c(
        "Secundaria completa",
        "Terciaria/Universitaria incompleta",
        "Terciaria/Universitaria completa"
      )
    ),
    .after = educacion
  ) |>

  # Categorizar ocupación
  mutate(
    ocupacion_cat = fct_collapse(
      ocupacion,
      "Jefe de hogar/estudiante" = c("Ama/o de casa", "Estudiante"),
      "Jubilado/pensionado" = "Jubilado/a o Pensionado/a",
      "Desocupado o subocupado" = c("Subocupado/a", "Desocupado/a", "Otro"),
      "Ocupado" = c(
        "Dependiente privado",
        "Dependiente público",
        "Independiente"
      )
    ),
    .after = ocupacion
  ) |>

  # Acumulación de agua los 30 días previos
  mutate(
    viv_acum_agua30d = if_else(
      grepl("[Dd]ic|[Úú]lt|mes$|sem|días|[Ss]áb|noche", viv_ult_inund) &
        !grepl("2015", viv_ult_inund),
      "Si",
      "No",
      missing = "No"
    ),
    .after = viv_acum_agua
  ) |>

  # Hasta donde llegó el agua
  mutate(
    viv_acum_agua30d_donde = if_else(
      viv_acum_agua30d == "No",
      NA,
      viv_donde_llego
    ),
    .after = viv_acum_agua30d
  ) |>

  # Duración del agua
  mutate(
    viv_acum_agua30d_tiempo = if_else(
      viv_acum_agua30d == "No",
      NA,
      viv_tiempo_agua
    ),
    .after = viv_acum_agua30d_donde
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
      TRUE ~ 0
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

  # Usa agua del río
  mutate(
    prac_no_usa_agua_rio = case_when(
      grepl(
        "[Bb]ido|[Bb]omba|[Ff]ilt|[Pp]ile|[Mm]ine|[Ll]luvia|Sauce",
        usa_para
      ) ~ 1,
      grepl("[Rr]ío|[Rr]eg|[Rr]efr|[Rr]ecr|const|lavar", usa_para) &
        !grepl("\\(", usa_para) ~ 0,
      TRUE ~ prac_no_usa_agua_rio
    )
  ) |>

  # Usa agua de lluvia
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
      "Si",
      "No"
    )
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
      act_sint6m_acciones == "Fue al médico" | act_mejoras_dengue == 1 ~
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
    .fns = ~ as.factor(.x)
  )) |>

  # Seleccionar variables
  select(
    id_encuesta:edad_cat,
    educacion_cat,
    ocupacion_cat:viv_tipo_calle,
    ends_with("_dist"),
    viv_suelo_cubierto:viv_acum_agua30d_tiempo,
    viv_fuente_agua,
    conoce_dengue:con_alguien_quienes,
    con_sintomas:sc_practicas,
    -ends_with("_bin"),
    -usa_para,
    -pal_dengue
  )


# Crear diccionario de datos ---------------------------------------------
data_dict <- tibble(
  variable = names(data_clean_kap),
  descripcion = NA,
  tipo = map_chr(data_clean_kap, class),
  niveles = ifelse(
    sapply(data_clean_kap, is.factor),
    sapply(data_clean_kap, levels) %>%
      str_remove_all("c\\(") %>%
      str_remove_all("\\)"),
    NA
  )
)

# Exportar datos limpios ---------------------------------------------------
## Dataset dengue
export(data_clean_kap, file = "clean/kap_dengue_clean.xlsx")

## Diccionario de datos
export(data_dict, file = "clean/data_dict_kap_dengue.xlsx")
