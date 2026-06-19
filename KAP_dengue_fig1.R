### Figure 1 for the manuscript: Unrecognized dengue transmission in socially
### vulnerable periurban neighborhoods of a temperate Argentine city: integrating
### serology with knowledge, attitudes and practices
### Author: Tamara Ricardo
# Última modificación: 19-06-2026 14:11

# Carga paquetes ----------------------------------------------------------
pacman::p_load(
  # Mapas
  sf,
  geoAr,
  tmap,
  tmaptools,

  # Manejo de datos
  rio,
  janitor,
  tidyverse
)


# Carga datos geográficos -------------------------------------------------
## Shapefile Argentina -----
shp_arg <- get_geo("ARGENTINA", level = "provincia") |>
  add_geo_codes() |>
  # Añadir columna para colorear Santa Fe
  mutate(prov_sf = if_else(name_iso == "Santa Fe", TRUE, FALSE))


## Shapefile Barrios Populares (RENABAP) -----
shp_renabap <- import(
  "raw/Registro Nacional de Barrios Populares 2023 - Santa Fe.rdata"
) |>
  # Estandarizar nombres de columnas
  clean_names() |>

  # Crear variable sigla barrios
  mutate(
    barrio = case_when(
      nombre_bar == "Chalet" ~ "CH",
      nombre_bar == "Colastine Sur" ~ "CS",
      str_detect(nombre_bar, "Paraguayo") ~ "VP",
      .default = ""
    )
  ) |>

  # Convertir a capa GIS
  st_as_sf(wkt = "wkt") |>

  # Añadir proyección
  st_set_crs(value = 4326)


## shapefile Índice Vulnerabilidad Sanitaria ----
shp_vs <- st_read(
  "raw/Vulnerabilidad sanitaria 2010-2018 - ud - Santa Fe.shp",
) |>
  # Estandarizar nombres de columnas
  clean_names() |>

  # Recortar
  st_crop(shp_renabap)


## Centros de salud -----
shp_salud <- st_read("raw/health_units.geojson", stringsAsFactors = T) |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  )


# Crear mapa -------------------------------------------------------------
## Minimapa Argentina
g1 <- tm_basemap(
  server = "CartoDB.PositronNoLabels",
  alpha = .75
) +
  # Capa Argentina
  tm_shape(shp = shp_arg) +
  tm_polygons(
    fill = "prov_sf",
    fill.scale = tm_scale_categorical(values = "-stevens.blue_fluoride"),
    fill_alpha = .8,
    fill.legend = tm_legend(show = FALSE)
  )

## Capa principal -----
g2 <- tm_basemap(
  server = "CartoDB.PositronNoLabels",
  alpha = .75
) +

  # Capa vulnerabilidad sanitaria
  tm_shape(shp = shp_vs) +
  tm_fill(
    fill = "vulnerabil",
    fill.scale = tm_scale_continuous(
      # values = "scico.tokyo",
      values = "stevens.blue_fluoride",
      label.format = tm_label_format(fun = function(x) scales::percent(x))
    ),
    fill.legend = tm_legend(
      title = "HVI",
      # frame = FALSE,
      # bg = FALSE,
      # orientation = "landscape",
      # width = 8,
      position = tm_pos_on_top(pos.h = "right"),
    ),
    fill_alpha = .9
  ) +

  # Capa hospitales
  tm_shape(shp = shp_salud) +
  tm_symbols(
    shape = "images/placeholder_429238.png",
    size = .3
  ) +

  # Marcadores barrios
  tm_shape(shp = shp_renabap |> filter_out(barrio == "")) +
  tm_polygons() +
  tm_markers(
    text = "barrio",
    fontface = "bold",
    bgcol = "white",
    bgcol_alpha = .8,
    size = .75,
    options = opt_tm_markers(dots.icon.scale = 1.5)
  ) +

  # Escala y flecha del Norte
  tm_compass() +
  tm_scalebar()


## Guardar gráfico -----
fig1 <- tmap_arrange(g1, g2, widths = c(.35, 1), asp = NA)

tmap_save(fig1, filename = "figure1.png", width = 16, height = 10, units = "cm")


# Clean working environment ----------------------------------------------
rm(list = ls())
