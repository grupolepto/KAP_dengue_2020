### Mapa Figura 1: Seropositivity to Dengue virus DENV in 
### three neighborhoods in the periphery of a city with a recent history of 
### outbreaks in Argentina: what can we learn from unreported infections?
### Author: Tamara Ricardo
### Last update:
# Tue Dec 10 12:56:00 2024 ------------------------------


# Carga paquetes ----------------------------------------------------------
# remotes::install_github("https://github.com/Chrisjb/basemapR")

pacman::p_load(
  # Mapas
  sf, 
  basemapR,
  ggspatial,
  ggimage, 
  
  # Herramientas gráficas
  scico, 
  patchwork,
  
  # Manejo de datos
  janitor,
  tidyverse
)



# Carga datos geográficos -------------------------------------------------
## shapefile Argentina
argentina <- st_read("../GIS/shp/ign_provincia.json") |> 
  
  # valida geometría
  st_make_valid() |> 
  
  # recorta Antártida
  st_crop(y = st_bbox(c(xmin = -74, xmax = -50,
                        ymin = -60, ymax = -22))) |> 
  
  # define datos a resaltar
  mutate(col_sf = if_else(nam == "Santa Fe", "si", "no"))


## shapefile Barrios Populares (RENABAP)
renabap <- st_read("gis/Registro Nacional de Barrios Populares - Santa Fe.shp") |> 
  clean_names() |> 
  
  # Añade columna con nombre de barrios muestreados
  mutate(barrio = case_when(
    id_renabap == 3014 ~ "CH",
    id_renabap == 3027 ~ "CS",
    id_renabap == 3040 ~ "VP",
    .default = NA
  ), .after = id_renabap) |> 
  
  # Categoriza el número de familias
  mutate(familias_cat = case_when(
    familias < 25 ~ "< 25",
    familias %in% c(25:249) ~ "25 to 250",
    familias %in% c(250:999) ~ "250 to 1000",
    familias %in% c(1000:249) ~ "1000 to 2500",
    familias >= 2500 ~ "> 2500") |> 
      fct_relevel("< 25", "25 to 250", "250 to 1000", "1000 to 2500", after = 0),
    .after = familias)

  
## shapefile Índice Vulnerabilidad Sanitaria
v_sanitaria <- st_read("gis/Vulnerabilidad sanitaria 2010-2018 - ud - Santa Fe.shp") |> 
  clean_names()


## Centros de salud
u_salud <- st_read("gis/health_units.geojson", stringsAsFactors = T) |> 
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2])



# Mapa de Argentina -------------------------------------------------------
map_arg <- ggplot() +
  # Mapa base
  base_map(bbox = st_bbox(argentina) |> 
             expand_bbox(Y = 100000, X = 100000),
           basemap = "voyager",
           increase_zoom = 0,
           nolabels = T) +
  
  # Resalta provincia de Santa Fe
  geom_sf(data = argentina, 
          mapping = aes(fill = col_sf)) +
  
    scale_fill_scico_d(palette = "buda", direction = -1, alpha = .75) +
  
  # Tema
  theme_void() +
  theme(legend.position = "none")



# Barrios populares/área de estudio ---------------------------------------
map_sf1 <- ggplot() +
  # Mapa base
  base_map(bbox = st_bbox(v_sanitaria),
           basemap = "positron",
           increase_zoom = 1,
           nolabels = T) +
  
  # Santa Fe
  geom_sf(data = v_sanitaria,
          fill = NA,
          color = "grey60") +
  
  # Barrios populares
  geom_sf(data = renabap, 
          mapping = aes(fill = familias_cat)) +
  
  scale_fill_scico_d(palette = "tokyo", direction = -1, alpha = .8) +
  
  # Barrios muestreados
  geom_sf(data = st_buffer(renabap |> filter(!is.na(barrio)), 
                           dist = c(300, 400, 300)),
          color = "#B200B2",
          lwd = .8,
          fill = NA) +
  
  geom_sf_label(data = renabap |> filter(!is.na(barrio)),
                mapping = aes(label = barrio),
                size = 3,
                alpha = .75,
                nudge_y = -.01) +
  
  # Etiquetas
  labs(x = NULL, y = NULL, fill = "Families")
# +
#   
#   # Tema
#   theme(axis.text.x = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks.x = element_blank())



# Vulnerabilidad sanitaria ------------------------------------------------
map_sf2 <- ggplot() +
  # Mapa base
  base_map(bbox = st_bbox(v_sanitaria),
           basemap = "positron",
           increase_zoom = 1,
           nolabels = T) +
  
  # Vulnerabilidad sanitaria
  geom_sf(data = v_sanitaria, 
          mapping = aes(fill = vulnerabil),
          color = "grey60") +
  
  scale_fill_scico(palette = "imola", direction = -1, alpha = .75) +
  
  scale_color_scico_d(palette = "buda", direction = -1, alpha = .75) +
  
  # Centros de salud/hospitales
  geom_image(data = u_salud, 
             mapping = aes(x = x, 
                           y = y, 
                           image = "images/placeholder_429238.png")) +
  
  # Barrios muestreados
  geom_sf(data = st_buffer(renabap |> filter(!is.na(barrio)), 
                           dist = c(300, 400, 300)),
          color = "#B200B2",
          lwd = .8,
          fill = NA) +
  
  # Etiquetas
  labs(x = NULL, y = NULL, fill = "HVI (%)")
  

### Ordena mapas
(map_sf1 +
    
    inset_element(map_arg, 
                  left = .55, 
                  bottom = .55, 
                  right = 1.3, 
                  top = 1,
                  align_to = "plot",
                  ignore_tag = T)) +
  
  map_sf2 +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr",
                         which_north = "true",
                         height = unit(.75, "cm"),
                         width = unit(.75, "cm")) +
  
  plot_layout(axes = "collect_x",
              guides = "collect",
              ncol = 1) +
  
  plot_annotation(tag_levels = "A")


# Exporta mapa
ggsave(filename = "figure1.png", 
       width = 18, 
       height = 18, 
       units = "cm", dpi = 400)
