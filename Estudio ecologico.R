
library(sf)
library(dplyr)
library(ggplot2)

source("Brasil.R")
source("Argentina.R")

# 1. Confirmar y unificar el CRS (Argentina e IBGE pueden no coincidir)
st_crs(mapa_data_argentina) # tu objeto de Argentina (departamentos)
st_crs(mapa_data_brasil)    # tu objeto de Brasil (municipios)

# Si difieren, transformá uno al CRS del otro (SIRGAS 2000 / EPSG:4674 es un buen común denominador,
# ya que es el que usa el IBGE y cubre bien la zona)
mapa_data_argentina <- st_transform(mapa_data_argentina, crs = 4674)
mapa_data_brasil <- st_transform(mapa_data_brasil, crs = 4674)

# 2. Armonizar columnas antes de unir (mismo nombre, mismo tipo)
arg_armonizado <- mapa_data_argentina %>%
  transmute(
    unidad = nam, # departamento
    provincia = provincia,
    pais = "Argentina",
    nivel = "Departamento",
    n,
    tasa_mortalidad,
    geometry
  )

#### Aca cambiar si es municipio o reg inmediata -----

brasil_armonizado <- mapa_data_brasil %>%
  transmute(
    unidad = NM_RGI,         # aca ajustar si es municipio rgi
    provincia = NM_UF,
    pais = "Brasil",
    nivel = "Municipio",
    n,
    tasa_mortalidad,
    geometry
  )

# 3. Unir ambos sf en uno solo
mapa_combinado <- bind_rows(arg_armonizado, brasil_armonizado)

# 4. Graficar

## Mapa estatico ----

ggplot(mapa_combinado) +
  geom_sf(aes(fill = tasa_mortalidad), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "Tasa de mortalidad\n(por 100.000 mujeres)",
    option = "plasma",
    na.value = "grey90"
  ) +
  theme_void() +
  labs(
    title = "Mortalidad por cáncer de cuello uterino",
    subtitle = "Región fronteriza Argentina-Brasil"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

### Mapa dinámico -----

library(leaflet)
library(htmltools)

pal = colorNumeric(
  palette = "YlOrRd",
  domain = mapa_combinado$tasa_mortalidad,
  na.color = "#d9d9d9"
)


mapa_coropletico =
  
  leaflet(data = mapa_combinado %>% 
            mutate(provincia_nombre = case_when(
              provincia == "54" ~ "Misiones",
              provincia == "18" ~ "Corrientes",
              .default = provincia)),
          width = "100%",
          height = 600) %>%
  
  addTiles(group = c("StreetMap", "Tasa de mortalidad por CCU")) %>%
  
  setView(lng = -55.5, lat = -28.3, zoom = 7) %>%   # ajustar centro/zoom a Misiones+Corrientes
  
  addPolygons(
    fillColor = ~pal(tasa_mortalidad),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.8,
    group = "Tasa de mortalidad por CCU",
    highlightOptions = highlightOptions(
      weight = 3, color = "#666", dashArray = "", fillOpacity = 0.9, bringToFront = TRUE),
    label = ~lapply(
      paste0(
        "<strong>", unidad, "</strong> (", provincia_nombre, ")<br>",
        "Tasa: ", round(tasa_mortalidad, 1), " por 100.000 mujeres<br>",
        "Casos: ", n
      ),
      htmltools::HTML
    ),
    labelOptions = labelOptions(direction = "auto",
                                style = list("font-size" = "14px"))) |>
  
  addLegend(
    pal = pal,
    values = ~tasa_mortalidad,
    opacity = 0.7,
    title = "Tasa de mortalidad por CCU<br>(por 100.000 mujeres)",
    position = "bottomright",
    group = "Tasa de mortalidad por CCU") %>%
  
  addLayersControl(
    baseGroups = c("StreetMap"),
    overlayGroups = c("Tasa de mortalidad por CCU"),
    position = "topright")

# Mostrar el mapa
mapa_coropletico






