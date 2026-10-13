
library(readr)
library(dplyr)
library(leaflet)
library(sf)
library(htmltools)
library(ggplot2)

## Lectura de base de codigos municipio -----

# Jerarquía Territorial del IBGE
# UF (Unidad Federal): Los 26 estados y el Distrito Federal. 
# Es el nivel político-administrativo más alto por debajo de la Unión.
# 
# Región Geográfica Intermedia: Reemplaza a las antiguas mesorregiones. 
# Organiza el territorio en función de metrópolis o centros regionales de gran relevancia, 
# sumando un total de 133 regiones en el país. 
# Sirve para la planificación de políticas públicas a largo plazo.
# 
# Región Geográfica Inmediata: Reemplaza a las antiguas microrregiones. 
# Se estructura en torno a centros urbanos cercanos para satisfacer las necesidades 
# inmediatas de la población, como compras, empleo, salud y 
# educación de complejidad media, sumando un total de 510 regiones.
# 
# Municipio: La unidad político-administrativa autónoma más pequeña del país, 
# con ayuntamiento y consejo municipal.

path <- "C:/Users/user/Desktop/Juan/4. Cursos externos/Maestria brasil/Tesis/Estudio ecologico/Bases brasil"

base_codigos_municipios <- readxl::read_excel(
  paste0(path,"./DTB_2023/DTB_2023/RELATORIO_DTB_BRASIL_MUNICIPIO.xls"), 
  skip = 5)
           
base_codigos_municipios <- base_codigos_municipios %>% 
  mutate(
    codigo_municipio_7 = `Código Município Completo`,
    codigo_municipio_6 = stringr::str_sub(`Código Município Completo`,1,6)
  )

## Lectura de base de mortalidad -----

base_brasil <- read_delim(paste0(path,"/Bases_mortalidad/base_CCU_10_24.csv"), 
                          delim = ",", escape_double = FALSE, trim_ws = TRUE)

#a <- count(bm_24,CODMUNRES)

### Cruce de base con codigos de municipios ----

base_brasil <- left_join(
  base_brasil %>% 
    mutate(CODMUNOCOR = as.character(CODMUNOCOR)),
  base_codigos_municipios %>% 
    select(codigo_municipio_6,Nome_Município,
           `Nome Região Geográfica Intermediária`,
           `Nome Região Geográfica Imediata`,
           Nome_UF,),
  by = join_by("CODMUNOCOR" == "codigo_municipio_6")
)

### Modificacion de variables ----



### Filtrar la base ----

base_brasil_limitrofe <- base_brasil %>% 
  
  filter(#### Codigos de cuello de utero ----
         stringr::str_sub(CAUSABAS,1,3) %in% c("C53","C54","C55"),
         
         #### UF ----
         Nome_UF %in% c("Paraná",
                        "Santa Catarina",
                        "Rio Grande do Sul")
         )

## Mapa -----

### Preparacion de base ----

# 1. Shapefile de regiones inmediatas de Brasil

rg_inmediatas <- st_read(paste0(path,"/BR_RG_Imediatas_2025/BR_RG_Imediatas_2025.shp"),
                         quiet = TRUE
                  #options = "ENCODING=WINDOWS-1252"
                  )

## 2. Filtrar Estados fronterizos

rg_inmediatas <- rg_inmediatas %>% 
  
  filter(NM_UF %in% c("Paraná",
                        "Santa Catarina",
                        "Rio Grande do Sul"))

# 3. Cargar tus tasas de mortalidad por rg inmediata

#install.packages("brpop")
#library(brpop)

# Población femenina total por municipio y año

# pob_fem <- mun_female_pop_totals(source = "datasus")
# 
# poblaciones_cod <- pob_fem %>% 
#   mutate(code_muni = as.character(code_muni)) %>% 
#   left_join(
#     base_codigos_municipios %>% 
#       select(codigo_municipio_6,Nome_Município,
#              `Nome Região Geográfica Intermediária`,
#              `Nome Região Geográfica Imediata`,
#              Nome_UF,),
#     by = join_by("code_muni" == "codigo_municipio_6")
#   ) %>% 
#   filter(#### UF 
#          Nome_UF %in% c("Paraná",
#                       "Santa Catarina",
#                       "Rio Grande do Sul"))
#          #year == 2021)
# 
# writexl::write_xlsx(poblaciones_cod,"./pob_fem_mun.xlsx")

poblaciones_cod <- readxl::read_xlsx(paste0(path,"/pob_fem_mun.xlsx")) %>% 
  filter(year == 2021)

poblaciones_cod <- poblaciones_cod %>% 
  group_by(`Nome Região Geográfica Imediata`) %>% 
  mutate(pop = sum(pop,na.rm = T)) %>% 
  ungroup() %>% 
  filter(!duplicated(`Nome Região Geográfica Imediata`))
  
tasas <- base_brasil_limitrofe %>% 
  # filter(provoc %in% c(18,54),
  #        depoc != 999) %>% 
  count(Nome_UF,`Nome Região Geográfica Imediata`) %>% 
  left_join(poblaciones_cod %>%
              select(Nome_UF,`Nome Região Geográfica Imediata`,pop),
            by = join_by("Nome Região Geográfica Imediata" == "Nome Região Geográfica Imediata",
                         "Nome_UF" == "Nome_UF")) %>%
  mutate(tasa_mortalidad = (n/pop)*10000,
         `Nome Região Geográfica Imediata` = if_else(
           `Nome Região Geográfica Imediata` == "Cornélio Procópio ¿ Bandeirantes",
           "Cornélio Procópio - Bandeirantes",`Nome Região Geográfica Imediata`))
         
  
# 4. Unir geometría con datos

mapa_data_brasil <- rg_inmediatas %>%
  left_join(tasas, 
            by = join_by("NM_RGI" == "Nome Região Geográfica Imediata",
                         "NM_UF" == "Nome_UF"))
                           