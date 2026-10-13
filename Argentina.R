
require(dplyr)
require(ggplot2)

path_arg <- "C:/Users/user/Desktop/Juan/4. Cursos externos/Maestria brasil/Tesis/Estudio ecologico/Bases argentina"

base_argentina <- readr::read_csv(paste0(path_arg,"/Ca_CU.csv"))

codigos <- readxl::read_excel(paste0(path_arg,"/codigos_prov_depto_censo2010.xls"),
                      sheet = "Departamentos", 
                      skip = 2) %>% 
  filter(!is.na(`Código provincia`)) %>% 
  mutate(cod_prov = as.numeric(`Código provincia`),
         cod_dep = as.numeric(`Código departamento`),
         cod_dep = as.numeric(paste0(cod_prov,cod_dep))
         )

base_argentina <- base_argentina %>% 
  mutate(
    depoc = case_when(
      depoc == 999 ~ 999,
      .default = as.numeric(paste0(provoc,depoc))
    ),
    
    depres = case_when(
      depres == 999 ~ 999,
      .default = as.numeric(paste0(provres,depres))
    )
    
  ) %>% 
  # Departamento de ocurrencia
  left_join(codigos %>% 
              mutate(Departamento = `Departamento/ partido/ comuna`) %>% 
              select(cod_dep,Departamento) %>% 
              rbind(c(999,"Sin dato")) %>% 
              mutate(cod_dep = as.numeric(cod_dep)) %>% 
              filter(!duplicated(cod_dep)),
            by = join_by("depoc" == "cod_dep")
  ) %>% 
  mutate(depoc_nombre = Departamento,
         Departamento = NULL) %>% 
  
  # Departamento de residencia
  left_join(codigos %>% 
              mutate(Departamento = `Departamento/ partido/ comuna`) %>% 
              select(cod_dep,Departamento) %>% 
              rbind(c(999,"Sin dato")) %>% 
              mutate(cod_dep = as.numeric(cod_dep)) %>% 
              filter(!duplicated(cod_dep)),
            by = join_by("depres" == "cod_dep")) %>% 
  mutate(depres_nombre = Departamento,
         Departamento = NULL)

base_argentina <- base_argentina %>% 
  # Provincia de ocurrencia
  left_join(codigos %>% 
              filter(!duplicated(cod_prov)) %>% 
              select(cod_prov,Provincia) %>% 
              rbind(c(98,"Sin dato"),
                    c(99,"Sin dato")) %>% 
              mutate(cod_prov = as.numeric(cod_prov)),
            by = join_by("provoc" == "cod_prov")) %>% 
  mutate(provoc_nombre = Provincia,
         Provincia = NULL) %>% 
  # Provincia de residencia
  left_join(codigos %>% 
              filter(!duplicated(cod_prov)) %>% 
              select(cod_prov,Provincia) %>% 
              rbind(c(98,"Sin dato"),
                    c(99,"Sin dato")) %>% 
              mutate(cod_prov = as.numeric(cod_prov)),
            by = join_by("provres" == "cod_prov")) %>% 
  mutate(provres_nombre = Provincia,
         Provincia = NULL) 


## Mapa -----

### Preparacion de base ----

# 1. Shapefile de departamentos de Argentina
# Fuentes recomendadas:
#   - IGN: https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG (capa "departamento")
#   - INDEC (Códigos Geográficos): https://www.indec.gob.ar/indec/web/Institucional-Indec-Codgeo
#   - Paquete geoAr (envuelve capas oficiales): install.packages("geoAr")

deptos <- st_read(paste0(path_arg,"/departamento/departamentoPolygon.shp"),
                  options = "ENCODING=WINDOWS-1252",
                  quiet = TRUE)

## 2. Filtrar Misiones y Corrientes

deptos_ne <- deptos %>%
  mutate(provincia = stringr::str_sub(in1,1,2),
         departamento = stringr::str_sub(in1,3,5),
         codigo = as.numeric(paste0(as.numeric(provincia),
                                    as.numeric(departamento))
                             )
         ) %>% 
  filter(provincia %in% c("18","54"))
  
# 3. Cargar tus tasas de mortalidad por departamento

poblaciones <- read_delim(paste0(path_arg,"/base_estimaciones_pob_deptos_2022_2035.csv"), 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(Sexo = case_when(Sexo == 1 ~ "F",
                          Sexo == 2 ~ "M",
                          .default = as.character(Sexo))) %>% 
  filter(Sexo == "F",
         `Nombre jurisdicción` %in% c("Misiones","Corrientes"),
         Fecha == 2024) %>% 
  mutate(provincia = stringr::str_sub(`Código departamento`,1,2),
         departamento = stringr::str_sub(`Código departamento`,3,5),
         codigo = as.numeric(paste0(as.numeric(provincia),
                                    as.numeric(departamento))))


tasas_argentina <- base_argentina %>% 
  filter(provoc %in% c(18,54),
         depoc != 999) %>% 
  
  #filter(anio_def == 2024) %>% 
  
  count(depoc)

tasas_argentina <- tasas_argentina %>% 
  left_join(poblaciones %>% 
              select(codigo,Población),
            by = c("depoc" = "codigo")) %>% 
  mutate(tasa_mortalidad = (n/Población)*10000)

# 4. Unir geometría con datos
mapa_data_argentina <- deptos_ne %>%
  left_join(tasas_argentina, 
            by = c("codigo" = "depoc"))










