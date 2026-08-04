# Título: IHEH en la altillanura
#
# Autor(es): Alejandra Narváez Vallejo
# Descripción: análisis multitemporal de la huella espacial humana por municipios /departamento o bioma
##
# Por hacer o  corregir:

## Clasificar la huella por susus rangos . Verificar los rangos de la reclasiisifcacion, se incluye el mayor o el menor
## quedé en los gráficos, revisar dimensiones
## falta hacer el readme

# Por hacer o  corregir: Observaciones

## El análisis aun mantiene el sistemas de referencia antiguo


#*******************************************************************************
# librerías o dependencias -----------------------------------------------------
#*******************************************************************************

## lectura de datos  ####

library (sf)
library(terra)
library(dplyr)
library(ggplot2)
#library(formattable)
library(readr)
#library(tidyr)
library(DT)
#library(alluvial)
library(ggalluvial)
library(htmlwidgets)


#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(), "..", ".."))

dir_Datos_Or <- file.path("Datos/Originales")
dir_Datos_Intm<- file.path ("Datos/Intermedios")
dir_Resultados <- file.path("Resultados")

#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(file.path(this.path::this.path(), "..", "..","..","..", "Funciones_comunes" , "estadísticas.R"))
source(file.path(this.path::this.path(), "..", "..","..","..", "Funciones_comunes" , "preprocesamiento.R"))


#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

Años <- c("2018", "2020", "2022")
Años_numero <- 3

Huella_cat <- data.frame(Cat = 1:5,
                         nom = c("Natural", "Bajo", "Medio", "Alto", "Muy Alto"))

Unidad_analsis <- "Municipio" # Departamento" Municipio, Bioma


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas raster

capas_raster <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "\\d{5}\\.tif$",
  full.names = T
)


raster_interes <- rast(capas_raster) %>% setNames(Años)
SisRef <- crs (raster_interes)

# Capas vector

# Municipio
ruta_archivo <- file.path(dir_Datos_Or,
                          "MUNICIPIOS/Carto100000_Colombia_DI_2022_gpkg/Carto100000_Colombia_DI_2022.gpkg")
st_layers(ruta_archivo)

# cargar y proyectar en el sistema de referencia definido
capas_st <- st_read(ruta_archivo,layer="Limite_Municipal_Poligono") %>% st_transform(SisRef)


# Bioma 
bioma <- st_read("~/GitHub/huella-humana-analisis/Datos/EcosistemasPotencialesDeColombia.gdb") %>% st_transform(SisRef)


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************
## preparar capa mixta
 unique(bioma$Bioma)

bioma["Bioma"]
capas_st [c(1,2,6)]
View(st_drop_geometry((capas_st)))

## El indicador  ####

plot(raster_interes)

# Clasificar la huella por sus rangos de intensidad 

## matriz de reclasificación

rc_matrix <- matrix(c(-1, 0, 1,
                      0, 15, 2,
                      15, 30, 3,
                      30, 50, 4,
                      50,100, 5), 
                    ncol = 3, byrow = TRUE)

raster_reclass <- classify(raster_interes, rc_matrix)

# Convertir a factor y asignar etiquetas
#levels(raster_reclass) <- Huella_cat


## capa base ####

r_base <- raster_interes[[1]]

## Rasterizar capas vectoriales ####
# se verifica si los raster ya existen , de lo contrario se generan y guardan

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm, "/", "COL_RATER9377_",Unidad_analsis, ".tif ")

# Arreglar los campos para rasterizar: revisar comentarios de las siguientes lineas para ver cual aributo definir

#atributo_rast <- c("dpto_cnmbr") # Departamento
#atributo_rast <- c("Bioma") # Bioma
atributo_rast <- c("MpCodigo") # para municipio


# Rasterizar capas vectoriales y asignar niveles

if (file.exists(raster_paths)) {
  r_aoi <- rast(raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  r_aoi <- capas_st %>%
    rasterize(y = r_base,
              field = atributo_rast,
              filename = raster_paths)
  
}

levels(r_aoi)

plot(r_aoi)

# Rasterizar capas vectoriales y asignar niveles (bioma)
raster_paths <- paste0(dir_Datos_Intm, "/", "COL_RATER9377_","Bioma", ".tif ")

# Arreglar los campos para rasterizar: revisar comentarios de las siguientes lineas para ver cual aributo definir


atributo_rastb <- c("Bioma") # Bioma



if (file.exists(raster_paths)) {
  r_bioma <- rast(raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  r_bioma <- capas_st %>%
    rasterize(y = r_base,
              field = atributo_rastb,
              filename = raster_paths)
  
}



lookup_bioma <- levels(r_bioma) [[1]]
lookup_muni <- levels(r_aoi) [[1]]
plot(r_aoi)
plot(r_bioma)

levels(r_aoi)


# crear combinacion bioma y municipio

r_bioma_100000 <- r_bioma*100000
levels(r_aoi_1000)
levels()

c_bioma_mun <- r_bioma_100000 +r_aoi



#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

# Se realiza un análisis espacial utilizando cada  departamentos para calcular estadísticas zonales
# del IHEH y sus categorías de intensidad. 
# los resutados se guarda en en dos data frames:
# Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar, Máximo y mínimo)
# Stat_reclass: Contiene la frecuencia y porcentaje de categorías de intesisdad de IHEH

## Estadísticas Zonales ####


capas_st <- capas_st%>% 
  mutate(area_cal_km2=units::drop_units(units::set_units(st_area(.),"km2"))) %>% 
  .[c(1,2,6,8)]# revisar que campos son de interes use 1,2,5,6 para municicpio, 2,4,6, bioma, xx c(1,2,5,6,8)




zonalTabla <- function(estadistico,r_zonas){
  zonal(raster_interes, r_zonas, fun = estadistico, na.rm=T) %>%
    tidyr::pivot_longer(
      cols = c(`2018`, `2020`, `2022`),   # columnas de años
      names_to = "Año",
      values_to = estadistico
    )
  
}


z_mean   <- zonalTabla("mean",c_bioma_mun)
z_sd     <- zonalTabla("sd",c_bioma_mun)
z_median <- zonalTabla("median",c_bioma_mun)
z_min <- zonalTabla("min", c_bioma_mun)
z_max <- zonalTabla("max", c_bioma_mun)



Stat_values <- Reduce(function(x, y) merge(x, y, by =c("Bioma", "Año")),
                      list(z_min,z_mean,z_median,z_max,z_sd))

# Separar niveles da municipio y bioma ara completar son la informacion
Stat_values <- Stat_values %>%
  mutate(muniLevel = Bioma %% 10000,
               BiomLevel = case_when(
               nchar(Bioma) == 7 ~ as.numeric(substr(Bioma, 1, 2)),
               nchar(Bioma) == 6 ~ as.numeric(substr(Bioma, 1, 1)),
               nchar(Bioma) < 6  ~ 0,
               TRUE ~ NA_real_
             )
           ) %>% 
  rename(CombiCOD=Bioma)
  



Stat_values <- merge(lookup_muni,Stat_values, by.x= "value" , by.y= "muniLevel") %>% 
  rename("muniLevel"=value)

Stat_values <- merge(lookup_bioma,Stat_values, by.x= "value" , by.y= "BiomLevel") %>% 
  rename("BiomLevel"=value)

Stat_values <- merge(capas_st,Stat_values, by= "MpCodigo") 


# Preparar los insumos para iterar los análisis por departamento 


pol_bioma_mun <- as.polygons(c_bioma_mun, dissolve = TRUE, na.rm = TRUE)

pol_bioma_mun <- st_as_sf(pol_bioma_mun)

list_deptos <- pol_bioma_mun %>% split(.[["Bioma"]])
#list_deptos [[c(-1088,-1089)]] quitar si municipios
list_deptos [[4]] #quitar si dpto
quitar <- c(1088,1089) # si dpto , no aplica para biomas


seq_along(list_deptos)[-quitar]
## Frecuencias de las categorías ####
# Construcción de listas

Stat_reclass <- data.frame()
i=5

atributo_rast <- "Bioma"

for (i in seq_along(list_deptos)) { # La indexación quinta las islas San Andrés y Providencia
  #for (i in seq_along(list_deptos)) { # si biomas
  
  
  Nombre_dept <-unique( list_deptos[[i]][[atributo_rast]])
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
  
  ### Para las clases ####
  
  # Calcular la frecuencia de las clases
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass <- group_by(frq_reclass, layer) %>%
    mutate(
      Cod_Zona = Nombre_dept,
      Porcentage = round(prop.table(count) * 100, 3),
      Categorías = factor(value, levels = 1:5, labels = Huella_cat$nom),
      Año = as.numeric(as.character(factor(
        layer,
        levels = 1:Años_numero,
        labels = Años
      )))
    ) %>%
    rename(Conteo = count)
  
  
  # guardar en Stat_reclass acumulando en cada iteración
  Stat_reclass <- rbind (Stat_reclass, tem_Stat_reclass) %>%
    ungroup()
}

#****************************************************************************
# Organizar las tablas y guardarlas ####
#****************************************************************************

Stat_values <-  Stat_values %>% st_drop_geometry()


Stat_reclass0 <- Stat_reclass %>% st_drop_geometry() %>% 
  dplyr::select( Cod_Zona, Año, Categorías, Conteo, Porcentage)%>% 
  rename("Porcentaje" = Porcentage)



# Separar niveles da municipio y bioma ara completar son la informacion
Stat_reclass0 <- Stat_reclass0 %>%
  mutate(muniLevel = Cod_Zona %% 10000,
         BiomLevel = case_when(
           nchar(Cod_Zona) == 7 ~ as.numeric(substr(Cod_Zona, 1, 2)),
           nchar(Cod_Zona) == 6 ~ as.numeric(substr(Cod_Zona, 1, 1)),
           nchar(Cod_Zona) < 6  ~ 0,
           TRUE ~ NA_real_
         )
  ) %>% 
  rename(CombiCOD=Cod_Zona)


Stat_reclass0 <- merge(lookup_muni,Stat_reclass0, by.x= "value" , by.y= "muniLevel") %>% 
  rename("muniLevel"=value)

Stat_reclass0 <- merge(lookup_bioma,Stat_reclass0, by.x= "value" , by.y= "BiomLevel") %>% 
  rename("BiomLevel"=value)

Stat_reclass0 <- merge(st_drop_geometry(capas_st),Stat_reclass0, by= "MpCodigo") 




# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEHcorine_stats_", "bio_mun",".csv"))
write_excel_csv2(Stat_reclass0, paste0(dir_Resultados, "/IHEHcorine_clases_","bio_mun",".csv"))


