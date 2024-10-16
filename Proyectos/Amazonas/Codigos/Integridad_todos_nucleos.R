# Título: Integridad del bosque en la Amazonas
#
# Descripción: análisis una capa de integridad 2019_ en amazonas y todos los núcleos. implica manipulación manual para cambiar la corrida de los núcleos o de la región amazonía.
#Ejemplo: aquí se debe elegir que opción usar si la de núclos o la de Amazonas.
##capas_st<-st_read(grep(capas_files, pattern="Amazonia_proHuella", value=T))# nucleos
#capas_st <- lapply(grep(capas_files, pattern="AMAZONAS.shp", value=T), CargarProyectar)[[1]] # amazonas
#
# Autor(es): Alejandra Narváez Vallejo
#
# Por hacer o  corregir:

#*******************************************************************************
# librerías o dependencias -----------------------------------------------------
#*******************************************************************************

## lectura de datos  ####

library (sf)
library(terra)
library(dplyr)
library(ggplot2)
library(randomcoloR)
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

setwd(file.path(this.path::this.path(),"..",".."))

dir_Datos_Or<- file.path("Datos", "Originales")
dir_Datos_Intm<- file.path("Datos","Intermedios")
dir_Resultados<- file.path ("Resultados", "Integridad_de_bosque")


#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(file.path("..", "..", "Funciones_comunes" , "estadísticas.R"))
source(file.path("..", "..", "Funciones_comunes" , "preprocesamiento.R"))
#source(file.path("..", "..", "Funciones_comunes" , "visualización.R"))

#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

nombres <- c( "2019")
Integridad_cat <- data.frame(Cat = 1:3,
                             nom = c( "Baja", "Media", "Alta"))

# Matriz de reclasificación Integridad

m <- c(0, 7, 1, 
       7, 14, 2,
       14, Inf, 3)

reclass_mat <- matrix(m, ncol = 3, byrow = TRUE)

#areaSel<-"NDFyB"#"Amazonas"
areaSel<-"Amazonas"  
#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas raster

capas_raster <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "^FSII.*\\.tif$",
  full.names = T
)

raster_interes <- rast(capas_raster) %>% setNames(nombres)

# Capas vector

capas_files <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "shp$",
  full.names = T
)

capas_files

SisRef <- crs (raster_interes)

# cargar y proyectar en el sistema de referencia definido

#capas_st<-st_read(grep(capas_files, pattern="Amazonia9377", value=T))%>%st_union()%>%st_sf
#st_crs(capas_st)<-9377

capas_st <- lapply(grep(capas_files, pattern="AMAZONAS.shp", value=T), CargarProyectar)[[1]]# Amazonas

plot(capas_st)

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

sf::sf_use_s2(F)

# Reclasificar

raster_reclass <- classify(raster_interes, reclass_mat, include.lowest =FALSE)%>%
  crop( capas_st)

plot(raster_reclass)


## capa base ####

r_base <- raster_reclass[[1]]
plot(r_base)


## Rasterizar capas vectoriales ####


# Arreglar los campos para rasterizar

capas_st$ID <- 1

# atributos a usar para rasterizar

atributo_rast <- c("ID")
capas_st$nom_simp <- areaSel
cat_rast <- c("nom_simp")


# Rasterizar capas vectoriales y asignar niveles

r_aoi <- capas_st %>%
  rasterize(y = r_base,
            field = atributo_rast)



levels(r_aoi) <- capas_st[c(atributo_rast, cat_rast)] %>% st_drop_geometry()

plot(r_aoi)


#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

# Se realiza un análisis espacial utilizando cada  departamentos para calcular estadísticas zonales
# del IHEH y sus categorías de intensidad. 
# los resutados se guarda en en dos data frames:
# Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar)
# Stat_reclass: Contiene la frecuencia y porcentaje de categorías de intesisdad de IHEH


# Preparar los insumos para iterar los análisis por departamento 

list_deptos <- list(capas_st)

# Construcción de listas

Stat_reclass <- data.frame()

for (i in seq_along(list_deptos)) {
  
  Nombre_dept <- areaSel
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  
  r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
  
  ### Para las clases ####
  
  # Calcular la frecuencia de las clases
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass <- group_by(frq_reclass, layer) %>%
    mutate(
      Municipio = Nombre_dept,
      Porcentaje = round(prop.table(count) * 100, 3),
      Categorías = factor(value, levels = 1:3, labels = Integridad_cat$nom),
      Año = as.numeric(as.character(factor(
        layer,
        levels = 1,
        labels = c( 2019)
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

Stat_reclass <- dplyr::select(Stat_reclass, Municipio, Año, Categorías, Conteo, Porcentaje)


# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/Int2019_clases_",areaSel,".csv"))

#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####

# llamr las tablas de los datos de la región amazonas y los núcleos , para dibujar en una única gráfica.

n<-read_csv2(paste0(dir_Resultados, "/Int2019_clases_NDFyB.csv"))
a<-read_csv2(paste0(dir_Resultados, "/Int2019_clases_Amazonas.csv"))


an<- rbind(n,a)

an



## Gráfico sencillo clases ####

an$Categorías <- factor(an$Categorías, levels=Integridad_cat$nom)

gg <- an %>%
  ggplot(aes(y = Porcentaje, x = Municipio, fill= Categorías))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Baja" = "red3", "Media" = "orange", "Alta" = "darkgreen"))+
  labs(y="Porcentaje de bosque potencial",
       x="", fill= "Integridad")+
  theme_bw()+
  theme(axis.text.y= element_text(hjust = .5), legend.position =  "bottom")

gg

ggsave(file.path(dir_Resultados, "PorcentajeBosquePotencial_Amazonas_NDFyB.png"))

gg
