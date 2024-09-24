# Título: IHEH en Sumapaz
#
# Descripción: análisis huella espacial humana 2019 en zonas especiales
#
# Autor(es): Alejandra Narváez Vallejo
#
# Por hacer o  corregir:

## funciones con variables por defecto
## El análisis aun mantiene el sistemas de referencia antiguo


#*******************************************************************************
# librerías o dependencias -----------------------------------------------------
#*******************************************************************************

## lectura de datos  ####

library (sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)


#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(),"..",".."))

dir_Datos_Or<- file.path("Datos", "Originales")
dir_Datos_Intm<- file.path("Datos","Intermedios")
dir_Resultados<- file.path ("Resultados")

#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(file.path("..", "..", "Funciones_comunes" , "estadísticas.R"))
source(file.path("..", "..", "Funciones_comunes" , "preprocesamiento.R"))

#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

nombres <- c("H2019")
nombre_capas<-c("paramo","runap","RCampesinas")
Huella_cat <- data.frame(Cat = 1:4,
                         nom = c("Natural", "Bajo", "Medio", "Alto"))


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas raster

capas_raster <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "tif$",
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

SisRef <- crs (raster_interes)

# cargar y proyectar en el sistema de referencia definido

capas_st <- lapply(capas_files[1:2], CargarProyectar)

capas_st[[3]]<-st_read(capas_files[3]) # se carga por aparte porque tiene problemas

names(capas_st)<-nombre_capas


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## El indicador  ####

# Recortar al área de estudio general
aoiG<- filter(capas_st[[1]], COMPLEJO == "Cruz Verde - Sumapaz")

raster_interes <- crop(raster_interes, aoiG, mask=TRUE)

plot(raster_interes)

# Clasificar la huella por sus rangos de intensidad 

## matriz de reclasificación

m <- c(0, 15, 1, 
       15, 40, 2,
       40, 60, 3,
       60, Inf, 4)

reclass_mat <- matrix(m, ncol = 3, byrow = TRUE)

## Aplicar la reclasificación

raster_reclass <- classify(raster_interes, reclass_mat, include.lowest =
                             TRUE)



levels(raster_reclass)<- Huella_cat

## Preparar mapas ####

# preparando las geometrías de ambos tipos de zonas

Runap<-capas_st$runap %>% 
  st_crop(aoiG)%>%
  st_intersection(aoiG$geometry ) %>%
  as("Spatial")%>%
  aggregate(by="nombre")%>%
  st_as_sf()

RCamp<-capas_st$RCampesinas %>% 
  st_crop(aoiG)%>%
  st_intersection(aoiG$geometry )


# parametros de gráficas

colores <- c("Natural" = "#2ca02c","Bajo" = "#1f77b4","Medio" = "#ff7f0e", "Alto" = "#d62728")

# Gráfico RUNAP
plot(raster_reclass, col= colores, 
     axes=FALSE, 
      box=TRUE, 
     main= "Intensidad de IHEH en RUNAP"
     )
plot(Runap, add=T, col= rgb(1, 1, 1, alpha = 0.3))

# Gráfico campesinas
plot(raster_reclass, col= colores, 
     axes=FALSE, 
     box=TRUE, 
     main= "Intensidad de IHEH en Z. Campesinas"
)
plot(RCamp, add=T, col= rgb(1, 1, 1, alpha = 0.3), lwd=.2)



## Rasterizar capas vectoriales ####

# capa base ####
r_base <- raster_interes[[1]]

# se verifica si los raster ya existen , de lo contrario se generan y guardan

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm,"/",nombre_capas,".tif ")


# Arreglar los campos para rasterizar

capas_st$RCampesinas$id_RC <- as.numeric(factor(capas_st$RCampesinas$Nombre))
capas_st$paramo$id_p <- as.numeric(factor(capas_st$paramo$COMPLEJO))

# atributos a usar para rasterizar

atributo_rast<- c("id_p",
                  "id_pnn",
                  "id_RC")


cat_rast<- c("COMPLEJO",
  "nombre",
              "Nombre")


# rasterizar capas vectoriales y asignar niveles

raster_capas <-setNames(lapply(seq_along(raster_paths), rasterizar), nombre_capas)

levels(raster_capas$runap)<- capas_st$runap[c(atributo_rast[2], cat_rast[2])]%>%st_drop_geometry()

levels(raster_capas$RCampesinas)<- capas_st$RCampesinas[c(atributo_rast[3], cat_rast[3])]%>%st_drop_geometry()


# Crear máscara de integridad de zonas no especiales #### 

raster_path <- paste0(dir_Datos_Intm,"/","mask_NoEspecial",".tif ")

if (file.exists(raster_path)) {
  exmask0 <- rast(raster_path)
  
} else {
  exmask0 <- Reduce(function(x, y) mask(x, y, inverse = TRUE), raster_capas[2:3], init = raster_interes)
  
  # Guardar el raster resultante en un archivo
  writeRaster(exmask0, raster_path)
}


plot(exmask0)

#****************************************************************************
# Análisis por zonas a indagar -------------------------------------------
#****************************************************************************

# Se realiza un análisis espacial utilizando cada  entidad de las zonas especiales para calcular estadísticas zonales del IHEH y sus categorías de intensidad. 
# los resutados se guarda en en dos 
# Stat_reclass: Contiene la frecuencia y porcentaje de categorías de intensidad de IHEH


# Preparar los insumos para iterar los análisis por entidad de las zonas especiales

# Correr manualmente 

# para RCampesinas

list_deptos <- capas_st$RCampesinas %>% 
  st_crop(aoiG)%>%
  split(., .$Nombre)


# para Runap
list_deptos <- capas_st$runap %>% 
  st_crop(aoiG)%>%
  st_intersection(aoiG$geometry ) %>%
  as("Spatial")%>%
  aggregate(by="nombre")%>%
  st_as_sf()%>%
  split(., .$nombre)


# Construcción de listas

Stat_reclass <- data.frame()
Stat_values <- data.frame()

i<-2

for (i in seq_along(list_deptos)) {

  Nombre_dept <- list_deptos[[i]]$Nombre # reservas campesinas
  #Nombre_dept <- list_deptos[[i]]$nombre # runap
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  r_aoi <- definicionAOI(raster_interes, i)
  r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
  
  ## Calcular estadísticas zonales ####
  ### Para los valores ####
  
  # resumen <- terra::global(r_aoi, fun = c("mean", "std"), na.rm = TRUE)
  # resumen_median <- global(r_aoi, fun = mediana_fun)
  # 
  # resumen <- cbind(Nombre_dept, resumen[1], resumen_median, resumen[2])
  # 
  # # Renombrar las columnas para que tengan nombres más descriptivos
  # 
  # names(resumen) <- c("Municipio", "Promedio", "Mediana", "Desviación estandar")
  # resumen$Año <- as.numeric(gsub("H", "", row.names(resumen)))
  # 
  # # guardar en Stat_values acumulando en cada iteración
  # Stat_values <- rbind (Stat_values, resumen)
  
  ### Para las clases ####
  
  # Calcular la frecuencia de las clases
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass <- group_by(frq_reclass, layer) %>%
    mutate(
      Municipio = Nombre_dept,
      Porcentage = round(prop.table(count) * 100, 3),
      Categorías = value
    ) %>%
    rename(Conteo = count)
  
  
  # guardar en Stat_reclass acumulando en cada iteración
  Stat_reclass <- rbind (Stat_reclass, tem_Stat_reclass) %>%
    ungroup()
  
}


#****************************************************************************
# Organizar las tablas y guardarlas ####
#****************************************************************************


Stat_reclass<-na.omit(Stat_reclass)%>%
  mutate(Area_ha=res(raster_interes)*res(raster_interes)*Conteo/10000)%>%
  rename(Zona=Municipio)

Stat_reclass <- dplyr::select(Stat_reclass, Zona, Categorías, Conteo, Porcentage, Area_ha)

Stat_reclassG<-Stat_reclass%>%
  group_by(Categorías)%>%
  summarise(conteo=sum(Conteo))%>%
  ungroup%>%
  mutate(Porcentage = round(prop.table(conteo) * 100, 3))

ggplot(Stat_reclass)+
  geom_bar(aes(x=Zona, y=Area_ha, fill =Categorías ), stat="identity")+
  scale_fill_manual(
    values = c(
      "Bajo" = "#1f77b4",      # Color azul para "bajo"
      "Medio" = "#ff7f0e",     # Color naranja para "medio"
      "Alto" = "#d62728",      # Color verde para "alto"
      "Natural" = "#2ca02c"     # Color rojo para "natural"
    )
  )+
  labs(title= "Reservas Campesinas",
       x="", 
       y="Área (ha)")

ggsave(file.path(dir_Resultados, "Promedio_IHEH.png"), width = 16.98625, height =  11.66812, units="cm")



ggplot(Stat_reclass)+
  geom_bar(aes(x=Zona, y=Porcentage), stat="identity")

# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/Rcampesinas.csv"))
write_excel_csv2(Stat_reclassG, paste0(dir_Resultados, "/RcampesinasG.csv"))

write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/Runap.csv"))
write_excel_csv2(Stat_reclassG, paste0(dir_Resultados, "/RunapG.csv"))


# gráfico de análisis de LUIS ####

Integridad<-read.csv2(paste0(dir_Resultados, "/Luis.csv"))

Int_ZC<-Integridad%>%
  dplyr::filter(Zonas.especiales=="Reservas campesinas", 
                Categoría!="Área total")%>%
  mutate(Categoría=factor(Categoría,levels=c("No Bosque", "Integridad Baja","Integridad Media", "Integridad Alta" )))%>%
  na.omit()

ggplot(Int_ZC)+
  geom_bar(aes(x=Áreas.de.interés, y=Hectáreas, fill =Categoría ), stat="identity")+
  scale_fill_manual(
    values = c(
      "Integridad Baja" = "#1f77b4",      # Color azul para "bajo"
      "Integridad Media" = "#ff7f0e",     # Color naranja para "medio"
      "Integridad Alta" = "#d62728",      # Color verde para "alto"
      "No Bosque" = "#2ca02c"     # Color rojo para "natural"
    )
  )+
  labs(title= "Reservas Campesinas",
       x="", 
       y="Área (ha)")


