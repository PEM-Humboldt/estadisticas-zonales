# Título: Serie de tiempo de IHEH por departamentos 
#
# Descripción: análisis multitemporal de la huella espacial humana por Departamentos
#
# Estadísticas (Promedio , mediana , desviación estándar) anuales por departamento de la huella Espacial humana
# Porcentajes de área en las clases de intensidad de huella humana (bajo, medio, alto)
# Gráficas de barras mostrando las categorías de intensidad de huella por departamentos
# Mapa Nacional de huella espacial humana enfocándose en los 10 departamentos más afectados
  

# Autor(es): Alejandra Narváez Vallejo


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

nombres <- c("H2022")
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

raster_interes <- rast(capas_raster[6]) %>% setNames(nombres)


# Capas vector

capas_files <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "shp$",
  full.names = T
)

SisRef <- crs (raster_interes)

# cargar y proyectar en el sistema de referencia definido

capas_st <- lapply(capas_files[1], CargarProyectar)[[1]]


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## El indicador  ####

# Recortar al área de estudio general

raster_interes <- crop(raster_interes, capas_st) %>% mask (capas_st)

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


## capa base ####

r_base <- raster_interes[[1]]

## Rasterizar capas vectoriales ####
# se verifica si los raster ya existen , de lo contrario se generan y guardan

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm, "/", "AOI", ".tif ")

# Arreglar los campos para rasterizar

capas_st$dpto_ccdgo  <- as.numeric(capas_st$dpto_ccdgo)

# atributos a usar para rasterizar

atributo_rast <- c("dpto_ccdgo")

cat_rast <- c("dpto_cnmbr")


# Rasterizar capas vectoriales y asignar niveles

if (file.exists(raster_paths)) {
  r_aoi <- rast(raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  r_aoi <- capas_st %>%
    rasterize(y = r_base,
              field = atributo_rast [1],
              filename = raster_paths)
  
}

levels(r_aoi) <- capas_st[c(atributo_rast, cat_rast)] %>% st_drop_geometry()


#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

# Se realiza un análisis espacial utilizando cada  departamentos para calcular estadísticas zonales
# del IHEH y sus categorías de intensidad. 
# los resutados se guarda en en dos data frames:
# Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar)
# Stat_reclass: Contiene la frecuencia y porcentaje de categorías de intesisdad de IHEH


# Preparar los insumos para iterar los análisis por departamento 

list_deptos <- capas_st %>% split(., .$dpto_ccdgo )

# Construcción de listas

Stat_reclass <- data.frame()
Stat_values <- data.frame()

i=1

for (i in seq_along(list_deptos)) {
  
  Nombre_dept <- list_deptos[[i]]$dpto_cnmbr
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  r_aoi <- definicionAOI(raster_interes, i)
  r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
  
  ## Calcular estadísticas zonales ####
  ### Para los valores ####
  
  resumen <- terra::global(r_aoi, fun = c("mean", "std"), na.rm = TRUE)
  resumen_median <- global(r_aoi, fun = mediana_fun)
  
  resumen <- cbind(Nombre_dept, resumen[1], resumen_median, resumen[2])
  
  # Renombrar las columnas para que tengan nombres más descriptivos
  
  names(resumen) <- c("Municipio", "Promedio", "Mediana", "Desviación estandar")
  resumen$Año <- as.numeric(gsub("H", "", row.names(resumen)))
  
  # guardar en Stat_values acumulando en cada iteración
  Stat_values <- rbind (Stat_values, resumen)
  
  ### Para las clases ####
  
  # Calcular la frecuencia de las clases
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass <- group_by(frq_reclass, layer) %>%
    mutate(
      Municipio = Nombre_dept,
      Porcentaje = round(prop.table(count) * 100, 3),
      Categorías = factor(value, levels = 1:4, labels = Huella_cat$nom),
      Año = as.numeric(as.character(factor(
        layer,
        levels = 1:5,
        labels = c(1970, 1990, 2000, 2015, 2018)
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

Stat_values <- dplyr::select(Stat_values,
                             Municipio,
                             Año,
                             Promedio,
                             Mediana,
                             `Desviación estandar`)
row.names(Stat_values) <- NULL

Stat_reclass <- dplyr::select(Stat_reclass, Municipio, Año, Categorías, Conteo, Porcentaje)


# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(3: 5), digits = 2,dec.mark = ",", mark = ".")
  
t1


# Guardar la tabla en un archivo HTML
saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEH_nal.html"))

t2 <- datatable(Stat_reclass[-4],
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 4, digits = 2, dec.mark = ",", mark = ".")

t2
saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEH_nal.html"))

#****************************************************************************
# Gráficas ####
#****************************************************************************


#Stat_reclass0 <- dplyr::select(Stat_reclass,  Categorías, Conteo, Porcentaje)

Stat_reclassG<-Stat_reclass%>%
  group_by(Categorías)%>%
  summarise(conteo=sum(Conteo))%>%
  ungroup%>%
  mutate(Porcentage = round(prop.table(conteo) * 100, 3))


Stat_reclass %>% 
  filter(Municipio %in% Stat_values$Municipio[c(2,22,11,19,10,6,9,15,21,5)]) %>% 
  mutate(Municipio=factor(Municipio, levels = Stat_values$Municipio[c(2,22,11,19,10,6,9,15,21,5)])) %>% 
  ggplot()+
  geom_bar(aes(x=Municipio, y=Porcentaje, fill =Categorías ), stat="identity")+
  scale_fill_manual(
    values = c(
      "Bajo" = "#1f77b4",      # Color azul para "bajo"
      "Medio" = "#ff7f0e",     # Color naranja para "medio"
      "Alto" = "#d62728",      # Color verde para "alto"
      "Natural" = "#2ca02c"     # Color rojo para "natural"
    )
  )+
  labs(
       x="", 
       y="Porcentaje")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave(file.path(dir_Resultados, "Promedio_IHEH.png"), width = 16.98625, height =  11.66812, units="cm")



# parametros de gráficas

levels(raster_reclass)<- Huella_cat 

sub <- capas_st%>% 
  filter(!dpto_cnmbr %in% Stat_values$Municipio[c(2,22,11,19,10,6,9,15,21,5)])

sub1 <- capas_st%>% 
  filter(dpto_cnmbr %in% Stat_values$Municipio[c(2,22,11,19,10,6,9,15,21,5)])

colores <- c("Natural" = "#2ca02c","Bajo" = "#1f77b4","Medio" = "#ff7f0e", "Alto" = "#d62728")

# Gráfico RUNAP
plot(raster_reclass, col= colores, 
     axes=FALSE, 
     box=TRUE
)
plot(sub, add=T, col= rgb(1, 1, 1, alpha = 0.5))
plot(sub1, add=T, col= rgb(1, 1, 1, alpha =0))



