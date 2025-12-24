# Título: sci en la Amazonas
#
# Descripción: análisis multitemporal de SCI amazonas y todos los núcleos. implica manipulación manual para cambiar la corrida de los núcleos o de la región amazonía.
# No parte la región en sus componentes sino que hace un cálculo global único
#Ejemplo: aquí se debe elegir que opción usar si la de núclos o la de Amazonas.
##capas_st<-st_read(grep(capas_files, pattern="Amazonia_proHuella", value=T))# nucleos
#capas_st <- lapply(grep(capas_files, pattern="AMAZONAS.shp", value=T), CargarProyectar)[[1]] # amazonas
#
# Autor(es): Alejandra Narváez Vallejo
#
# Por hacer o  corregir:

## El análisis aun mantiene el sistemas de referencia antiguo


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
dir_Resultados<- file.path ("Resultados","SCI")


#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(file.path("..", "..", "Funciones_comunes" , "estadísticas.R"))
source(file.path("..", "..", "Funciones_comunes" , "preprocesamiento.R"))
#source(file.path("..", "..", "Funciones_comunes" , "visualización.R"))

#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

# corrida nueva huella ecosisitemas
nombres <- c( "H2018", "H2020", "H2022")


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas raster
## Huella continua

capas_raster <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "SCI_Colombia.*\\.tif$",
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

capas_st <- lapply(grep(capas_files, pattern="Amazonia_proHuella", value=T), CargarProyectar)[[1]] # nucleos
#capas_st <- lapply(grep(capas_files, pattern="AMAZONAS.shp", value=T), CargarProyectar)[[1]] # amazonas


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

sf::sf_use_s2(F)

## El indicador  ####

# Recortar al área de estudio general

raster_interes <- crop(raster_interes, capas_st) %>% mask (capas_st)

plot(raster_interes)

## capa base ####

r_base <- raster_interes[[1]]
plot(r_base)
## Rasterizar capas vectoriales ####
# se verifica si los raster ya existen , de lo contrario se generan y guardan

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm, "/", "AOI_SCI", ".tif ")

# Arreglar los campos para rasterizar

capas_st$ID <- as.numeric(capas_st$ID) #################################### NUCLEOS
#capas_st$ID <- as.numeric(capas_st$OBJECTID) #################################### AMAZONAS

# atributos a usar para rasterizar

atributo_rast <- c("ID")

capas_st$nom_simp <- gsub("NDFyB\\s*", "", capas_st$Area_PlanC)# de núcleos
#capas_st$nom_simp <- "Amazonas"
cat_rast <- c("nom_simp")


# Rasterizar capas vectoriales y asignar niveles

if (file.exists(raster_paths)) {
  r_aoi <- rast(raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  r_aoi <- capas_st %>%
    rasterize(y = r_base,
              field = atributo_rast,
              filename = raster_paths,
              overwrite=TRUE)
  
}

# esto es para Amazonas region
r_aoi <- capas_st %>%
  rasterize(y = r_base,
            field = atributo_rast)


levels(r_aoi) <- capas_st[c(atributo_rast, cat_rast)] %>% st_drop_geometry()

plot(r_aoi)
#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

# Se realiza un análisis espacial utilizando la UA para calcular estadísticas zonales
## Se realiza un análisis espacial  a la unidad de analisis. El bucle para años
# los resutados se guarda en en dos data frames:
# Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar)
# Stat_reclass: Contiene la frecuencia y porcentaje de categorías de intesisdad de IHEH


# Preparar los insumos para iterar los análisis por departamento 

list_deptos <- list(capas_st)
# número de pixeles en los núcleos
r_aoi_tot <- sum(as.integer(freq(r_aoi)$count))


i=1
for (i in seq_along(list_deptos)) {
  
  Nombre_dept <- "NDFyB" # todos los nucleos
  #Nombre_dept <- "Amazonas" # todo amazonas
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  r_aoi_dat <- definicionAOI(raster_interes, i)
  
  
  ## Calcular estadísticas zonales ####
  ### Para los valores ####
  
  freq_df <- as_tibble(freq(r_aoi_dat))
  colnames(freq_df) <- c("Capa","Valor", "Frecuencia")
  freq_df$Capa <- as.factor(freq_df$Capa)
  freq_df$Frecuencia <- as.integer(freq_df$Frecuencia)
  freq_df <- rbind(freq_df,c(1,0,r_aoi_tot-sum(freq_df$Frecuencia[freq_df$Capa==1])))
  freq_df <- rbind(freq_df,c(2,0,r_aoi_tot-sum(freq_df$Frecuencia[freq_df$Capa==2])))
  freq_df <- rbind(freq_df,c(3,0,r_aoi_tot-sum(freq_df$Frecuencia[freq_df$Capa==3])))
  
  
  freq_df <- freq_df %>%
    mutate(Capa = case_when(
      Capa == 1 ~ "2018",
      Capa == 2 ~ "2020",
      Capa == 3 ~ "2022",
      TRUE ~ as.character(Capa)
    ),
    region =Nombre_dept) %>% 
    group_by(Capa) %>%
    mutate(Porcentaje = 100 * Frecuencia / sum(Frecuencia)) %>%
    ungroup()
  
  
  ggplot(freq_df, aes(x = factor(Valor), y = Porcentaje, fill = Capa)) +
    geom_bar(stat = "identity",  position = position_dodge()
             )+
    facet_wrap(~ region, scales = "fixed", ncol=3) +
    theme_bw() +
    labs(
      fill = "",
      x = "ICEB",
      y = "Porcentaje (%)"
    )+
    theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(size=10))
  
    
  ggsave(file.path(dir_Resultados, "Porcentaje_Categorias_SCI_NDFyB_n.png"), width = 15, height =  10, units= "cm")
  ggsave(file.path(dir_Resultados, "Porcentaje_Categorias_SCIAmazonas_n.png"), width = 15, height =  10, units= "cm")
    
    
  }

#****************************************************************************
# Organizar las tablas y guardarlas ####
#****************************************************************************

# Guardar la información de las estadísticas zonales

# Para núcleos
write_excel_csv2(freq_df, paste0(dir_Resultados, "/SCI_stats_NDFyB_n.csv"))


# para amazonas

write_excel_csv2(freq_df, paste0(dir_Resultados, "/SCI_stats_Amazonas_n.csv"))



# Elaborar tablas dinámicas
t1 <- datatable(freq_df,
                options = list(
                  pageLength = 18 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) 


# Guardar la tabla en un archivo HTML
# # para nucleo
# saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_SCI_NDFyB_n.html"))

# para amazonas
saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEH_Amazonas_n.html"))
