# Título: IHEH en la Amazonas
#
# Descripción: análisis multitemporal de la nueva huella espacial humana region amazonas y todos los núcleos. implica manipulación manual para cambiar la corrida de los núcleos o de la región amazonía.
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
dir_Resultados<- file.path ("Resultados","IHEH")


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
Huella_cat <- data.frame(Cat = 1:5,
                         nom = c("Natural", "Bajo", "Medio", "Alto", "Muy Alto"))


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas raster
## Huella continua

capas_raster <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "IHEH_IAVH1.*\\.tif$",
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

# Clasificar la huella por sus rangos de intensidad 

## matriz de reclasificación

m <- c(-1, 0, 1,
       0, 15, 2,
       15, 30, 3,
       30, 50, 4,
       50,100, 5)

reclass_mat <- matrix(m, ncol = 3, byrow = TRUE)

## Aplicar la reclasificación

raster_reclass <- classify(raster_interes, reclass_mat)


## capa base ####

r_base <- raster_interes[[1]]
plot(r_base)
## Rasterizar capas vectoriales ####
# se verifica si los raster ya existen , de lo contrario se generan y guardan

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm, "/", "AOI", ".tif ")

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
# Análisis por unidad de análisis ----------------------------
#****************************************************************************

# Se realiza un análisis espacial  a la unidad de analisis. El bucle está para años
# los resutados se guarda en en dos data frames:
# Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar)
# Stat_reclass: Contiene la frecuencia y porcentaje de categorías de intesisdad de IHEH


# Preparar los insumos para iterar los análisis por departamento 

list_deptos <- list(capas_st)

# Construcción de listas

Stat_reclass <- data.frame()
Stat_values <- data.frame()

i=1
for (i in seq_along(list_deptos)) {
  
  Nombre_dept <- "NDFyB" # todos los nucleos
  #Nombre_dept <- "Amazonas" # todo amazonas
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  r_aoi <- definicionAOI(raster_interes, i)
  r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
  
  ## Calcular estadísticas zonales ####
  ### Para los valores ####
  
  resumen <- terra::global(r_aoi, fun = c("mean", "sd"), na.rm = TRUE)
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
      Categorías = factor(value, levels = 1:5, labels = Huella_cat$nom),
      Año = as.numeric(as.character(factor(
        layer,
        levels = 1:3,
        labels = c( 2018,2020,2022)
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
                             `Desviación estandar`)%>%
  rename("Núcleo"=Municipio)
row.names(Stat_values) <- NULL

Stat_reclass <- dplyr::select(Stat_reclass, Municipio, Año, Categorías, Conteo, Porcentaje)%>%
  rename("Núcleo"=Municipio)


# Guardar la información de las estadísticas zonales

# Para núcleos
write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEH_stats_NDFyB_n.csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/IHEH_clases_NDFyB_n.csv"))

# para amazonas

write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEH_stats_Amazonas_n.csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/IHEH_clases_Amazonas_n.csv"))


# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(3, 5), digits = 2)


# Guardar la tabla en un archivo HTML
# # para nucleo
# saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEH_NDFyB_n.html"))

# para amazonas
saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEH_Amazonas_n.html"))

t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 5, digits = 2)
# # para nucleos
# saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEH__NDFyB.html"))

# para amazonas
saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEH_Amazonas.html"))

#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####
set.seed()

paleta_colores1 <- sample(colors(), 22)
paleta_colores1 <- distinctColorPalette(22)


ggplot(Stat_values) +
  geom_line( mapping=aes(x = Año, y = Promedio, colour = Núcleo), linewidth =              1) +
  #  geom_line(data=Stat_values[71:130,], mapping=aes(x = Año, y = Promedio, colour = Municipio), linewidth =  1, lty=4) +
  geom_ribbon(aes(x=Año,ymin = Promedio - `Desviación estandar`, ymax = Promedio + `Desviación estandar`), fill = "blue", alpha = 0.2)+  # Área sombreada
  labs(x = "",
       y = "Promedio ± SD",
       colour="",
       title = "Promedio ± SD de todos los núcleos")+
  theme_bw()+
  theme(title=element_text(size=8))

ggsave(file.path(dir_Resultados, "Promedio_IHEH_NDFyB.png"), width = 15, height =  10, units="cm")


# Una sola gráfica ara amazonas región y núcleos

n<-read_csv2(paste0(dir_Resultados, "/IHEH_stats_NDFyB_n.csv"))
a<- read_csv2(paste0(dir_Resultados, "/IHEH_stats_Amazonas_n.csv"))

an<- rbind(n,a)

ggplot(an) +
  geom_line( mapping=aes(x = Año, y = Promedio, colour = Núcleo), linewidth =              1) +
  #  geom_line(data=Stat_values[71:130,], mapping=aes(x = Año, y = Promedio, colour = Municipio), linewidth =  1, lty=4) +
  geom_ribbon(aes(x=Año,ymin = Promedio - `Desviación estandar`, ymax = Promedio + `Desviación estandar`, fill=Núcleo), alpha = 0.2)+  # Área sombreada
  labs(x = "",
       y = "IHEH",
       colour="Promedio",
       fill="Promedio ± SD"
  )+
  theme_bw()+
  theme(title=element_text(size=8))

ggsave(file.path(dir_Resultados, "Promedio_IHEH_Nucleos _Region_n.png"), width = 15, height =  10, units="cm")






#Stat_valuesN<- Stat_values





dev.size(units="cm")






## Gráfico sencillo clases ####
Stat_reclass$Categorías <- as.factor(Stat_reclass$`Categorías`)

Stat_reclass0  <-   Stat_reclass
gg <- Stat_reclass0 %>%
  ggplot(aes(y = Porcentaje, x = Año, alluvium = Categorías))
# barras color , alluvium con color
gg +
  geom_alluvium(
    aes(fill = Categorías, colour = Categorías),
    width = 1,
    alpha = .4,
    
    curve_type = "arctangent",
    curve_range = 1
  ) +
  geom_stratum(aes(stratum = Categorías
                   , fill = Categorías), #     alpha = .4),
               #decreasing = FALSE,
               width = 1,
               linewidth=.2,
               color="grey30") +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  scale_colour_brewer(palette = "RdYlGn", direction = -1) +
  labs(y = "Porcentaje de área", x = "") +
  facet_wrap(~ Núcleo, scales = "fixed", ncol=3) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(dir_Resultados, "Porcentaje_Categorias_NDFyB_n.png"), width = 15, height =  10, units= "cm")
ggsave(file.path(dir_Resultados, "Porcentaje_Categorias_Amazonas_n.png"), width = 15, height =  10, units= "cm")

dev.size(units="cm")
