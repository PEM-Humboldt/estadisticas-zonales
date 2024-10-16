# Título: IHEH en la Amazonas
#
# Descripción: análisis multitemporal de la huella espacial humana por núcleos base
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
dir_Resultados<- file.path ("Resultados", "IHEH")


#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(file.path("..", "..", "Funciones_comunes" , "estadísticas.R"))
source(file.path("..", "..", "Funciones_comunes" , "preprocesamiento.R"))
#source(file.path("..", "..", "Funciones_comunes" , "visualización.R"))

#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

nombres <- c("H1970", "H1990", "H2000", "H2015", "H2018", "H2019")
Huella_cat <- data.frame(Cat = 1:4,
                         nom = c("Natural", "Bajo", "Medio", "Alto"))



#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas raster

capas_raster <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "IHEH_.*\\.tif$",
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



# cargar y proyectar en el sistema de referencia definido

capas_st<-st_read((grep(capas_files, pattern="Amazonia_proHuella", value=T)))
SisRef <- crs (capas_st)
capas_st0 <- lapply(grep(capas_files, pattern="Bioma_clase", value=T), CargarProyectar)[[1]] # problemas con la proyección


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

sf::sf_use_s2(F)

# Elegir núcleos de interés

capas_st<-filter(capas_st, Area_PlanC %in% c( "NDFyB PNN Sierra de La Macarena" , "NDFyB PNN Tinigua" ))


## El indicador  ####

# Recortar al área de estudio general

raster_interes <- crop(raster_interes, capas_st) %>% mask (capas_st)

plot(raster_interes)

capas_st0AOI<-st_intersection(capas_st0, capas_st["Area_PlanC"])

plot(capas_st0AOI)

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
plot(r_base)

## Rasterizar capas vectoriales ####
# se verifica si los raster ya existen , de lo contrario se generan y guardan


# Arreglar los campos para rasterizar

capas_st0AOI$ID <- as.numeric(as.factor(capas_st0AOI$BIOMA_c)) #################################### NUCLEOS
#capas_st$ID <- as.numeric(capas_st$OBJECTID) #################################### AMAZONAS
capas_st0AOI$IDG <- paste(capas_st0AOI$Area_PlanC, capas_st0AOI$ID, sep = "_")

# atributos a usar para rasterizar

atributo_rast <- c("ID")

cat_rast <- c("BIOMA_c")


# Rasterizar capas vectoriales y asignar niveles

r_aoi <- capas_st0AOI %>%
  rasterize(y = r_base,
            field = atributo_rast)
            
niveles <- unique(capas_st0AOI[c(atributo_rast, cat_rast)] %>% st_drop_geometry())

levels(r_aoi) <- niveles

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


entidad<-"IDG"
list_deptos <- capas_st0AOI %>% split(., .[[entidad]])


# Construcción de listas

Stat_reclass <- data.frame()
Stat_values <- data.frame()

i=1
for (i in seq_along(list_deptos)) {

  Nombre_dept <- list_deptos[[i]][[entidad]]
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  r_aoi <- definicionAOI(raster_interes, i)
  r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
  
  ## Calcular estadísticas zonales ####
  ### Para los valores ####
  
  resumen <- terra::global(r_aoi, fun = c("mean", "std"), na.rm = TRUE)
  resumen_median <- global(r_aoi, fun = mediana_fun)
  resumenCell<-ncell(r_aoi)
  
  resumen <- cbind(Nombre_dept,resumenCell, resumen[1], resumen_median, resumen[2])
  
  # Renombrar las columnas para que tengan nombres más descriptivos
  
  names(resumen) <- c("Municipio", "Conteo","Promedio", "Mediana", "Desviación estandar")
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
        levels = 1:6,
        labels = c(1970, 1990, 2000, 2015, 2018,2019)
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
library(stringr)
## Stat_values ####
Stat_values

Stat_values <- dplyr::select(Stat_values,
                             Municipio,
                             Conteo,
                             Año,
                             Promedio,
                             Mediana,
                             `Desviación estandar`)
row.names(Stat_values) <- NULL

# Separar el ID y reconstruir bioma

Stat_values[c('Núcleo', 'Bioma')] <- str_split_fixed(Stat_values$Municipio, '_', 2)

Stat_values <- merge(Stat_values, niveles, by.x="Bioma", by.y="ID")

# reorganizar la tabla

Stat_values <- dplyr::select(Stat_values,
                             Núcleo,
                             BIOMA_c,
                             Conteo,
                             Año,
                             Promedio,
                             Mediana,
                             `Desviación estandar`)

## Stat_values ####
Stat_reclass <- dplyr::select(Stat_reclass, Municipio, Año, Categorías, Conteo, Porcentaje)


# Separar el ID y reconstruir bioma

Stat_reclass[c('Núcleo', 'Bioma')] <- str_split_fixed(Stat_reclass$Municipio, '_', 2)

Stat_reclass <- merge(Stat_reclass, niveles, by.x="Bioma", by.y="ID")

# reorganizar la tabla

Stat_reclass <- dplyr::select(Stat_reclass,  
                              Núcleo,
                              BIOMA_c, 
                              Año, 
                              Categorías,
                              Conteo, 
                              Porcentaje)


# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEH_stats_NDFyBsel.csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/IHEH_clases_NDFyBsel.csv"))


# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 20 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(4:6), digits = 2)


# Guardar la tabla en un archivo HTML
saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEH_NDFyBsel.html"))


t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 6, digits = 2)

saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEH__NDFyBsel.html"))


#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####
set.seed(2)

paleta_colores1 <- distinctColorPalette(5)

Stat_values%>%
  dplyr::filter(Núcleo == "NDFyB PNN Tinigua")%>%
ggplot() +
  geom_line( mapping=aes(x = Año, y = Promedio, colour = BIOMA_c), linewidth =  1) +
  #  geom_line(data=Stat_values[71:130,], mapping=aes(x = Año, y = Promedio, colour = Municipio), linewidth =  1, lty=4) +
  labs(x = "", y = "Promedio de IHEH", colour="") +
  scale_colour_manual(values = paleta_colores1) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(margin = margin(l = 10), vjust = 3.5))


ggsave(file.path(dir_Resultados, "Promedio_IHEH_tinigua.png"), width = 20, height =  12, units="cm")
dev.size(units = "cm")


Stat_values%>%
  dplyr::filter(Núcleo == "NDFyB PNN Sierra de La Macarena")%>%
  ggplot() +
  geom_line( mapping=aes(x = Año, y = Promedio, colour = BIOMA_c), linewidth =  1) +
  #  geom_line(data=Stat_values[71:130,], mapping=aes(x = Año, y = Promedio, colour = Municipio), linewidth =  1, lty=4) +
  labs(x = "", y = "Promedio de IHEH", colour="") +
  scale_colour_manual(values = paleta_colores1) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(margin = margin(l = 10), vjust = 3.5))


ggsave(file.path(dir_Resultados, "Promedio_IHEH_Macarena.png"), width = 20, height =  12, units="cm")




## Gráfico sencillo clases ####
Stat_reclass$Categorías <- as.factor(Stat_reclass$`Categorías`)

Stat_reclass0  <-   Stat_reclass

gg <- Stat_reclass0 %>%
  dplyr::filter(Núcleo == "NDFyB PNN Tinigua")%>%
  #dplyr::filter(Núcleo == "NDFyB PNN Sierra de La Macarena")%>%
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
  facet_wrap(~ BIOMA_c, scales = "fixed", ncol=2) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(dir_Resultados, "Porcentaje_Categorias_tinigua.png"), width = 18, height =  14.5, units= "cm")
ggsave(file.path(dir_Resultados, "Porcentaje_Categorias_macarena.png"), width = 18, height =  14.5, units= "cm")

dev.size(units="cm")

# grafica barras area

capas_st0AOI<-capas_st0AOI %>%
  mutate(areas =units::set_units(st_area(.),"km^2"))

capas_st0AOI%>%
  ggplot(aes(x = areas, y = reorder(BIOMA_c,areas)))+
  geom_bar(stat="identity")+
  facet_wrap(vars(Area_PlanC))+
  labs(x="Área (Km^2)",
       y="")+
  theme_bw()+
  theme(axis.text.y= element_text(hjust = 1))

ggsave(file.path(dir_Resultados, "Areas_macarena_Tinigua .jpeg"), width = 23, height =  9, units= "cm")


d <- filter(capas_st0AOI,Area_PlanC=="NDFyB PNN Sierra de La Macarena")

prop.table(d$areas)
