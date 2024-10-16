# Título: Integridad del bosque en la Amazonas
#
# Descripción: análisis una capa de integridad 2019_ en núcleos base. Análisis por bioma.
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

capas_st<-st_read(grep(capas_files, pattern="Amazonia9377", value=T))
st_crs(capas_st)<-9377

capas_st0 <- lapply(grep(capas_files, pattern="Bioma_clase", value=T), CargarProyectar)[[1]] # problemas con la proyección
st_crs(capas_st0)<-9377

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

sf::sf_use_s2(F)

# Elegir núcleos de interés

capas_st<-filter(capas_st, Area_PlanC %in% c( "NDFyB PNN Sierra de La Macarena" , "NDFyB PNN Tinigua" ))

# Reclasificar

raster_reclass <- classify(raster_interes, reclass_mat, include.lowest =FALSE)%>%
  crop( capas_st)

capas_st0AOI<-st_intersection(capas_st0, capas_st["Area_PlanC"])

plot(raster_interes)


## capa base ####

r_base <- raster_reclass[[1]]
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

i=1
for (i in seq_along(list_deptos)) {
  
  Nombre_dept <- list_deptos[[i]][[entidad]]
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
  
  ## Calcular estadísticas zonales ####
  ### Para los valores ####
  
  ### Para las clases ####
  
  # Calcular la frecuencia de las clases
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass <- group_by(frq_reclass, layer) %>%
    mutate(
      Municipio = Nombre_dept,
      Porcentaje = round(prop.table(count) * 100, 3),
      Categorías = factor(value, levels = 1:3, labels = Integridad_cat$nom),
      Año = 2019    )%>%
    rename(Conteo = count)
  
  
  # guardar en Stat_reclass acumulando en cada iteración
  Stat_reclass <- rbind (Stat_reclass, tem_Stat_reclass) %>%
    ungroup()
  
}


#****************************************************************************
# Organizar las tablas y guardarlas ####
#****************************************************************************
library(stringr)
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

write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/Integridad_NDFyBsel.csv"))


# Elaborar tablas dinámicas

t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 6, digits = 2)

t2
saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_Integridad__NDFyBsel.html"))

#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####

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


## Gráfico sencillo clases ####

## todas las gráficas juntas ####


gg <- Stat_reclass0 %>%
  ggplot(aes(x = Porcentaje, y = BIOMA_c, fill= Categorías))+
  geom_bar(stat="identity")+
  facet_wrap(vars(Núcleo))+
  scale_fill_manual(values = c("Baja" = "red3", "Media" = "orange", "Alta" = "darkgreen"),
                    guide = guide_legend(reverse = TRUE))+
  labs(x="Porcentaje de bosque potencial",
       y="", fill= "Integridad")+
  theme_bw()+
  theme(axis.text.y= element_text(hjust = 1), legend.position =  "bottom")

gg

ggsave(file.path(dir_Resultados, "PorcentajeNucleos_Bioma.png"))

gg

gg <- Stat_reclass0 %>%
  mutate(Núcleo = case_when(
    Núcleo == "NDFyB PNN Sierra de La Macarena" ~ "PNN Sierra de La Macarena",
    Núcleo == "NDFyB PNN Tinigua" ~ "PNN Tinigua",
    TRUE ~ Núcleo
  ))%>%
  ggplot(aes(x = Conteo*310*310/1000000, y = reorder(BIOMA_c,Conteo)))+
  geom_bar(stat="identity")+
  facet_wrap(vars(Núcleo))+
  scale_fill_manual(values = c("Baja" = "red3", "Media" = "orange", "Alta" = "darkgreen"),
                    guide = guide_legend(reverse = TRUE))+
  labs(x="Bosque potencial (Km^2)",
       y="", fill= "Integridad")+
  theme_bw()+
  theme(axis.text.y= element_text(hjust = 1), legend.position =  "bottom")

gg

ggsave(file.path(dir_Resultados, "AreasNucleos_Bioma.png"))
