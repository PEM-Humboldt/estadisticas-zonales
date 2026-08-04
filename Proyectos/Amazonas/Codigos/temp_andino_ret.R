# Título: IHEH en l Andes
#
# Descripción: análisis multitemporal de la huella espacial humana en cada núcleo. Análisis por biomas 
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
library(patchwork)


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
#source(file.path("..", "..", "Funciones_comunes" , "visualización.R"))

#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

nombres <- c( "2018", "2020", "2022")
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
SisRef <- crs (raster_interes)

# Capas vector

capas_files <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "shp$",
  full.names = T
  
)
# cargar y proyectar en el sistema de referencia definido

capas_st <- st_read("C:/Users/alejandra.narvaez/Downloads/Andes/Andes.shp") %>% st_transform(SisRef)
#capas_st <- lapply(grep(capas_files, pattern="AMAZONAS.shp", value=T), CargarProyectar)[[1]] # amazonas

capas_st0 <- st_read(grep(capas_files, pattern="Bioma_clase_corregido_9377.shp", value=T))

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

sf::sf_use_s2(F)

## El indicador  ####

# Recortar al área de estudio general

raster_interes <- crop(raster_interes, capas_st) %>% mask (capas_st)

plot(raster_interes)

capas_st0AOI<-st_intersection(capas_st0, capas_st["NOM_REGIó"])

plot(capas_st0AOI$geometry)


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


## Rasterizar capas vectoriales ####
# se verifica si los raster ya existen , de lo contrario se generan y guardan


# Arreglar los campos para rasterizar

capas_st0AOI$ID <- as.numeric(as.factor(capas_st0AOI$bioma_prel)) #################################### NUCLEOS
capas_st0AOI$BIOMA_c <- as.factor(capas_st0AOI$bioma_prel)

# atributos a usar para rasterizar

atributo_rast <- c("bioma_prel")

cat_rast <- c("bioma_prel")


# Rasterizar capas vectoriales y asignar niveles
# 
# r_aoi <- capas_st0AOI %>%
#   rasterize(y = r_base,
#             field = atributo_rast)

niveles <- unique(capas_st0AOI[c(atributo_rast, cat_rast)] %>% st_drop_geometry())

# levels(r_aoi) <- niveles
# 
# plot(r_aoi)

#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

# Se realiza un análisis espacial utilizando cada  departamentos para calcular estadísticas zonales
# del IHEH y sus categorías de intensidad. 
# los resutados se guarda en en dos data frames:
# Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar)
# Stat_reclass: Contiene la frecuencia y porcentaje de categorías de intesisdad de IHEH


# Preparar los insumos para iterar los análisis por departamento 


entidad<-"bioma_prel"
capas_st0AOI1 <- sf::st_collection_extract(capas_st0AOI, "POLYGON") 

list_deptos <- capas_st0AOI1 %>% split(., .[[entidad]])


# Construcción de listas

Stat_reclass <- data.frame()
Stat_values <- data.frame()

i=8
for (i in seq_along(list_deptos)) {
  
  Nombre_dept <- unique(list_deptos[[i]][[entidad]])
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  r_aoi <- definicionAOI(raster_interes, i)
  r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
  
  ## Calcular estadísticas zonales ####
  ### Para los valores ####
  
  resumen <- terra::global(r_aoi, fun = c("mean", "std"), na.rm = TRUE)
  resumen_median <- global(r_aoi, fun = mediana_fun)
  resumenCell<-global((!is.na(r_aoi[[1]])), sum)
  
  
  
  resumen <- cbind(Nombre_dept,resumenCell, resumen[1], resumen_median, resumen[2])
  
  # Renombrar las columnas para que tengan nombres más descriptivos
  
  names(resumen) <- c("Municipio", "Conteo","Promedio", "Mediana", "Desviación estandar")
  resumen$Año <- as.numeric(row.names(resumen))
  
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
        labels = c(2018,2020, 2022)
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
Stat_values <- Stat_values %>% 
  mutate(Año = recode(Año,
                    `1` = 2018,
                    `2` = 2020,
                    `3` = 2022))

# reorganizar la tabla

## Stat_values ####
Stat_reclass <- dplyr::select(Stat_reclass, Municipio, Año, Categorías, Conteo, Porcentaje)


save(Stat_values,Stat_reclass,file="~/entregables/andes_stats2.R")

# Guardar la información de las estadísticas zonales
library(openxlsx)

write.xlsx(
  list(
    "Stat_reclass" = Stat_reclass,
    "Stat_values"  = Stat_values
  ),
  file = "~/entregables/andes_biomas2.xlsx"
)



# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 20 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(4:6), digits = 2)


# Guardar la tabla en un archivo HTML
#saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEH_andina_bio.html"))


t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 6, digits = 2)

#saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEH__andina_bio.html"))


#****************************************************************************
# Gráficas ####
#****************************************************************************
Stat_values%>%
  #dplyr::filter(Núcleo == "NDFyB PNN Tinigua")%>%
  ggplot() +
  geom_line( mapping=aes(x = Año, y = Promedio, colour = Municipio), linewidth =  1) +
  #  geom_line(data=Stat_values[71:130,], mapping=aes(x = Año, y = Promedio, colour = Municipio), linewidth =  1, lty=4) +
  labs(x = "", y = "Promedio de IHEH", colour="") +
  #scale_colour_manual(values = paleta_colores1) +
  theme_bw() +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))+
  theme(
        axis.title.y = element_text(margin = margin(l = 10), vjust = 3.5))


ggsave( "~/entregables/andes_biomas_promedio_plot2.png", width = 16, height =  14.5, units= "cm")
dev.size(units = "cm")




## Gráfico sencillo clases ####
Stat_reclass0  <-   Stat_reclass

g <- Stat_reclass0 %>%
  filter(Categorías %in% c("Alto", "Muy Alto"),# c("Natural", "Bajo") 
         Año == 2022
  ) %>%
  group_by(Municipio) %>%
  summarise(orden = sum(Porcentaje))

# Orden descendente
orden <- (g %>% arrange(desc(orden)))$Municipio

# Aplicar orden como factor global
Stat_reclass0$Municipio <- factor(Stat_reclass0$Municipio, levels =rev( orden))

Stat_reclass0$Categorías <- as.factor(Stat_reclass$`Categorías`)


gg <- Stat_reclass0 %>%
  #  dplyr::filter(Núcleo == "NDFyB PNN Tinigua")%>%
  #dplyr::filter(Núcleo == "NDFyB PNN Sierra de La Macarena")%>%
  ggplot(aes(y = Porcentaje, x = Año, alluvium = Categorías))
# barras color , alluvium con color
gg +
  geom_alluvium(
    aes(fill = Categorías, colour = Categorías),
    width = 1,
    alpha = .3,
    
    curve_type = "arctangent",
    curve_range = 1
  ) +
  geom_stratum(aes(stratum = Categorías
                   , fill = Categorías), #     alpha = .4),
               #decreasing = FALSE,
               width = 1,
               linewidth=.2,
               color="grey30") +
  
  
  scale_x_continuous(breaks = c(2018, 2020, 2022))+
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  scale_colour_brewer(palette = "RdYlGn", direction = -1) +
  labs(y = "Porcentaje de área", x = "") +
  facet_wrap(~ Municipio, scales = "fixed", ncol=2) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave( "~/entregables/andes_biomas_plot.png", width = 16, height =  14.5, units= "cm")


dev.size(units="cm")

# Gráfica definitiva#######################################################################

Stat_reclass22  <-   Stat_reclass %>%  
  filter( Año == 2022)

g <- Stat_reclass22 %>%
  filter(Categorías %in%  c("Natural", "Bajo") 
  
  ) %>%
  group_by(Municipio) %>%
  summarise(orden = sum(Porcentaje))

# Orden descendente
orden <- (g %>% arrange(desc(orden)))$Municipio

# Aplicar orden como factor global
Stat_reclass22$Municipio <- factor(Stat_reclass22$Municipio, levels =rev( orden))

Stat_reclass22$Categorías <- as.factor(Stat_reclass22$`Categorías`)



  # Paleta (muy parecida a la de tu gráfico)
  colores <- c(
    "Natural" = "#3E8E41",
    "Bajo" = "#5B8FA8",
    "Medio" = "#E6B800",
    "Alto" = "#F07F00",
    "Muy Alto" = "#D92525"
  )

# -------- PANEL PORCENTAJE --------
p1 <- Stat_reclass22 %>%
  ggplot(aes(x = Porcentaje, y = Municipio, fill = Categorías)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores) +
  labs(x = "Porcentaje", y = "") +
 # theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.margin = margin(5, 0, 5, 0)
  )

# -------- PANEL ÁREA --------
p2 <- Stat_reclass22 %>%
  ggplot(aes(x = Conteo/100, y = Municipio, fill = Categorías)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores) +
  labs(x = "km2", y = "") +
 # theme_minimal() +
  theme(
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank() ,
    legend.position = "none",
    plot.margin = margin(5, 0, 5, 0)
  )

# -------- UNIR LOS DOS PANELES --------
library(patchwork)
  p1 + p2 +
    plot_layout(guides = "collect") &   # 👈 leyenda compartida
    theme(
      legend.position = "bottom"
    )
  
  
  
ggsave( "~/entregables/andes_biomas_plot2.png", width = 19, height =  11, units= "cm")


dev.size(units="cm")

