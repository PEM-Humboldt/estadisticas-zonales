# Título: IHEH en la altillanura
#
# Descripción: análisis multitemporal de la huella espacial humana por municipios en la altillanura
#
# Autor(es): Alejandra Narváez Vallejo
#
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

dir_Datos_Or <- file.path("Datos")
dir_Datos_Intm<- file.path ("Intermedios")
dir_Resultados <- file.path("Resultados")

#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(file.path(this.path::this.path(), "..", "..","..","..", "Funciones_comunes" , "estadísticas.R"))
source(file.path(this.path::this.path(), "..", "..","..","..", "Funciones_comunes" , "preprocesamiento.R"))


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
  pattern = "\\d{4}\\.tif$",
  full.names = T
)


raster_interes <- rast(capas_raster[1:5]) %>% setNames(nombres[1:5])


# Capas vector

capas_files <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "CO.shp$",
  full.names = T
)

SisRef <- crs (raster_interes)

# cargar y proyectar en el sistema de referencia definido

capas_st <- lapply(capas_files, CargarProyectar)[[1]] %>% 
  filter(dpto_cnmbr %in% c("CAQUETÁ","GUAVIARE","PUTUMAYO")) # Seleccionar departamentos  de interés


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

capas_st$dpto_cnmbr_ft <- as.factor(capas_st$dpto_cnmbr)

# atributos a usar para rasterizar

atributo_rast <- c("dpto_cnmbr_ft")

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

levels(r_aoi) 


#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

# Se realiza un análisis espacial utilizando cada  departamentos para calcular estadísticas zonales
# del IHEH y sus categorías de intensidad. 
# los resutados se guarda en en dos data frames:
# Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar)
# Stat_reclass: Contiene la frecuencia y porcentaje de categorías de intesisdad de IHEH


# Preparar los insumos para iterar los análisis por departamento 

list_deptos <- capas_st %>% split(., .$dpto_cnmbr)

# Construcción de listas

Stat_reclass <- data.frame()
Stat_values <- data.frame()



for (i in seq_along(list_deptos)) {

  Nombre_dept <- list_deptos[[i]]$dpto_cnmbr
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  r_aoi <- definicionAOI(raster_interes, i)
  r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
  
  ## Calcular estadísticas zonales ####
  ### Para los valores ####
  
  resumen <- terra::global(r_aoi, fun = c("mean", "std"), na.rm = TRUE)
  #resumen_median <- global(r_aoi, fun = mediana_fun)
  
  resumen <- cbind(Nombre_dept, resumen[1], resumen[2])
  
  # Renombrar las columnas para que tengan nombres más descriptivos
  
  names(resumen) <- c("Municipio", "Promedio", "Desviación estandar")
  resumen$Año <- as.numeric(gsub("H", "", row.names(resumen)))
  
  # guardar en Stat_values acumulando en cada iteración
  Stat_values <- rbind (Stat_values, resumen)
  
  ### Para las clases ####
  
  # Calcular la frecuencia de las clases
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass <- group_by(frq_reclass, layer) %>%
    mutate(
      Municipio = Nombre_dept,
      Porcentage = round(prop.table(count) * 100, 3),
      Categorías = factor(value, levels = 1:4, labels = Huella_cat$nom),
      Año = as.numeric(as.character(factor(
        layer,
        levels = 1:5,
        labels = c(1970,1990,2000, 2015, 2018)
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
                             #Mediana,
                             `Desviación estandar`) %>% 
  rename("Departamento"= Municipio)
row.names(Stat_values) <- NULL

Stat_reclass <- dplyr::select(Stat_reclass, Municipio, Año, Categorías, Conteo, Porcentage)%>% 
  rename("Departamento"= Municipio, "Porcentaje" = Porcentage)


# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEH_stats_dept.csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/IHEH_clases_dept.csv"))


# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(3, 4), digits = 2)

t1
# Guardar la tabla en un archivo HTML
saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEH_depto.html"))

t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 5, digits = 2)

t2
saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEH_dpto.html"))

#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####

ggplot(Stat_values) +
  geom_line(aes(x = Año, y = Promedio, colour = Departamento), linewidth =
              1.5) +
  geom_point(aes(x = Año, y = Promedio, colour = Departamento), size=3) +
  labs(x = "", y = "Promedio de IHEH") +
  scale_color_brewer(palette = "Set2", direction = -1) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(margin = margin(l = 10), vjust = 3.5))


ggsave(file.path(dir_Resultados, "Promedio_IHEH_dpto.png"), width = 16.98625, height =  11.66812, units="cm")


Stat_reclass$Categorías <- as.factor(Stat_reclass$`Categorías`)


Stat_reclass0  <-   Stat_reclass

## Gráfico sencillo clases ####

gg <- Stat_reclass0 %>%
  filter(Municipio == "CUMARIBO") %>%
  ggplot(aes(y = Porcentage, x = Año, alluvium = Categorías))

# barras con color , alluvium gris
gg + geom_alluvium(
  color = "black",
  width = 1.5,
  alpha = .2,
  curve_type = "arctangent" ,
  curve_range = 1
) +
  geom_stratum(aes(stratum = Categorías, fill = Categorías), width = 1.5) +
  theme_bw()



# barras blancas , alluvium con color
gg + geom_alluvium(
  aes(fill = Categorías, colour = Categorías),
  width = 1.5,
  alpha = 2 / 3,
  #   decreasing = FALSE,
  curve_type = "arctangent",
  curve_range = 1,
) +
  geom_stratum(aes(stratum = Categorías), #decreasing = FALSE,
               width = 1.5) +
  theme_bw()


## todas las gráficas juntas ####

gg <- Stat_reclass0 %>%
  ggplot(aes(y = Porcentaje, x = Año, alluvium = Categorías))

# barras color , alluvium con color
gg +
  geom_alluvium(
    aes(fill = Categorías, colour = Categorías),
    width = 2,
    alpha = .4,
    curve_type = "arctangent",
    curve_range = 1
  ) +
  geom_stratum(aes(stratum = Categorías
                   , fill = Categorías), #     alpha = .4),
               #decreasing = FALSE,
               width = 2) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(y = "Porcentaje de área", x = "") +
  facet_wrap(~ Departamento, scales = "fixed", nrow = 4) +
  theme_bw() +
  theme(legend.position = "bottom")


ggsave(file.path(dir_Resultados, "Porcentaje_Categorias_dpto.png"), width = 16.98625, height =  15.5, units= "cm")



# barras blancas , alluvium con color
gg + geom_alluvium(
  aes(fill = Categorías, colour = Categorías),
  width = 2,
  alpha = 2 / 3,
  #   decreasing = FALSE,
  curve_type = "arctangent",
  curve_range = 1
) +
  geom_stratum(aes(stratum = Categorías), width = 2) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(y = "Porcentage de área", x = "") +
  facet_wrap( ~ Departamento, scales = "fixed", nrow = 4) +
  theme_bw() +
  theme(legend.position = "bottom")
    
 # mapas ####


# parametros de gráficas

colores <- c("Natural" = "#2ca02c","Bajo" = "#1f77b4","Medio" = "#ff7f0e", "Alto" = "#d62728")

raster_reclass8 <- raster_reclass[[5]]
levels(raster_reclass8) <- Huella_cat

# Gráfico RUNAP
plot(raster_reclass8, col= colores, 
     axes=FALSE, 
     box=TRUE, 
     main= "Intensidad de IHEH 2018"
)

plot(capas_st$geometry, add=T)
plot(Runap, add=T, col= rgb(1, 1, 1, alpha = 0.3))
 

library(ggplot2)

# Convertir raster a data.frame para ggplot
r_df <- as.data.frame(raster_reclass8, xy = TRUE)
colnames(r_df)[3] <- "valor"

# Centroides para etiquetas
centroides <- st_centroid(capas_st)

# Plot
ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = valor)) +
  scale_fill_manual(values = colores, name = "Intensidad IHEH") +
  geom_sf(data = capas_st, fill = NA, color = "black", size = 0.3) +
 # geom_sf_label(data=capas_st,aes(label=dpto_cnmbr), size=2)
  geom_sf_text(data = capas_st, aes(label = dpto_cnmbr),
               size = 2.5, color = "black", fontface = "bold", nudge_x = 60000,nudge_y = -10000) +
  theme_minimal() +
  labs(title = "Intensidad de IHEH 2018", x="", y="") +
  coord_sf()+
  theme(legend.position = "bottom")

ggsave(ggsave(file.path(dir_Resultados, "Intensidad IHEH_2018.png"), width = 16.98625, height =  15.5, units= "cm"))
