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

## El análisis aun mantiene el sistemas de referenci antiguo


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
here::i_am("Código/Huella_Altillanura.R")

dir_Datos_Or <- here::here("Datos", "Originales")
dir_Datos_Intm <- here::here("Datos", "Intermedios")
dir_Resultados <- here::here("Resultados")

#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(here::here("..", "..", "Funciones_comunes" , "estadísticas.R"))
source(here::here("..", "..", "Funciones_comunes" , "preprocesamiento.R"))

#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

nombres <- c("H1970", "H1990", "H2000", "H2015", "H2018")
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

capas_st <- lapply(capas_files[2], CargarProyectar)[[1]]


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

capas_st$COD_MUNICI <- as.numeric(capas_st$COD_MUNICI)

# atributos a usar para rasterizar

atributo_rast <- c("COD_MUNICI")

cat_rast <- c("NOMBRE_ENT")


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

list_deptos <- capas_st %>% split(., .$NOMBRE_ENT)

# Construcción de listas

Stat_reclass <- data.frame()
Stat_values <- data.frame()



for (i in seq_along(list_deptos)) {

  Nombre_dept <- list_deptos[[i]]$NOMBRE_ENT
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
      Porcentage = round(prop.table(count) * 100, 3),
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

Stat_reclass <- dplyr::select(Stat_reclass, Municipio, Año, Categorías, Conteo, Porcentage)


# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEH_stats.csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/IHEH_clases.csv"))


# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(3, 5), digits = 2)


# Guardar la tabla en un archivo HTML
saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEH.html"))

t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 5, digits = 2)

saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEH.html"))

#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####

ggplot(Stat_values) +
  geom_line(aes(x = Año, y = Promedio, colour = Municipio), linewidth =
              1.5) +
  labs(x = "", y = "Promedio de IHEH") +
  scale_color_brewer(palette = "Set2", direction = -1) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(margin = margin(l = 10), vjust = 3.5))


ggsave(file.path(dir_Resultados, "Promedio_IHEH.png"), width = 16.98625, height =  11.66812, units="cm")


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
  ggplot(aes(y = Porcentage, x = Año, alluvium = Categorías))

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
  labs(y = "Porcentage de área", x = "") +
  facet_wrap(~ Municipio, scales = "fixed", nrow = 4) +
  theme_bw() +
  theme(legend.position = "bottom")


ggsave(file.path(dir_Resultados, "Porcentage_Categorias1.png"), width = 16.98625, height =  15.5, units= "cm")



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
  facet_wrap( ~ Municipio, scales = "fixed", nrow = 4) +
  theme_bw() +
  theme(legend.position = "bottom")
    

