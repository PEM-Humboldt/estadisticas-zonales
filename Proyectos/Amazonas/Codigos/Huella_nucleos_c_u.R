# Título: IHEH en la Amazonas
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
capas_raster

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
capas_st<-st_read(grep(capas_files, pattern="Amazonia_proHuella", value=T))
#capas_st <- lapply(capas_files[2], CargarProyectar)[[1]] # problemas con la proyección


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

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm, "/", "AOI", ".tif ")

# Arreglar los campos para rasterizar

capas_st$ID <- as.numeric(capas_st$ID)

# atributos a usar para rasterizar

atributo_rast <- c("ID")
capas_st$nom_simp <- gsub("NDFyB\\s*", "", capas_st$Area_PlanC)
cat_rast <- c("nom_simp")


# Rasterizar capas vectoriales y asignar niveles

if (file.exists(raster_paths)) {
  r_aoi <- rast(raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  r_aoi <- capas_st %>%
    rasterize(y = r_base,
              field = atributo_rast,
              filename = raster_paths)
  
}

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
entidad<-"nom_simp"

list_deptos <- capas_st %>% split(., .[[entidad]])

# Construcción de listas

Stat_reclass <- data.frame()
Stat_values <- data.frame()


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
        levels = 1:6,
        labels = c(1970, 1990, 2000, 2015, 2018, 2019)
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


# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEH_stats.csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/IHEH_clases.csv"))


# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 20 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(3, 5), digits = 2)


# Guardar la tabla en un archivo HTML
saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEH.html"))

t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 20 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 5, digits = 2)

saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEH.html"))

#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####
set.seed()

paleta_colores1 <- sample(colors(), 22)
paleta_colores1 <- distinctColorPalette(22)

paleta_colores1<-c("#DBE2DF", "sienna" ,"#D9B2A1", "#C641E4" ,"#C4EC89" ,"#849F5C", "#6AE5D0" ,"#78708D" ,"black","#72E693" ,"#D2EB48","#E36DC9" ,"#DD5861" ,"#78CAE4" ,"#DFC54B" ,"#E2D899", "#71E954", "#D0BFE2" ,"#8798E3" ,"cyan4", "#D78CB1" ,"purple3")


g<-ggplot(Stat_values) +
  geom_line( mapping=aes(x = Año, y = Promedio, colour = Municipio), linewidth =              1) +
#  geom_line(data=Stat_values[71:130,], mapping=aes(x = Año, y = Promedio, colour = Municipio), linewidth =  1, lty=4) +
  labs(x = "", y = "Promedio de IHEH", colour="") +
  scale_colour_manual(values = paleta_colores1) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(margin = margin(l = 10), vjust = 3.5))

g

library(plotly)
ggplotly(g)

ggsave(file.path(dir_Resultados, "Promedio_IHEH.png"), width = 27, height =  20, units="cm")
dev.size()

Stat_reclass$Categorías <- as.factor(Stat_reclass$`Categorías`)


Stat_reclass0  <-   Stat_reclass

## Gráfico sencillo clases ####

gg <- Stat_reclass0 %>%
  filter(Municipio == "CUMARIBO") %>%
  ggplot(aes(y = Porcentaje, x = Año, alluvium = Categorías))

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

orden <- c( "Villa Catalina" ,"Orotuyo","Llanos del Yarí Yaguará II","El Camuya","Ciudad Yarí" , "Chuapal - Manavires", "Paraíso Amazónico","Miraflores"  ,  "Angoleta","Cueva del Jaguar"  ,"NDF Nueva Ilusión"  , "Solano"      ,   "Charras"  ,"Los Puertos", "Kuway-Nueva York-La Cristalina" , "Agua Bonita"  ,"Mapiripán" ,  "Cuemaní"    , "Mecaya"      ,"PNN Tinigua",   "PNN Sierra de La Macarena" ,"Las Perlas" )
            
Stat_reclass0$Municipio <- factor(Stat_reclass0$Municipio, levels=orden)



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
  facet_wrap(~ Municipio, scales = "fixed", ncol=3) +
  theme_bw() +
  theme(legend.position = "bottom")


ggsave(file.path(dir_Resultados, "Porcentaje_Categorias1.png"), width = 19, height =  30, units= "cm")


dev.size(units="cm")

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
  labs(y = "Porcentaje de área", x = "") +
  facet_wrap( ~ Municipio, scales = "fixed", nrow = 4) +
  theme_bw() +
  theme(legend.position = "bottom")
    

