# Título: IHEH en la altillanura
#
# Autor(es): Alejandra Narváez Vallejo
# Descripción: análisis multitemporal de la huella espacial humana por municipios /departamento o bioma
##
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

dir_Datos_Or <- file.path("Datos/Originales")
dir_Datos_Intm<- file.path ("Datos/Intermedios")
dir_Resultados <- file.path("Resultados")

#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(file.path(this.path::this.path(), "..", "..","..","..", "Funciones_comunes" , "estadísticas.R"))
source(file.path(this.path::this.path(), "..", "..","..","..", "Funciones_comunes" , "preprocesamiento.R"))


#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

Años <- c("2018", "2020", "2022")
Años_numero <- 3

Huella_cat <- data.frame(Cat = 1:5,
                         nom = c("Natural", "Bajo", "Medio", "Alto", "Muy Alto"))

Unidad_analsis <- "Municipio" # Departamento" Municipio, Bioma


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas raster

capas_raster <- list.files(
  dir_Datos_Or,
  recursive = T,
  pattern = "\\d{5}\\.tif$",
  full.names = T
)


raster_interes <- rast(capas_raster) %>% setNames(Años)
SisRef <- crs (raster_interes)

# Capas vector

# Municipio
ruta_archivo <- file.path(dir_Datos_Or,
                          "MUNICIPIOS/Carto100000_Colombia_DI_2022_gpkg/Carto100000_Colombia_DI_2022.gpkg")
st_layers(ruta_archivo)

# cargar y proyectar en el sistema de referencia definido
capas_st <- st_read(ruta_archivo,layer="Limite_Municipal_Poligono") %>% st_transform(SisRef)

# Departamento
capas_st <- st_read(file.path(dir_Datos_Or ,"MGN2023_DPTO_POLITICO/MGN_ADM_DPTO_POLITICO.shp")) %>% st_transform(SisRef)

# Bioma 
capas_st <- st_read("~/GitHub/huella-humana-analisis/Datos/EcosistemasPotencialesDeColombia.gdb") %>% st_transform(SisRef)


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## El indicador  ####

plot(raster_interes)

# Clasificar la huella por sus rangos de intensidad 

## matriz de reclasificación

rc_matrix <- matrix(c(-1, 0, 1,
                      0, 15, 2,
                      15, 30, 3,
                      30, 50, 4,
                      50,100, 5), 
                    ncol = 3, byrow = TRUE)

raster_reclass <- classify(raster_interes, rc_matrix)

# Convertir a factor y asignar etiquetas
#levels(raster_reclass) <- Huella_cat


## capa base ####

r_base <- raster_interes[[1]]

## Rasterizar capas vectoriales ####
# se verifica si los raster ya existen , de lo contrario se generan y guardan

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm, "/", "COL_RATER9377_",Unidad_analsis, ".tif ")

# Arreglar los campos para rasterizar: revisar comentarios de las siguientes lineas para ver cual aributo definir

#atributo_rast <- c("dpto_cnmbr") # Departamento
#atributo_rast <- c("Bioma") # Bioma
atributo_rast <- c("MpCodigo") # para municipio


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

levels(r_aoi) 
plot(r_aoi)

#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

# Se realiza un análisis espacial utilizando cada  departamentos para calcular estadísticas zonales
# del IHEH y sus categorías de intensidad. 
# los resutados se guarda en en dos data frames:
# Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar, Máximo y mínimo)
# Stat_reclass: Contiene la frecuencia y porcentaje de categorías de intesisdad de IHEH

## Estadísticas Zonales ####

zonalTabla <- function(estadistico){
  zonal(raster_interes, r_aoi, fun = estadistico, na.rm=T) %>%
    tidyr::pivot_longer(
      cols = c(`2018`, `2020`, `2022`),   # columnas de años
      names_to = "Año",
      values_to = estadistico
    )
  
}


z_mean   <- zonalTabla("mean")
z_sd     <- zonalTabla("sd")
z_median <- zonalTabla("median")
z_min <- zonalTabla("min")
z_max <- zonalTabla("max")

Stat_values <- Reduce(function(x, y) merge(x, y, by =c(atributo_rast, "Año")),
                      list(z_min,z_mean,z_median,z_max,z_sd))


capas_st <- capas_st%>% 
  mutate(area_cal_km2=units::drop_units(units::set_units(st_area(.),"km2"))) %>% 
  .[c(1,2,5,6,8)]# revisar que campos son de interes use 1,2,5,6 para municicpio, 2,4,6, bioma


Stat_values <- merge(capas_st,Stat_values, by = atributo_rast)



# Preparar los insumos para iterar los análisis por departamento 

list_deptos <- capas_st %>% split(.[[atributo_rast]])
#list_deptos [[c(-1088,-1089)]] quitar si municipios
list_deptos [[4]] #quitar si dpto
quitar <- c(1088,1089) # si dpto , no aplica para biomas


seq_along(list_deptos)[-quitar]
## Frecuencias de las categorías ####
# Construcción de listas

Stat_reclass <- data.frame()
i=5

for (i in seq_along(list_deptos)[- quitar]) { # La indexación quinta las islas San Andrés y Providencia
#for (i in seq_along(list_deptos)) { # si biomas
  
  
  Nombre_dept <-unique( list_deptos[[i]][[atributo_rast]])
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
   r_aoi_reclass <- definicionAOI(raster_reclass, i)
  
 
  ### Para las clases ####
  
  # Calcular la frecuencia de las clases
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass <- group_by(frq_reclass, layer) %>%
    mutate(
      Cod_Zona = Nombre_dept,
      Porcentage = round(prop.table(count) * 100, 3),
      Categorías = factor(value, levels = 1:5, labels = Huella_cat$nom),
      Año = as.numeric(as.character(factor(
        layer,
        levels = 1:Años_numero,
        labels = Años
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

Stat_values <-  Stat_values %>% st_drop_geometry()


Stat_reclass0 <- Stat_reclass %>% st_drop_geometry() %>% 
  dplyr::select( Cod_Zona, Año, Categorías, Conteo, Porcentage)%>% 
  rename(!!atributo_rast := Cod_Zona, "Porcentaje" = Porcentage)


Stat_reclass <- merge(
  st_drop_geometry(capas_st),
  Stat_reclass0,
  by = atributo_rast,
  all.y = TRUE
)

# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEHcorine_stats_", Unidad_analsis,".csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/IHEHcorine_clases_",Unidad_analsis,".csv"))


# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 30 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(5, 7:11), digits = 2)

t1
# Guardar la tabla en un archivo HTML
saveWidget(t1, file = file.path(dir_Resultados, paste0( "Estadísticas_IHEHcorine_", Unidad_analsis,".html")))

t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 30 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(5,9), digits = 2)

t2
saveWidget(t2, file = file.path(dir_Resultados,paste0( "EstadísticasClases_IHEHcorine_", Unidad_analsis,".html")))

#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####

ggplot(Stat_values) +
  geom_line(aes(x = Año, y = mean, colour = !!Unidad_analsis), linewidth =
              1.5)
+
  geom_point(aes(x = Año, y = mean, colour = Unidad_analsis), size=3) +
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
