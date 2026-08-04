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

nombres <- c( "2024", "2025")
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
  pattern = "IHEH_IAVHA.*\\.tif$",
  full.names = T
)

raster_interes <- rast(capas_raster[c(2,1)]) %>% setNames(nombres)


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
      Categorías = factor(value, levels = 1:5, labels = Huella_cat$nom),
      Año = as.numeric(as.character(factor(
        layer,
        levels = 1:2,
        labels = c(2024, 2025)
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

Stat_reclass <- dplyr::select(Stat_reclass, Municipio, Año, Categorías, Conteo, Porcentaje)


# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEH_sinchi_stats.csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/IHEH_sinchi_clases.csv"))


# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 20 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(3,4, 5), digits = 2)

t1


# Guardar la tabla en un archivo HTML
saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEHSinchi.html"))

t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 20 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 5, digits = 2)

t2
saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEHSinchi.html"))

#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####
set.seed()

paleta_colores1 <- sample(colors(), 22)
paleta_colores1 <- distinctColorPalette(22)

paleta_colores1<-c("#DBE2DF", "sienna" ,"#D9B2A1", "#C641E4" ,"#C4EC89" ,"#849F5C", "#6AE5D0" ,"#78708D" ,"black","#72E693" ,"#D2EB48","#E36DC9" ,"#DD5861" ,"#78CAE4" ,"#DFC54B" ,"#E2D899", "#71E954", "#D0BFE2" ,"#8798E3" ,"cyan4", "#D78CB1" ,"purple3")


g<-ggplot(Stat_values) +
  geom_line( mapping=aes(x = Año, y = Promedio, colour = Núcleo), linewidth =              1) +
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


## todas las gráficas juntas ####


# Colores para huella


colores <- c(
  "Natural" = "#3a9243",   # 👈 verde más natural y menos saturado
  "Bajo" = "#5f9bbd",
  "Medio" = "#e6b800",
  "Alto" = "#ec8200",
  "Muy Alto" = "#d62728"
)

g <- Stat_reclass %>% filter(Categorías %in% c("Natural", "Bajo"), Año == 2025) %>% group_by(Municipio) %>% summarise(orden=sum(Porcentaje))

orden <- (g %>% arrange(desc(orden)))$Municipio

Stat_reclass0$Municipio <- factor(Stat_reclass0$Municipio, levels=orden)

write.csv2(Stat_reclass0, file.path(dir_Resultados, "Nucleos_sinchi.csv"))



i <- Stat_reclass0 %>%
  ggplot(aes(x = Porcentaje, y = as.factor(Año), fill = Categorías)) +
  geom_bar(stat = "identity") +
  facet_grid(vars(Municipio)) +
  scale_fill_manual(values = colores) + 
  labs(
    y = "",
    fill = "Intensidad \n de IHEH",
    x = "Porcentaje del área"
  ) +
  theme(
    legend.position = "bottom",
    strip.text.y = element_text(angle = 0,size = 8),
    axis.text = element_text(size = 6),
    axis.title.x = element_text(size = 8),
    
    # 🔥 quitar fondo
    panel.background = element_blank(),
    plot.background = element_blank(),
   # strip.background = element_blank(),
    
    # opcional: quitar grilla
    #panel.grid = element_blank()
  )

i1 <- i+
  theme(
    plot.margin = margin(t = 5, r = 0, b = 5, l = 1)
  )

i1

dev.size(units="cm")

dif_promedio <- Stat_values %>%
  group_by(Núcleo) %>%
  summarise(
    diferencia_25_24 = Promedio[Año == 2025] - Promedio[Año == 2024]
  ) 

dif_promedio$Núcleo <- factor(dif_promedio$Núcleo, levels=rev(orden))
  
  
ggplot(dif_promedio)+
  geom_bar(aes(x=diferencia_25_24, y= Núcleo),stat="identity")

d <- ggplot(dif_promedio) +
  geom_bar(
    aes(x = diferencia_25_24, 
        y = Núcleo,
        fill = diferencia_25_24 > 0),
    stat = "identity",
    width= .6
  )+ 
  labs(x="Δ Promedio 2025-2024",
       y="")+
  scale_fill_manual(
    values = c("TRUE" = "#c44e52", "FALSE" = "#4c72b0"),
    name = "Δ en la \nintensidad \ndel IHEH",
    labels = c("FALSE" = "Disminución", "TRUE" = "Aumento")
  )+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 8))
  #theme_light()
  #theme_void() 
d

d1 <- d +
  theme(
    plot.margin = margin(t = 5, r = 1, b = 5, l = 1)
  )



figura <- (i1 + d1) +
  
  # Recolectar leyendas en una sola
  plot_layout(guides = "collect", widths = c(2, 1)) &
  
  
  
  # Dividir leyenda en dos filas
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) &
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)
  )


ggsave(
  figura,
  filename = paste0("Resultados/iheh_nucleos_26_1.png"),
  width = 17,
  height = 17,
  units = "cm"
)
ggsave(figura,filename="Resultados/iheh_nucleos_26.png" )

getwd()
