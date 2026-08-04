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

capas_st0 <- st_read(grep(capas_files, pattern="Bioma_clase_corregido_9377", value=T)) # problemas con la proyección

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

sf::sf_use_s2(F)

## El indicador  ####

# Recortar al área de estudio general

raster_interes <- crop(raster_interes, capas_st) %>% mask (capas_st)

plot(raster_interes)

capas_st0AOI<-st_intersection(capas_st0, capas_st["Area_PlanC"])

plot(capas_st0AOI)

st_write(capas_st0AOI,paste0("Resultados/NDF_bioma.shp") )
st_write(capas_st0AOI,paste0("Resultados/NDF_bioma.gpkg") )



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
#capas_st$ID <- as.numeric(capas_st$OBJECTID) #################################### AMAZONAS
capas_st0AOI$IDG <- paste(capas_st0AOI$Area_PlanC, capas_st0AOI$ID, sep = "_")

# atributos a usar para rasterizar

atributo_rast <- c("ID")

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
  resumenCell<-global((!is.na(r_aoi[[1]])), sum)
  
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
                             bioma_prel,
                             Bioma,
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
                              bioma_prel, 
                              Bioma,
                              Año, 
                              Categorías,
                              Conteo, 
                              Porcentaje)


# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_values, paste0(dir_Resultados, "/IHEHsinchi_stats_NDFyBiomal.csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados, "/IHEHsinchi_clases_NDFyBioma.csv"))


# Elaborar tablas dinámicas
t1 <- datatable(Stat_values,
                options = list(
                  pageLength = 20 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = c(4:6), digits = 2)


# Guardar la tabla en un archivo HTML
saveWidget(t1, file = file.path(dir_Resultados, "Estadísticas_IHEHsinchi_NDFyBioma.html"))


t2 <- datatable(Stat_reclass,
                options = list(
                  pageLength = 10 ,
                  paging = T,
                  language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ entradas")
                )) %>%
  formatRound(columns = 6, digits = 2)

saveWidget(t2, file = file.path(dir_Resultados, "EstadísticasClases_IHEHsinchi__NDFyBioma.html"))


#****************************************************************************
# Gráficas ####
#****************************************************************************

# =============================== =
# 🔹 Obtener lista de biomas únicos
# =============================== =

Stat_reclass0 <- Stat_reclass

Stat_reclass0$nom_simp <- gsub("NDFyB\\s*", "", Stat_reclass0$Núcleo)

biomas <- unique(Stat_reclass$bioma_prel)

# Seleccionar un bioma específico (ejemplo)
bioma = biomas[5]


# ======================================== =
# 🔹 Calcular diferencia de promedios (Δ)
#    entre 2025 y 2024 por núcleo y bioma
# ======================================== =
dif_promedio <- Stat_values %>%
  group_by(Núcleo, bioma_prel) %>%
  summarise(
    diferencia_25_24 = Promedio[Año == 2025] - Promedio[Año == 2024]
  )

dif_promedio$nom_simp <- gsub("NDFyB\\s*", "", dif_promedio$Núcleo)

# ========================================================= = 
# 🔹 Función para graficar composición + cambio por bioma
# ========================================================= =
composicionClases_promedios <- function(bioma, altura = 14) {
  
  # ------------------------------------------------------- -
  # 🔹 Definir orden de núcleos
  #    basado en la suma de Natural + Bajo (2025)
  # ------------------------------------------------------- -
  g <- Stat_reclass0 %>%
    filter(Categorías %in% c("Natural", "Bajo"),
           Año == 2025,
           bioma_prel == bioma) %>%
    group_by(nom_simp) %>%
    summarise(orden = sum(Porcentaje))
  
  # Orden descendente
  orden <- (g %>% arrange(desc(orden)))$nom_simp
  
  # Aplicar orden como factor global
  Stat_reclass0$nom_simp <- factor(Stat_reclass0$nom_simp, levels = orden)
  
  
  # ------------------------------------------------------- -
  # 🔹 Gráfico i: composición de clases (barras apiladas)
  # ------------------------------------------------------- -
  i <- Stat_reclass0 %>%
    filter(bioma_prel %in% bioma) %>%
    ggplot(aes(x = Porcentaje, y = as.factor(Año), fill = Categorías)) +
    geom_bar(stat = "identity") +
    facet_grid(vars(nom_simp)) +
    
    # Paleta de colores definida previamente
    scale_fill_manual(values = colores) +
    
    labs(
      y = "",
      fill = "Intensidad \n de IHEH",
      x = "Porcentaje del área"
    ) +
    
    # Estética general
    theme(
      legend.position = "bottom",
      strip.text.y = element_text(angle = 0, size = 7),
      axis.text = element_text(size = 5),
      axis.title.x = element_text(size = 8),
      panel.spacing = unit(0.2, "lines"),  
      
      # Quitar fondos
      panel.background = element_blank(),
      plot.background = element_blank()
    )
  
  i
  # ------------------------------------------------------- -
  # 🔹 Filtrar diferencias para el bioma actual
  # ------------------------------------------------------- -
  filtrado_bioma <- dif_promedio %>%
    filter(bioma_prel == bioma)
  
  # Orden inverso para alinear con gráfico i
  filtrado_bioma$nom_simp <- factor(filtrado_bioma$nom_simp, levels = rev(orden))
  
  
  # ------------------------------------------------------- -
  # 🔹 Gráfico d: diferencia (Δ) 2025-2024
  # ------------------------------------------------------- -
  d <- filtrado_bioma %>%
    ggplot() +
    geom_bar(
      aes(
        x = diferencia_25_24,
        y = nom_simp,
        fill = diferencia_25_24 > 0  # TRUE = aumento, FALSE = disminución
      ),
      stat = "identity",
      width = .6
    ) +
    
    # Escala fija para comparar entre biomas
    scale_x_continuous(limits = c(-1.9, 1.6)) +
    
    labs(
      x = "Δ Promedio 2025-2024",
      y = ""
    ) +
    
    # Colores sobrios para cambio
    scale_fill_manual(
      values = c("TRUE" = "#c44e52", "FALSE" = "#4c72b0"),
      name = "Δ en la \nintensidad \ndel IHEH",
      labels = c("FALSE" = "Disminuye", "TRUE" = "Aumenta")
    ) +
    
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.text = element_text(size = 6),
      axis.title.x = element_text(size = 8)
    )
  
  
  # ------------------------------------------------------- -
  # 🔹 Ajuste de márgenes (alineación entre gráficos)
  # ------------------------------------------------------- -
  i1 <- i + theme(plot.margin = margin(t = 5, r = 0, b = 5, l = 1))
  d1 <- d + theme(plot.margin = margin(t = 5, r = 1, b = 5, l = 1))
  
  
  # ------------------------------------------------------- -
  # 🔹 Combinar gráficos con patchwork
  # ------------------------------------------------------- -
  figura <- (i1 + d1) +
    
    # Recolectar leyendas en una sola
    plot_layout(guides = "collect", widths = c(2, 1)) &
    
    # Título dinámico
    plot_annotation(title = bioma) &
    
    # Dividir leyenda en dos filas
    #guides(fill = guide_legend(nrow = 2, byrow = TRUE)) &
    
    theme(
      
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      title = element_text( size=8)
    
    )

  figura
  # ------------------------------------------------------- -
  # 🔹 Guardar figura
  # ------------------------------------------------------- -
  ggsave(
    figura,
    filename = paste0("Resultados/grafica_iheh_nucleos_26_", bioma, "2.png"),
    width = 17,
    height = altura,
    units = "cm"
  )
  
  # Retornar figura
  figura
}


# ========================================================= =
# 🔹 Aplicar la función a todos los biomas
# ========================================================= =
lapply(biomas, composicionClases_promedios)


# 🔹 Ejecuciones específicas con tamaños personalizados
composicionClases_promedios(biomas[2], 13)

composicionClases_promedios(biomas[3], 5.8)
composicionClases_promedios(biomas[4], 4.5)



# Ver tamaño del dispositivo gráfico
dev.size("cm")

#****************************************************************************
# Estadísticas generales ####
#****************************************************************************

Stat_values <- read_csv2(paste0(dir_Resultados, "/IHEHsinchi_stats_NDFyBiomal.csv"))
Stat_reclass <- read_csv2(paste0(dir_Resultados, "/IHEHsinchi_clases_NDFyBiomal.csv"))


library(dplyr)
library(forcats)

box <- Stat_values %>% 
  filter(Año==2025) %>% 
  mutate(bioma_prel = fct_reorder(bioma_prel, Promedio, .fun = median)) %>% 
  ggplot()+
  geom_boxplot(aes(x=Promedio,y=bioma_prel), outlier.shape = NA)+
  geom_jitter(aes(x=Promedio,y=bioma_prel), alpha =0.3, size=.8, height = .2)+
  labs(
    y = "",
    x = "Promedio de IHEH"
  ) +
  theme_bw()
  
box

bp <- 
  boxplot(Promedio~ BIOMA_c,Stat_values )
bp


tvp <- Stat_values %>% 
  group_by(bioma_prel, Año) %>% 
  summarise(mean(Promedio)
            , median(Promedio))
write.csv2(tvp,paste0("Resultados/ihehsinchi_nucleosstats_biomasall1.csv"))
bp
dev.size("cm")
ggsave(
  box,
  filename = paste0("Resultados/grafica_ihehsinchi_nucleos_biomasall1.png"),
  width = 15,
  height = 6,
  units = "cm"
)

unlink(tempfile())
