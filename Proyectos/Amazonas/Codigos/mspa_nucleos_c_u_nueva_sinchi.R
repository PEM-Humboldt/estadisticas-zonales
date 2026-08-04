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

nombres <- c( "I_2024","II_2024","IV_2024","I_2025","II_2025", "III_2025","IV_2025")
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
  pattern = "naCor.*\\.tif$",
  full.names = T
)

raster_interes <- rast(capas_raster[c(1,3,6,2,4,5,7)]) %>% setNames(nombres)


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

## reduccion/reclasificacion mspa ####
rcl <- matrix(c(
  # Core
  17, 1,
  117,1,
  
  # Islet
  9, 2,
  109,2,
  
  # Perforation
  5, 3,
  105,3,
  
  # Edge
  3, 4,
  103,4,
  
  # Loop
  65,5,
  165,5,
  67,5,
  167,5,
  69,5,
  169,5,
  
  # Bridge
  33,6,
  133,6,
  35,6,
  135,6,
  37,6,
  137,6,
  
  # Branch
  1, 7,
  101,7,
  
  
  # Background 
  0, 8,
  100,8,
  220,8,
  
  
  #  NA
  129, NA
  
), ncol = 2, byrow = TRUE)

r_mspa_clases <- classify(raster_interes, rcl)

niveles <- data.frame(
  value = 1:8,
  class = c("Núcleo", "Isla", "Perforación", "Borde",
            "Bucle", "Puente", "Rama", "No Bosque")
)



for(i in 1:nlyr(r_mspa_clases)){
  levels(r_mspa_clases[[i]]) <- niveles
}

plot(r_mspa_clases)

## capa base ####

r_base <- raster_interes[[1]]

plot(r_base)
## Rasterizar capas vectoriales ####
# se verifica si los raster ya existen , de lo contrario se generan y guardan

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm, "/", "AOI_10_base_mspa", ".tif ")

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



for (i in seq_along(list_deptos)) {
  
  Nombre_dept <- list_deptos[[i]][[entidad]]
  print(Nombre_dept)
  
  # limitar las huellas al area de estudio 
  
  r_aoi_reclass <- definicionAOI(r_mspa_clases, i)
  
  
  ## Calcular estadísticas zonales ####
  
  ### Para las clases ####
  
  # Calcular la frecuencia de las clases
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass <- group_by(frq_reclass, layer) %>%
    mutate(
      Municipio = Nombre_dept,
      Porcentaje = round(prop.table(count) * 100, 3),
      fecha = factor(
        layer,
        levels = 1:7,
        labels = nombres
      )
    ) %>%
    rename(Categorías= value,
           Conteo = count)
  
  
  # guardar en Stat_reclass acumulando en cada iteración
  Stat_reclass <- rbind (Stat_reclass, tem_Stat_reclass) %>%
    ungroup()
  
}

#****************************************************************************
# Organizar las tablas y guardarlas ####
#****************************************************************************


Stat_reclass1 <- dplyr::select(Stat_reclass, Municipio, fecha, Categorías, Conteo, Porcentaje)%>%
  rename("Núcleo"=Municipio)


# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_reclass1, paste0(dir_Resultados, "/IHEH_sinchi_mspa.csv"))
save(Stat_reclass1, file=paste0(dir_Resultados, "/Statreclass_mspa"))


#****************************************************************************
# Gráficas ####
#****************************************************************************

## Gráfica de valores ####

Stat_reclass1$Categorías <- as.factor(Stat_reclass1$`Categorías`)


Stat_reclass0  <-   Stat_reclass1


Stat_reclass0 <- Stat_reclass0 %>%
  mutate(Categorías = recode(Categorías,
                             "No Natural" = "No Bosque"))



Stat_reclass0 <- Stat_reclass0 %>%
  mutate(fecha1 = recode(fecha,
                             "I_2024" = "Ene 2024",
                         "IV_2024" = "Oct 2024",
                         "III_2025" = "Jul 2025"))



## todas las gráficas juntas ####


# Colores para huella



colores_mspa <- c(
  "Núcleo" = "#00C800",
  "Isla" = "#A03C00",
  "Perforación" = "#0000FF",
  "Borde" = "#000000",
  "Bucle" = "#FFFF00",
  "Puente" = "#FF0000",
  "Rama" = "#FF8C00",
  "No Bosque"="grey"
  
)

g <- Stat_reclass0 %>% filter(Categorías %in% c("Núcleo"), fecha == "III_2025") %>% group_by(Núcleo) %>% summarise(orden=sum(Porcentaje))

orden <- (g %>% arrange(desc(orden)))$Núcleo

Stat_reclass0$Núcleo <- factor(Stat_reclass0$Núcleo, levels=orden)





i <-

  Stat_reclass0 %>% 
    filter(fecha %in% c("I_2024", "IV_2024", "III_2025")) %>% 
  ggplot(aes(x = Porcentaje, y = as.factor(fecha1), fill = Categorías)) +
  geom_bar(stat = "identity") +
  facet_grid(vars(Núcleo)) +
  scale_fill_manual(values = colores_mspa) + 
  labs(
    y = "",
    fill = "",
    x = "Porcentaje del área"
  ) +
  theme(
    legend.position = "bottom",
    strip.text.y = element_text(angle = 0,size = 8),
    axis.text = element_text(size = 6),
    axis.text.y = element_text(size = 5),
    axis.title.x = element_text(size = 8),
    
    # 🔥 quitar fondo
    panel.background = element_blank(),
    plot.background = element_blank(),
    # strip.background = element_blank(),
    
    # opcional: quitar grilla
    #panel.grid = element_blank()
  )

i
i1 <- i+
  theme(
    plot.margin = margin(t = 5, r = 0, b = 5, l = 1)
  )

i1

dev.size(units="cm")

dif_promedio <- Stat_reclass0 %>%
  filter(Categorías=="Núcleo") %>% 
  group_by(Núcleo) %>%
  reframe(
    diferencia_25_24 = Porcentaje[fecha == "III_2025"] - Porcentaje[fecha == "I_2024"]
  ) 

dif_promedio$Núcleo <- factor(dif_promedio$Núcleo, levels=rev(orden))


ggplot(dif_promedio)+
  geom_bar(aes(x=diferencia_25_24, y= Núcleo),stat="identity")

d <-
ggplot(dif_promedio) +
  geom_bar(
    aes(x = diferencia_25_24, 
        y = Núcleo,
        fill = diferencia_25_24 > 0),
    stat = "identity",
    width= .6
  )+ 
  labs(x="Δ  en % de Núcleo",
       y="")+
  scale_fill_manual(
    values = c("TRUE" = "#c44e52", "FALSE" = "#4c72b0"),
    name = "Δ en % de Núcleo \nJul 2025 - Ene 2024",
    labels = c("FALSE" = "Disminuye", "TRUE" = "Aumenta")
  )+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 7))

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
    legend.text = element_text(size = 8)
  )

figura
ggsave(
  figura,
  filename = paste0("Resultados/mspa_nucleos_.png"),
  width = 18,
  height = 20,
  units = "cm"
)


getwd()

writeRaster(r_mspa_clases[[7]], filename = "Resultados/2025_octubre.tif", datatype="INT1U")
