# Título: IHEH en la altillanura
#
# Descripción: análisis multitemporal de la huella espacial humana por municipios Cruzando con la formación de valor agregado del Dane
#
# Autor(es): Alejandra Narváez Vallejo
#
# Por hacer o  corregir:

## Clasificar la huella por susus rangos . Verificar los rangos de la reclasiisifcacion, se incluye el mayor o el menor
## quedé en los gráficos, revisar dimensiones
## falta hacer el readme

# Por hacer o  corregir: Observaciones

## Código en desarrollo


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
library(readxl)
library(purrr)
library(stringr)
library(tidyr)
library(corrplot)
library(forcats)
#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(), "..", ".."))

dir_Datos_Or <- file.path("Datos/Originales")
dir_Datos_Intm<- file.path ("Datos/Intermedios")
dir_Resultados <- file.path("Resultados")


#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

Años <- c("2018", "2020", "2022")
Años_numero <- 3


Unidad_analsis <- "bio_mun" # Departamento" Municipio, Bioma
atributo_rast <- c("MpCodigo")

#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************
#*

archivo <- file.path(dir_Datos_Or,"DANE/VA_mun_corriente_milesmillonesCOP.xlsx")

# 1. Obtener nombres de las hojas
hojas <- excel_sheets(archivo)

# 2. Leer todas las hojas y unirlas
tabla_final <- map_dfr(hojas, function(h) {
  read_excel(archivo, sheet = h) %>%
    mutate(Año = h)  # agrega el nombre de la hoja como columna
}) %>% 
  rename( "VA"= "Valor agregado\r\n")


Stat_values <- read.csv2( paste0(dir_Resultados, "/IHEHcorine_stats_", Unidad_analsis,".csv"))
Stat_reclass <- read.csv2( paste0(dir_Resultados, "/IHEHcorine_clases_",Unidad_analsis,".csv"))


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## tablas iheh  ####

homogenizarCodigo <- function(Stat){
  str_pad(
    as.character(Stat[[atributo_rast]]),
    width = 7,
    side = "left",
    pad = "0"
  )
}

Stat_values[["atributo_rast"]] <- homogenizarCodigo(Stat_values)
Stat_reclass[["atributo_rast"]] <- homogenizarCodigo(Stat_reclass)


## completar con información económica ####


ecoMergeIheh <- function(Stat){
  merge(Stat,tabla_final, 
        by.x =c(atributo_rast, "Año"), 
        by.y=c( "Código Municipio", "Año") )
}


Eco_iheh <- ecoMergeIheh(Stat_values) %>% 
  mutate(VA1area= `Actividades primarias *`/area_cal_km2,
         VA2area= `Actividades secundarias **`/area_cal_km2,
         VA3area= `Actividades terciarias ***`/area_cal_km2, 
         VATarea= VA/area_cal_km2 ) %>% 
  
  mutate(
    Piso_termico = case_when(
      MpAltitud < 1000 ~ "Cálido",
      MpAltitud >= 1000 & MpAltitud < 2000 ~ "Templado",
      MpAltitud >= 2000 & MpAltitud < 3000 ~ "Frío",
      MpAltitud >= 3000 & MpAltitud < 4000 ~ "Páramo",
      MpAltitud >= 4000 ~ "Nival",
      TRUE ~ NA_character_
    ),
    Piso_termico = factor(Piso_termico, levels = c("Cálido", "Templado","Frío", "Páramo", "Nival")))


Eco_iheh_rcl <- ecoMergeIheh(Stat_reclass)%>% 
  mutate(VA1area= `Actividades primarias *`/area_cal_km2,
         VA2area= `Actividades secundarias **`/area_cal_km2,
         VA3area= `Actividades terciarias ***`/area_cal_km2, 
         VATarea= VA/area_cal_km2 )%>%
  mutate(
    Piso_termico = case_when(
      MpAltitud < 1000 ~ "Cálido",
      MpAltitud >= 1000 & MpAltitud < 2000 ~ "Templado",
      MpAltitud >= 2000 & MpAltitud < 3000 ~ "Frío",
      MpAltitud >= 3000 & MpAltitud < 4000 ~ "Páramo",
      MpAltitud >= 4000 ~ "Nival",
      TRUE ~ NA_character_
    ),
    Piso_termico = factor(Piso_termico, levels = c("Cálido", "Templado","Frío", "Páramo", "Nival")),
    Categorías = factor(Categorías, levels = c("Natural" , "Bajo", "Medio"  ,     "Alto"  ,  "Muy Alto"   ))
    )


Eco_iheh_rcl <- Eco_iheh_rcl %>%
  mutate(
    Categorías2 = fct_collapse(
      Categorías,
      Alto = c("Alto", "Muy Alto")
    )
  )


#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************


#### prueba con las ponderadas por areas #########

Eco_iheh %>%
  ggplot(aes(
    x = fct_reorder(Bioma, mean, .fun = stats::median),
    y = mean
  )) +
  geom_boxplot() +
 # geom_jitter(aes(size = VATarea), alpha = 0.1, colour="blue") +
  geom_jitter(size=.3, alpha = 0.1, colour="blue") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+ coord_flip()+
  labs(x="", y= " mean huella")



Eco_iheh %>%
  ggplot(aes(
    x = fct_reorder(Bioma, mean, .fun = stats::median),
    y = mean
  )) +
  geom_boxplot() +
  geom_jitter(size=.3, alpha = 0.1, colour="blue") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+ 
  facet_grid (cols=vars(Piso_termico))+
coord_flip()+
labs(x="", y= " mean huella")


# por clases



Eco_iheh_rcl %>% 
  ggplot(aes(x = Categorías, y = Porcentaje)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(size=.3, alpha = .05, colour="blue") +
  facet_wrap(
    vars(Bioma),
    labeller = labeller(Bioma = label_wrap_gen(width = 30))
  ) +
  theme(
    strip.text = element_text(size = 8)
  )


Eco_iheh_rcl %>% 
  ggplot(aes(x = Categorías2, y = Porcentaje)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(size=.3, alpha = .05, colour="blue") +
  facet_wrap(
    vars(Bioma),
    labeller = labeller(Bioma = label_wrap_gen(width = 30))
  ) +
  theme(
    strip.text = element_text(size = 8)
  )


Eco_iheh_rcl %>% 
  ggplot(aes(x = fct_reorder(Bioma, Porcentaje, .fun = stats::median), y = Porcentaje)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(size=.3, alpha = .05, colour="blue") +
  facet_grid(
    cols=vars(Categorías2)
    #labeller = labeller(Bioma = label_wrap_gen(width = 30)
    ) +
  coord_flip()+
  labs(x="")



Eco_iheh_rcl %>% 
  ggplot(aes(x = Categorías2, y = Porcentaje)) +
  geom_boxplot(alpha = 0.1) +
  facet_grid( cols= vars (Piso_termico),rows=vars(Bioma),
    labeller = labeller(Bioma = label_wrap_gen(width = 30))
   )+
  theme(
    strip.text = element_text(size = 8)
  )


Eco_iheh_rcl %>% 
  ggplot(aes(x = Piso_termico , y = Porcentaje)) +
  geom_boxplot(alpha = 0.1) +
  facet_grid( cols= vars (Categorías2),rows=vars(Bioma),
              labeller = labeller(Bioma = label_wrap_gen(width = 10))
  )+
  theme(
    strip.text = element_text(size = 5)
  )


Eco_iheh_rcl %>% 
  ggplot(aes(x = Piso_termico , y = Porcentaje)) +
  geom_boxplot(alpha = 0.1) +
  
  theme(
    strip.text = element_text(size = 5)
  )


# grafica  desacople ####

load(file= file.path(dir_Resultados,"mun_desacoplados.csv"))


Eco_iheh_rcl_ds <- Eco_iheh_rcl %>% 
  filter(MpNombre %in% revisar$MpNombre)

Eco_iheh_ds <- Eco_iheh %>% 
  filter(MpNombre %in% revisar$MpNombre)

#### prueba con las ponderadas por areas #########

Eco_iheh_ds %>%
  ggplot(aes(
    x = fct_reorder(Bioma, mean, .fun = stats::median),
    y = mean
  )) +
  geom_boxplot() +
  # geom_jitter(aes(size = VATarea), alpha = 0.1, colour="blue") +
  geom_jitter(size=.3, alpha = 0.1, colour="blue") +
  scale_y_continuous(limits = c(0, 100)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+ coord_flip()+
  labs(x="", y= " mean huella")



Eco_iheh_ds %>%
  ggplot(aes(
    x = fct_reorder(Bioma, mean, .fun = stats::median),
    y = mean
  )) +
  geom_boxplot() +
  geom_jitter(size=.3, alpha = 0.1, colour="blue") +
  scale_y_continuous(limits = c(0, 100)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+ 
  facet_grid (cols=vars(Piso_termico))+
  coord_flip()+
  labs(x="", y= " mean huella")


# por clases



Eco_iheh_rcl_ds %>% 
  ggplot(aes(x = fct_reorder(Bioma, Porcentaje, .fun = stats::median), y = Porcentaje)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(size=.3, alpha = .05, colour="blue") +
  facet_grid(
    cols=vars(Categorías2)
    #labeller = labeller(Bioma = label_wrap_gen(width = 30)
  ) +
  coord_flip()+
  labs(x="")



Eco_iheh_rcl %>% 
  ggplot(aes(x = Categorías2, y = Porcentaje)) +
  geom_boxplot(alpha = 0.1) +
  facet_grid( cols= vars (Piso_termico),rows=vars(Bioma),
              labeller = labeller(Bioma = label_wrap_gen(width = 30))
  )+
  theme(
    strip.text = element_text(size = 8)
  )


Eco_iheh_rcl %>% 
  ggplot(aes(x = Piso_termico , y = Porcentaje)) +
  geom_boxplot(alpha = 0.1) +
  facet_grid( cols= vars (Categorías2),rows=vars(Bioma),
              labeller = labeller(Bioma = label_wrap_gen(width = 10))
  )+
  theme(
    strip.text = element_text(size = 5)
  )


# prueba de categorias de cambios ####
load(file.path(dir_Resultados,"mun_desacoplados.csv"))


tc_x_unir <- tabla_cambios %>% 
  select(MpCodigo,MpNombre,categoria_cambio)

Eco_iheh_c <- merge(Eco_iheh, tc_x_unir)

plot_Eco_iheh_c <- Eco_iheh_c %>% filter(Año ==2018)



ggplot(plot_Eco_iheh_c, aes(y = Bioma, x = mean)) +   
  geom_boxplot() +   
  geom_jitter(size = .6, alpha = .2, colour = "blue")+
  #facet_wrap(vars(categoria_cambio), nrow=1, labeller = label_wrap_gen(width = 20))
facet_grid(col=vars(categoria_cambio), labeller = label_wrap_gen(width = 20))+
  labs(y="", x= "Promedio de huella")+
  theme(axis.text=element_text(size=6),
        strip.text = element_text(size=6),
        axis.title.x = element_text(size = 7)
        )


ggplot(plot_Eco_iheh_c, aes(y = Bioma, x = mean)) +   
  geom_boxplot() +   
  geom_jitter(size = .6, alpha = .2, colour = "blue")+
  facet_grid(row=vars(Piso_termico), col= vars(categoria_cambio))
