
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


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************
#*
#*
Años <- c( "2018")

Huella_cat <- data.frame(Cat = 1:5,
                         nom = c("Natural", "Bajo", "Medio", "Alto", "Muy Alto"))

raster_interes <- rast("~/GitHub/huella-humana-analisis/Resultados/IHEH_IAVH12018.tif") %>% setNames(Años)



# Mu
wcs <- rast("~/GitHub/huella-humana-analisis/Datos/hft_others/wcs/hii_2018-01-01 (1).tif")

plot(wcs)

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## arReglar las proyecciones

extension <- ext(raster_interes)

extent_moll<-project(extension, to=crs(wcs),from= "EPSG:9377")

r_base <-  crop(wcs, extent_moll) 
plot(r_base)
r_base <- project(r_base, raster_interes, method = "near")

r_base <-  mask(r_base, raster_interes)
 hist(r_base)

comparar <- c( r_base, raster_interes)
plot(comparar)

plot(r_base*100/6400- raster_interes ,
      main="Externo - IAVH")


correlacion <- layerCor(comparar, fun = "pearson", na.rm = TRUE)

vals <- values(comparar, na.rm = TRUE)

df_10 <- vals %>% 
  data.frame() %>% 
  rename(hfp2018 = 1, iavh = 2) %>% 
  
  sample_frac(0.001)

plot( df_10$hfp2018*100/6400,df_10$iavh,
      xlab = "HFP_ext",
      ylab = "iavh",
      main = "Relación lineal entre variables raster",
)


abline(lm(iavh ~ I(hfp2018*100/6400), data = df_10), lwd = 2, col="blue")

(cor_ihehs <- data.frame(cor_v=cor(df_10$iavh, df_10$hfp2018)))

modelo <- lm(iavh ~ I(hfp2018*100/6400), data = df_10)
summary(modelo)



#install.packages("ggpmisc")
library(ggpmisc)

df_10 %>% 
  #filter(Año==2018 & Actividades == "VATarea" & variable %in% c("mean", "sd")) %>% 
  ggplot(aes(x = hfp2018*100/6400, y = iavh)) +
  
  geom_point(alpha = 0.4) +
  
  # ✅ Ajuste lineal
  geom_smooth(method = "lm", se = FALSE) +
  
  # ✅ Fórmula del modelo en la gráfica
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    label.x = "right",
    label.y = "bottom",
    size = 3
  ) +
  
  # Correlaciones que ya tienes
  geom_text(
    data = cor_ihehs,
    aes(
      x = Inf, y = Inf,
      label = paste0(
        "r=", round(cor_v, 3)
      )
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +

  labs(
    title = "wcs"
    
  )+
  coord_cartesian(ylim = c(0, 100))


# Clasificar la huella iavh por sus rangos de intensidad 

## matriz de reclasificación

rc_matrix <- matrix(c(-1, 0, 1,
                      0, 15, 2,
                      15, 30, 3,
                      30, 100, 4), 
                    ncol = 3, byrow = TRUE)

raster_reclass <- classify(raster_interes, rc_matrix)

# Clasificar la huella iavh por sus rangos de intensidad 

## matriz de reclasificación

rc_matrix_t <- matrix(c(-1, 0.001,1,
                        0.001,1, 2,
                        1,4, 3,
                        4, 64, 4
                        ), 
                      ncol = 3, byrow = TRUE)


raster_reclass_ext <- classify(r_base/100, rc_matrix_t, right =FALSE)

plot(raster_reclass)
plot(raster_reclass_ext)


# comparar reclas

comparar_rcl <- c( raster_reclass, raster_reclass_ext)

plot(comparar_rcl)
vals_rcl <- values(comparar_rcl, na.rm = TRUE)
df_10_rcl <- vals_rcl %>% 
  data.frame() %>% 
  sample_frac(0.0002)%>% 
  mutate(
    "X2018" = recode(as.character(X2018),
                     "1" = "natural",
                     "2" = "bajo",
                     "3" = "medio",
                     "4" = "alto"),
    
    "X2018" = factor(2018, levels = c("natural", "bajo", "medio", "alto"))
  )



ggplot(df_10_rcl)+
  geom_bar(aes(x=hii_2018.01.01..1.))+
  facet_wrap(vars(X2018))

