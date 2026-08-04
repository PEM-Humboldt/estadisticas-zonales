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
Años <- c( "2020")

Huella_cat <- data.frame(Cat = 1:5,
                         nom = c("Natural", "Bajo", "Medio", "Alto", "Muy Alto"))

raster_interes <- rast("~/GitHub/huella-humana-analisis/Resultados/IHEH_IAVH12020.tif") %>% setNames(Años)
SisRef <- crs (raster_interes)


# gasseter


c1 <- rast("~/GitHub/huella-humana-analisis/Datos/hft_others/gassert/HFP-100m-2020/Users/jmazzariello/Desktop/hfp/2020/hfp_2020_100m_2271378.852256801_-8209695.696147293_cog.tif")

c2 <- rast("~/GitHub/huella-humana-analisis/Datos/hft_others/gassert/HFP-100m-2020/Users/jmazzariello/Desktop/hfp/2020/hfp_2020_100m_2271378.852256801_-7390495.696147293_cog.tif")

c3 <- rast("~/GitHub/huella-humana-analisis/Datos/hft_others/gassert/HFP-100m-2020/Users/jmazzariello/Desktop/hfp/2020/hfp_2020_100m_632978.852256801_-7390495.696147293_cog.tif")

c4 <- rast("~/GitHub/huella-humana-analisis/Datos/hft_others/gassert/HFP-100m-2020/Users/jmazzariello/Desktop/hfp/2020/hfp_2020_100m_632978.852256801_-8209695.696147293_cog.tif")

c5 <- rast("~/GitHub/huella-humana-analisis/Datos/hft_others/gassert/HFP-100m-2020/Users/jmazzariello/Desktop/hfp/2020/hfp_2020_100m_1452178.852256801_-7390495.696147293_cog.tif")

#c6 <- rast("~/GitHub/huella-humana-analisis/Datos/hft_others/gassert/HFP-100m-2020/Users/jmazzariello/Desktop/hfp/2020/hfp_2020_100m_-186221.14774319902_-8209695.696147293_cog.tif")

c7 <- rast("~/GitHub/huella-humana-analisis/Datos/hft_others/gassert/HFP-100m-2020/Users/jmazzariello/Desktop/hfp/2020/hfp_2020_100m_1452178.852256801_-8209695.696147293_cog.tif")

c8 <- rast("~/GitHub/huella-humana-analisis/Datos/hft_others/gassert/HFP-100m-2020/Users/jmazzariello/Desktop/hfp/2020/hfp_2020_100m_-186221.14774319902_-7390495.696147293_cog.tif")



col <- merge(c1,c2,c3,c4,c5,c7,c8)

col

plot(col)
plot(c1)
plot(c2)
plot(c3)
plot(c4)

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## arReglar las proyecciones

extension <- ext(raster_interes)

extent_moll<-project(extension, to=crs(col),from= "EPSG:9377")

r_base <-  crop(col, extent_moll) 


r_base <- project(col, raster_interes, method = "near")

r_base <-  mask(r_base, raster_interes)

r_base[r_base == 64536 ] <- NA


plot(r_base/500- raster_interes ,
     main="Externo - IAVH")

comparar <- c( r_base, raster_interes)
plot(comparar)


correlacion <- layerCor(comparar, fun = "pearson", na.rm = TRUE)

vals <- values(comparar, na.rm = TRUE)

df_10 <- vals %>% 
  data.frame() %>% 
  rename(hfp2020 = 1, iavh = 2) %>% 
  
  sample_frac(0.001)

plot( df_10$hfp2020/500,df_10$iavh,
     xlab = "HFP_ext",
     ylab = "iavh",
     main = "Relación lineal entre variables raster",
      )


abline(lm(iavh ~ I(hfp2020/500), data = df_10), lwd = 2, col="blue")

cor(df_10$iavh/500, df_10$hfp2020)

modelo <- lm(iavh ~ I(hfp2020/500), data = df_10)
summary(modelo)



# Clasificar la huella iavh por sus rangos de intensidad 

## matriz de reclasificación

rc_matrix <- matrix(c(-1, 0, 1,
                      0, 15, 2,
                      15, 30, 3,
                      30, 100, 4                      ), 
                    ncol = 3, byrow = TRUE)

raster_reclass <- classify(raster_interes, rc_matrix)

# Clasificar la huella iavh por sus rangos de intensidad 

## matriz de reclasificación

rc_matrix_t <- matrix(c(-1, 1000, 1,
                      1000, 4000, 2,
                      4000, 50000, 3,
                      50000, 70000, NA), 
                    ncol = 3, byrow = TRUE)

raster_reclass_ext <- classify(r_base, rc_matrix_t, right =TRUE)

plot(raster_reclass)
plot(raster_reclass_ext)


# comparar reclas

comparar_rcl <- c( raster_reclass, raster_reclass_ext)

plot(comparar_rcl)
vals_rcl <- values(comparar_rcl, na.rm = TRUE)
df_10_rcl <- vals_rcl %>% 
  data.frame() %>% 
  rename(hfp2020 = `hfp_2020_100m_2271378.852256801_.8209695.696147293_cog`) %>% 
  filter(hfp2020 != 64536) %>% 
  sample_frac(0.0002) %>% 
  mutate(
    X2020 = recode(as.character(X2020),
                   "1" = "natural",
                   "2" = "bajo",
                   "3" = "medio",
                   "4" = "alto"),
    
    X2020 = factor(X2020, levels = c("natural", "bajo", "medio", "alto"))
  )



ggplot(df_10_rcl)+
  geom_bar(aes(x=hfp2020))+
  facet_wrap(vars(X2020))
  
