# Título: estructura del paisaje_alluvium
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: Esté toma los datos de os resultados organizados de los análisis ejecutados con el Softwae GUIDOS de los núcleos y hace la respectiva gráfica de alluvium, mostrando como es la transformación de las diferentes clases estructurales a los largo del tiempo.

# requiere de manipulación manual para elegir que datos (tabla graficar).Los objetos que requieren dicha manipulación son :
#- ta
#-noCambio
# tema


#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

library(ggalluvial)
library(tidyverse)

#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(), "..", ".."))

dir_Datos_Or <- file.path("Datos", "Originales")
dir_Resultados <- file.path ("Resultados", "GUIDOS")

#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# comente o quite comentario deacuerdo a la tabla que desea usar
#ta <- read_csv(file.path(dir_Datos_Or, "tablas_GUIDOS","macarena.csv"))
#ta<- read_csv(file.path(dir_Datos_Or, "tablas_GUIDOS","tinigua.csv"))
ta<- read.csv(file.path(dir_Datos_Or, "tablas_GUIDOS", "NucleosAll_alluviumIntro.csv"))

#categorias que no cambiaron en todos los grupos como conjunto
t_noCambio <- read_csv(file.path(dir_Datos_Or, "tablas_GUIDOS","RepertidoFull_nucleos_alluv.csv"))

#**********************************************************
# Parametros globales ----------------------------
#**********************************************************

#tema <- "PNN Sierra de la Macarena"
#tema <- "PNN Tinigua"
tema <- "NDFyB"

# comente o quite comentario de acuerdo a la tabla elegida anteriormente

#noCambio <- c(1, 2, 13, 15, 17, 22, 83, 240)# Macarena
#noCambio<-c(1, 5,7,8,20,28,150,677)#tinigua
#noCambio<-t_noCambio$Value  # todos los núcleos


#**********************************************************
# Gráficar ----------------------------
#**********************************************************

filter(ta, !(Value %in% noCambio)) %>%
  ggplot(
    aes(
      x = Año,
      stratum = Estructura,
      alluvium = Value,
      y = AREA_ha,
      fill = Estructura,
      label = Estructura
    )
  ) +
  # scale_x_discrete(expand = c(.1, .9)) +
  geom_flow() +
  geom_stratum(alpha = 1, width = 1) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Área (ha)") +
  ggtitle(tema)

