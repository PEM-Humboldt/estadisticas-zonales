# Título: Soy una plantilla
#
# Descripción: r
#
# Autor(es): Alejandra Narváez Vallejo
# 
# Por hacer o  corregir: 

## Clasificar la huella por susus rangos . Verificar los rangos de la reclasiisifcacion, se incluye el mayor o el menor


#*******************************************************************************
# librerías o dependencias -----------------------------------------------------
#*******************************************************************************

## lectura de datos  ####

library (sf) 
library(terra)
library(dplyr)
library(ggplot2)
library(formattable)
library(readr)
library(tidyr)
library(DT)
library(alluvial)
library(ggalluvial)


#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

dir_Datos_Or<- here::here("Datos", "Originales")
dir_Datos_Intm<- here::here("Datos","Intermedios")
dir_Resultados<- here::here("Resultados")

#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(here::here("Código", "Funciones.R"))


#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

nombres <- c("H1970", "H1990", "H2000", "H2015", "H2018")
Huella_cat<- data.frame(Cat=1:4, nom=c("Natural", "Bajo", "Medio", "Alto"))


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas raster

capas_raster<-list.files(dir_Datos_Or, recursive=T, pattern= "tif$", full.names = T)

raster_interes<-rast(capas_raster)%>% setNames(nombres)



# Capas raster 

capas_files<-list.files(dir_Datos_Or, recursive=T, pattern= "shp$", full.names = T)

SisRef <- crs (raster_interes)
capas_st<-lapply(capas_files[2],CargarProyectar)[[1]]



#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## El indicador  ####

# Recortar al área de estudio general

raster_interes<-crop(raster_interes, capas_st) %>% mask (capas_st)

plot(raster_interes)

# Clasificar la huella por sus rangos  

m <- c(0, 15, 1,
       15, 40, 2,
       40, 60, 3,
       60, Inf, 4)

# Convertir la matriz en un objeto de reclasificación
reclass_mat <- matrix(m, ncol=3, byrow=TRUE)

# Aplicar la reclasificación

raster_reclass <- classify(raster_interes, reclass_mat, include.lowest=TRUE)


 
## capa base ####

r_base <- raster_interes[[1]]

## Rasterizar capas vectoriales ####

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm,"/","AOI",".tif ")

# Arreglar los campos para rasterizar

str(capas_st)

capas_st$COD_MUNICI<- as.numeric(capas_st$COD_MUNICI)

# atributos a usar para rasterizar

atributo_rast<- c("COD_MUNICI")


cat_rast<- c("NOMBRE_ENT")


# rasterizar capas vectoriales y asignar niveles


raster_capas <-setNames(lapply(seq_along(raster_paths), rasterizar), nombre_capas)

if (file.exists(raster_paths)) {
  r_aoi <- rast(raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  r_aoi <- capas_st %>%
  rasterize(y = r_base, field =atributo_rast [1], filename=raster_paths)
  
}

levels(r_aoi)<- capas_st[c(atributo_rast, cat_rast)]%>%st_drop_geometry()



#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

list_deptos<- capas_st %>% split(., .$NOMBRE_ENT)

# Construcción de listas


Stat_reclass<- data.frame()
Stat_values<- data.frame()


i=1

for ( i in seq_along(list_deptos)){

#for ( i in 1:5){  
  
  Nombre_dept<- list_deptos[[i]]$NOMBRE_ENT
  print(Nombre_dept)
  
  # areas de estudio para las capas base: Integridad y mascara de áreas no Específcas  ####
  r_aoi<-definicionAOI(raster_interes, i)
  r_aoi_reclass<-definicionAOI(raster_reclass, i)

  
  ## Calcular estadísticas zonales #### 
  # Para los valores
  
  resumen <- terra::global(r_aoi, fun = c("mean","std"), na.rm = TRUE)
  resumen_median <- global(r_aoi, fun = mediana_fun)
  
  resumen <- cbind(Nombre_dept,resumen[1], resumen_median,resumen[2])
  # Renombrar las columnas para que tengan nombres más descriptivos
  names(resumen) <- c("Municipio","Promedio", "Mediana", "Desviación estandar")
  resumen$Año<- as.numeric(gsub("H", "", row.names(resumen)))
  
  
  Stat_values<- rbind (Stat_values, resumen)
  
  # Para las clases
  
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass<- group_by(frq_reclass, layer) %>% 
    mutate(Municipio= Nombre_dept,
           Porcentage = round(prop.table(count)*100, 3), 
           Categorías= factor(value, levels=1:4, labels=Huella_cat$nom),
           Año = as.numeric(as.character(factor(layer, levels=1:5, labels=c(1970,1990,2000,2015,2018 )))))%>%
    rename(Conteo=count)
    
  
  
  Stat_reclass<- rbind (Stat_reclass, tem_Stat_reclass)
  
  
}

#****************************************************************************
# Organizar las tablas y guardarlas ####
#****************************************************************************

Stat_values<- select(Stat_values, Municipio,Año, Promedio, Mediana, `Desviación estandar`)
row.names(Stat_values)<- NULL
Stat_reclass<- select(Stat_reclass, Municipio, Año, Categorías, Conteo, Porcentage)


# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_values, paste0(dir_Resultados,"/IHEH_stats.csv"))
write_excel_csv2(Stat_reclass, paste0(dir_Resultados,"/IHEH_clases.csv"))


datatable(Stat_values, 
          options = list(pageLength = 10 , 
                         paging=T,        
                         language = list( search = "Buscar:",
                                          lengthMenu = "Mostrar _MENU_ entradas"))) %>%
  formatRound(columns = c(3,5), digits = 2)
  


datatable(Stat_reclass, 
          options = list(pageLength = 10 , 
                         paging=T,        
                         language = list( search = "Buscar:",
                                          lengthMenu = "Mostrar _MENU_ entradas"))) %>%
  formatRound(columns = 6, digits = 2)


#****************************************************************************
# Gráficas ####
#****************************************************************************


## Gráfica de valores ####

ggplot(Stat_values)+
  geom_line(aes(x=Año, y= Promedio,colour = Municipio), linewidth=1.5 )+
  labs(x="", 
       y= "Promedio de IHEH")+
  scale_color_brewer( palette = "Set2", direction = -1) +
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.y = element_text(margin = margin(l = 10),vjust = 3.5))
  



Stat_reclass$Categoriás <- as.factor(Stat_reclass$Categoriás)


Stat_reclass0 <- 
  Stat_reclass

## Gráfico sencillo clases ####

gg <- Stat_reclass0%>%
  filter(Municipio== "CUMARIBO")%>%
ggplot(aes(y = percent, x = Año, alluvium = Categoriás))

# barras con color , alluvium gris
gg + geom_alluvium(color= "black",
                   width = 1.5, 
                   alpha = .2
                   , curve_type = "arctangent"
                   ,curve_range = 1
                   )+ 
  geom_stratum(aes(stratum = Categoriás, fill= Categoriás),
               width = 1.5)+
  theme_bw()



# barras blancas , alluvium con color
gg + geom_alluvium(aes(fill = Categoriás, colour = Categoriás),
                   width = 1.5, alpha = 2/3,
                #   decreasing = FALSE,
                , curve_type = "arctangent"
                ,curve_range = 1)+ 
  geom_stratum(aes(stratum = Categoriás), 
               #decreasing = FALSE, 
               width = 1.5)+
  theme_bw()




## todas las gráficas juntas ####

gg <- Stat_reclass0%>%
  ggplot(aes(y = percent, x = Año, alluvium = Categoriás))

# barras color , alluvium con color
gg + geom_alluvium(aes(fill = Categoriás, colour = Categoriás),
                   width = 2, alpha = .4,
                   #   decreasing = FALSE,
                   , curve_type = "arctangent"
                   ,curve_range = 1)+ 
  geom_stratum(aes(stratum = Categoriás
                   , fill= Categoriás,
              #     alpha = .4
                   ), 
               #decreasing = FALSE, 
               width = 2)+
  scale_fill_brewer( palette = "RdYlGn", direction = -1) +
  labs(y="Porcentage de área", x="")+
  facet_wrap(~ Municipio, scales = "fixed",nrow=4 ) +
  theme_bw()+ 
  theme(legend.position = "bottom")



# barras blancas , alluvium con color
gg + geom_alluvium(aes(fill = Categoriás, colour = Categoriás),
                   width = 2, alpha = 2/3,
                   #   decreasing = FALSE,
                   , curve_type = "arctangent"
                   ,curve_range = 1)+ 
  geom_stratum(aes(stratum = Categoriás
                   #, fill= Categoriás
  ), 
  #decreasing = FALSE, 
  width = 2)+
    scale_fill_brewer( palette = "RdYlGn", direction = -1) +
  labs(y="Porcentage de área", x="")+
  facet_wrap(~ Municipio, scales = "fixed",nrow=4 ) +
  theme_bw()+ 
  theme(legend.position = "bottom")

















































































