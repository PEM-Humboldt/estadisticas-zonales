# Título: Soy una plantilla
#
# Descripción: r
#
# Autor(es): Alejandra Narváez Vallejo
# 
# Por hacer o  corregir: 

## Vaerguar sobre Roxygen para la documentción de funciones
## Completar funciones con ROxygen


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


#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

dir_Datos<- here::here("Datos")
capas_files<-list.files(dir_Datos, recursive=T, pattern= "shp$", full.names = T)

#**********************************************************
# Cargar funciones ----------------------------
#**********************************************************

source(here::here("Código", "Funciones.R"))


#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

SisRef <- 9377


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas Vector

capas<-lapply(capas_files, st_read)

Col<-capas[[1]]%>% st_transform( crs= 9377)
runap<-capas[[2]]%>% st_transform( crs= 9377)
Negras<- capas[[3]] %>%  st_transform( crs= 9377)
resguardo<-capas[[4]] %>% st_transform( crs= 9377)
campesinas<- capas[[5]]# %>% st_transform( crs= 9377)

# integridad

Intg<-rast( "Datos/Integrida309377.tif")


# raster base

r_base<-rast("Datos/rbase_30_noSA.tif")



#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

# Integridad que ignore los valores 0, que es zona de no bosquepotencial  según compación con la capa de mapbiomas

Intg_0<- Intg
Intg_0[Intg==0]<- NA 


# Rasterizar
r_Col<- Col%>%
  rasterize( r_base, field="dpto_cnmbr")

r_Col<- rast( "Datos/Dep_COL.tif")

r_runap<- runap%>%
  rasterize( r_base, field="nombre")

r_runap<- rast(  "r_runap.tif")

r_Negras<- Negras%>%
  rasterize( r_base, field="CNNOMBRE")

r_Negras<-rast("Datos/r_Negras.tif")

r_resguardo<- resguardo%>%
  rasterize( r_base, field="NOMBRE_RES")

r_resguardo <- rast("Datos/r_resguardo.tif")

r_campesinas<- campesinas%>%
  terra::rasterize( r_base, field="Nombre")

r_campesinas <- rast("Datos/r_campesinas.tif")



# máscaras no zonas especiales para Integridad

exmask0<-mask(Intg,r_runap , inverse=T)%>%
mask(r_Negras , inverse=T)%>%
mask(r_resguardo , inverse=T)%>%
  mask(r_campesinas , inverse=T)


  
#****************************************************************************
# Analisis por departamento ----------------------------
#****************************************************************************

list_deptos<- Col %>% split(., .$dpto_ccdgo)

# Construcción de listas

list_runap<- list()
list_negras<- list()
list_resguardo<- list()
list_campesinas<- list()
list_ex<- list()


df_runap<-data.frame()
df_negros<-data.frame()
df_resguardos<-data.frame()
df_campesino<-data.frame()


i=28


for ( i in c(1:27,29:length(list_deptos))){
  
Nombre_dept<- list_deptos[[i]]$dpto_cnmbr
print(Nombre_dept)

## Corte de área de estudio ####

#r_Col_aoi<-definicionAOI(r_Col,i) # parece se obsoleto
r_runap_aoi<-definicionAOI(r_runap,i)
r_negros_aoi<-definicionAOI(r_Negras,i)
r_resguardos_aoi<-definicionAOI(r_resguardo, i)
r_campesinas_aoi<-definicionAOI(r_campesinas, i)
r_Intg_aoi<-definicionAOI(Intg, i)
r_Intg0_aoi<-definicionAOI(Intg_0, i)


## Estadísticas zonales. Descripción local ####

# #antes
# runap_intg<-zonalAnalisis( r_runap_aoi, tipo="_runap_intg1.csv")
# negros_intg<-zonalAnalisis( r_negros_aoi, tipo="_negros_intg1.csv")
# resguardos_intg<-zonalAnalisis( r_resguardos_aoi, tipo="_resguardos_intg1.csv")
# campesinas_intg<-zonalAnalisis( r_campesinas_aoi, tipo="_campesinas_intg1.csv")

runap_intg<-data.frame( Dpto=Nombre_dept,zonalAnalisis( r_runap_aoi))
negros_intg<-data.frame( Dpto=Nombre_dept,zonalAnalisis( r_negros_aoi))
resguardos_intg<-data.frame( Dpto=Nombre_dept,zonalAnalisis( r_resguardos_aoi))
campesinas_intg<-data.frame( Dpto=Nombre_dept,zonalAnalisis( r_campesinas_aoi))

# colectar las estadísticas zonales

df_runap<-rbind(df_runap,runap_intg )
df_negros<-rbind(df_negros,negros_intg)
df_resguardos<-rbind(df_resguardos,resguardos_intg)
df_campesino<-rbind(df_campesino,campesinas_intg)


## Para estadísticos globales ####

# Se usa r_Intg_aoi porque se necesitan todos lo pixels para el cálculo de áreas
# máscaras de capas para el boxplot 

mask_IntRunap<-mask(r_Intg_aoi,r_runap_aoi ) # temporalmente fuera
mask_IntNegros<-mask(r_Intg_aoi,r_negros_aoi )# temporalmente fuera
mask_IntResguardo<-mask(r_Intg_aoi,r_resguardos_aoi )# temporalmente fuera
mask_IntCampesinas<-mask(r_Intg_aoi,r_campesinas_aoi )
exmask<- definicionAOI(exmask0, i)


# construcción de stacks por grupo

list_runap<-listaMascaras(list_runap, mask_IntRunap)# temporalmente fuera
list_negras<-listaMascaras(list_negras, mask_IntNegros)# temporalmente fuera
list_resguardo<-listaMascaras(list_resguardo, mask_IntResguardo)# temporalmente fuera
list_campesinas<-listaMascaras(list_campesinas, mask_IntCampesinas)
list_ex<-listaMascaras(list_ex, exmask)# temporalmente fuera
gc()

}



# Guardar la información de las estadísticas zonales

write_excel_csv2(df_runap, paste0("Resultados/tablas departamento/", "Runap.csv"))
write_excel_csv2(df_negros, paste0("Resultados/tablas departamento/", "CNegras.csv"))
write_excel_csv2(df_resguardos, paste0("Resultados/tablas departamento/", "Resguardos.csv"))
write_excel_csv2(df_campesino, paste0("Resultados/tablas departamento/", "ACampesinas.csv"))

datatable(df_campesino, 
          options = list(pageLength = 35 , 
                         paging=T,        
                         language = list( search = "Buscar:",
                                          lengthMenu = "Mostrar _MENU_ entradas"))) 



## extraer la información para box plots

i=20
raster_list<- list_ex


raster_values <- data.frame()
areas_con<-data.frame()

for (i in c(1:27,29: length(raster_list))){
#for (i in c(21:27,29: length(raster_list))){

if( !all(is.na(values(raster_list[[i]])))){
  print(i)
  dpto<- as.factor(names(raster_list)[i])
  valores<- values(raster_list[[i]], na.rm = TRUE)
  area_ha <- round(length(valores)* 0.09 ) # area en hectareas
  
  temp_df <- data.frame(Value = valores[valores>0], # solo los valores mayores a 0
                        Dpto = dpto)
  
  temp_area<- data.frame(Dpto = dpto, area_ha)
  
  # Agregar los valores al data frame principal
  raster_values <- rbind(raster_values, temp_df)
  areas_con<- rbind(areas_con, temp_area)
  
  gc()
}
  cat(i, " vacios")
  gc()
}


saveRDS(raster_values,file= "Resultados/tablas_Col/exC_valores")
write_csv2(raster_values,file= "Resultados/tablas_Col/exC_valores.csv")

write_csv2(areas_con, file= "Resultados/tablas_Col/exC_areas.csv")
saveRDS(areas_con,file= "Resultados/tablas_Col/exC_area")


# Preparación de tabla resumen

raster_values<-readRDS("Resultados/tablas_Col/ex_valores")


rm( list=ls()[-(28)])

raster_values0<-dplyr::filter(raster_values, Dpto %in% c("ANTIOQUIA", "VICHADA"))
raster_values0<- raster_values %>%
  group_by(Dpto) %>%
  sample_frac(0.6)# funciono para resguardos
raster_values


# Crear el box plot
modelo<- ggplot(raster_values0, aes(y = Dpto, x = Value)) +
  geom_boxplot(outlier.size=1)+ # , outlier.alpha=.2
  labs(title = "Box Plots de Integridad forestal en Áreas no especiales ", x = "Integridad forestal", y = "") 

gc()
modelo

ggsave(file="Resultados/Gráficas/NoEspecialC.png" ,plot=modelo)

saveRDS(modelo, file="Resultados/graf_negras")
rm(modelo)
load("Resultados/graf_negras")










































#**********************************************************
# Análisis ----------------------------
#**********************************************************


num_cores<- 5 # numero de nucleos para procesamiento paralelo
plan(sequential); gc() # limpiar cache
plan(multisession, workers= num_cores) # lanzar sesiones paralelas

# Ejecucion paralela
with_progress(result <- {
  p <- progressor(along= seq_along(names_deptos))
  
  result<-future_lapply( names_deptos, function(label_studyarea) {
  }
  )
}
)



