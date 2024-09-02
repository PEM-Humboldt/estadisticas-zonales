# Título: Soy una plantilla
#
# Descripción: r
#
# Autor(es): Alejandra Narváez Vallejo
# 
# Por hacer o  corregir: 

## Vaerguar sobre Roxygen para la documentción de funciones
# Documentar las funciones
## Completar funciones con ROxygen
# paralelizar y usar clusters
# Mejorar la consulta de si el raster solo tiene NaN
# falta la parte donde se extraer la información para boxplots usar list_rasters
# Diseñar la rtabla comparativa
 # revisar que todos loa paquetesw se usen



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

here::i_am(path="Codigos/Cruce_integridad1.R")




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

SisRef <- 9377
nombre_capas<-c("Dep_COL", "runap", "CNegras" , "Resguardos","RCampesinas")


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas Vector

capas_files<-list.files(dir_Datos_Or, recursive=T, pattern= "shp$", full.names = T)

capas_st<-lapply(capas_files[1:4],CargarProyectar)

capas_st[[5]]<- st_read(capas_files[[5]])

names(capas_st)<-nombre_capas


# Capas raster
# integridad

Intg<-rast( file.path(dir_Datos_Intm,"Integrida309377.tif"))

# raster base

r_base<-rast( file.path(dir_Datos_Intm,"rbase_30_noSA.tif"))


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

# Integridad que ignore los valores 0, que es zona de no bosquepotencial  según compación con la capa de mapbiomas

Intg_0<- Intg
Intg_0[Intg==0]<- NA 


# Rasterizar capas vectoriales ####

# Ruta del archivo raster
raster_paths <- paste0(dir_Datos_Intm,"/",nombre_capas,".tif ")


# Arreglar los campos para rasterizar

capas_st$Dep_COL$dpto_ccdgo<- as.numeric(capas_st$Dep_COL$dpto_ccdgo)

capas_st$CNegras$CNCODIGO<- as.numeric(capas_st$CNegras$CNCODIGO)

capas_st$Resguardos$id_RI <- as.numeric(factor(capas_st$Resguardos$NOMBRE_RES))

capas_st$RCampesinas$id_RC <- as.numeric(factor(capas_st$RCampesinas$Nombre))




# atributos a usar para rasterizar


atributo_rast<- c("dpto_ccdgo",
                  "id_pnn",
                  "CNCODIGO",
                  "id_RI",
                  "id_RC")


cat_rast<- c("dpto_cnmbr",
                   "nombre",
                   "CNNOMBRE",
                   "NOMBRE_RES",
                   "Nombre")


# rasterizar capas vectoriales y asignar niveles

raster_capas <-setNames(lapply(seq_along(raster_paths), rasterizar), nombre_capas)

levels(raster_capas$Dep_COL)<- capas_st$Dep_COL[c("dpto_ccdgo", "dpto_cnmbr")]%>%st_drop_geometry()
levels(raster_capas$runap)<- capas_st$runap[c(atributo_rast[2], cat_rast[2])]%>%st_drop_geometry()
levels(raster_capas$CNegras)<- capas_st$CNegras[c(atributo_rast[3], cat_rast[3])]%>%st_drop_geometry()
levels(raster_capas$Resguardos)<- unique(capas_st$Resguardos[c(atributo_rast[4], cat_rast[4])]%>%st_drop_geometry())
levels(raster_capas$RCampesinas)<- capas_st$RCampesinas[c(atributo_rast[5], cat_rast[5])]%>%st_drop_geometry()



# Crear máscara de integridad de zonas no especiales #### 


raster_path <- paste0(dir_Datos_Intm,"/","mask_NoEspecial",".tif ")

if (file.exists(raster_path)) {
    exmask0 <- rast(raster_path)
    
  } else {
    exmask0 <- Reduce(function(x, y) mask(x, y, inverse = TRUE), raster_capas[2:5], init = Intg)
    
  # Guardar el raster resultante en un archivo
    writeRaster(exmask0, raster_path)
  }


  
#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

list_deptos<- capas_st[[1]] %>% split(., .$dpto_ccdgo)

# Construcción de listas

list_rasters<- list()

df_global<- data.frame()

df_runap<-data.frame()
df_negros<-data.frame()
df_resguardos<-data.frame()
df_campesino<-data.frame()


i=1


#for ( i in c(1:27,29:length(list_deptos))){

for ( i in 1:5){  
  
  
  Nombre_dept<- list_deptos[[i]]$dpto_cnmbr
  print(Nombre_dept)
  
  # areas de estudio para las capas base: Integridad y mascara de áreas no Específcas  ####
  r_Intg_aoi<-definicionAOI(Intg, i)
  r_Intg0_aoi<-definicionAOI(Intg_0, i)
  exmask<- definicionAOI(exmask0, i)
  
  
  # Calcular estadísticas zonales y las mascaras de zonas para integridad
  
  C_ZM<-lapply(raster_capas[2:5],zonalMask)
  
  
  ## Estadísticas zonales. Descripción local ####
  
  # colectar las estadísticas zonales
  
  df_runap<-rbind(df_runap,C_ZM[[1]][[1]] )
  df_negros<-rbind(df_negros,C_ZM[[2]][[1]])
  df_resguardos<-rbind(df_resguardos,C_ZM[[3]][[1]])
  df_campesino<-rbind(df_campesino,C_ZM[[4]][[1]])
  
  # calcular estatisticas globales por departamentos ####
  
  # construcción de contenedor raster 
  # llenado con Stack de los rasters para cada zona especial 
  
  list_rasters[[i]]<- c(C_ZM[[1]][[2]],C_ZM[[2]][[2]],C_ZM[[3]][[2]],C_ZM[[4]][[2]],exmask)%>%
    setNames(c(names(C_ZM), "noEspecial"))
  
  names(list_rasters)[i]<-Nombre_dept
  
  # calcular estatisticas globales 
  
  Stats<-lapply(list_rasters[[i]], StatRaster)
  
  # Combinar la lista de data.frames en un solo data.frame
  Stats_df <- bind_rows(Stats)%>%
    mutate(Grupo=rownames(.))
  
  df_global<- rbind(df_global, Stats_df)
  
  # Crear el box plot ####
  
  png(filename=paste0("Resultados/Gráficas Departamentos/",Nombre_dept), width=555)
  
  boxplot(list_rasters[[i]])
  
  dev.off()
  
  gc()
  
}


# Guardar la información de las estadísticas zonales

write_excel_csv2(df_runap, paste0(dir_Resultados,"/tablas departamento/", nombre_capas[2],".csv"))
write_excel_csv2(df_negros, paste0(dir_Resultados,"/tablas departamento/", nombre_capas[3],".csv"))
write_excel_csv2(df_resguardos, paste0(dir_Resultados,"/tablas departamento/", nombre_capas[4],".csv"))
write_excel_csv2(df_campesino, paste0(dir_Resultados,"/tablas departamento/", nombre_capas[5],".csv"))


datatable(df_runap, 
          options = list(pageLength = 35 , 
                         paging=T,        
                         language = list( search = "Buscar:",
                                          lengthMenu = "Mostrar _MENU_ entradas"))) 





# Guardar la información de las estadísticas deapartamentales
df_global<-df_global[,c(5,1:4)]
row.names(df_global)<-NULL
write_excel_csv2(df_global, paste0(dir_Resultados,"/tablas_Col_def/Especiales_Stats.csv"))


datatable(df_global, 
          options = list(pageLength = 35 , 
                         paging=T,        
                         language = list( search = "Buscar:",
                                          lengthMenu = "Mostrar _MENU_ entradas")))  %>%
  formatRound(3:5, 2)




list_rasters






























































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



