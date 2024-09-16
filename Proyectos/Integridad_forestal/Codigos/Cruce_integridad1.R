# Título: Soy una plantilla
#
# Descripción: r
#
# Autor(es): Alejandra Narváez Vallejo
# 
# Por hacer o  corregir: 


# paralelizar y usar clusters
# descripcion de la rutina
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
library(htmlwidgets)


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
source(file.path("..", "..", "Funciones_comunes" , "visualización.R"))


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

capas_st[[5]]<- st_read(capas_files[[5]])# reservas campesinas e cargo por separado porque presentaba problemas de proyección

names(capas_st)<-nombre_capas


# Capas raster

Intg0<-rast(file.path(dir_Datos_Or, "Integridad ecologica NASA", "Integrity_inde_x100.tif"))


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## Integridad

# proyectar raster ---

# se verifica si los raster ya existen , de lo contrario se generan y guardan

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm, "/", "Integridad9377", ".tif ")

path_base <- paste0(dir_Datos_Intm, "/", "rbase_30_noSA", ".tif ")


# creación de raster base

if (file.exists(path_base)) {
  r_base<-rast( file.path(dir_Datos_Intm,"rbase_30_noSA.tif"))
} else {
r_base<- rast(xmin=4330516, ymin= 1090467, xmax= 5684465, ymax= 2935222,  crs="EPSG:9377", res=30)
values(r_base)<-1

}

# Proyectar integridad

if (file.exists(raster_paths)) {
  Intg <- rast(raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  Intg <-  terra::project(Ing0,r_base,
              filename = raster_paths)
  
}

names( Intg)<- "Integridad"

# Sólo zona de bosque potencial 
# Integridad valores 0 son NA, que es zona de no bosque potencial  según compación con la capa de mapbiomas

Intg_0<- Intg
Intg_0[Intg==0]<- NA 


## Capas vectoriales ####

# Rasterizar 

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

levels(raster_capas$Dep_COL)<- capas_st$Dep_COL[c(atributo_rast[1], cat_rast[1])]%>%st_drop_geometry()
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

for ( i in 1:3){  
  
  
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
  
  # calcular estadísticas globales por departamentos ####
  
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
  
  
  png(filename=paste0(dir_Resultados,"/Gráficas Departamentos/",Nombre_dept), width=555)
  
  boxplot(list_rasters[[i]])
  
  dev.off()
  
  gc()
  
}


# Guardar la información de las estadísticas zonales

l_tablas<- list(df_runap,df_negros,df_resguardos,df_campesino)
names(l_tablas)<- nombre_capas[2:5]


GuardarTablas<-function(x){

  write_excel_csv2(l_tablas[[x]], paste0(dir_Resultados,"/tablas departamento/", names(l_tablas)[x],".csv"))

  t_html<-datatable(l_tablas[[x]], 
            options = list(pageLength = 35 , 
                           paging=T,        
                           language = list( search = "Buscar:",
                                            lengthMenu = "Mostrar _MENU_ entradas"))) 
  
  
  saveWidget(t_html, file = paste0(dir_Resultados,"/tablas departamento/",names(l_tablas)[x] ,".html"))
  
}

lapply(seq_along(l_tablas),GuardarTablas)

# Guardar la información de las estadísticas deapartamentales

df_global<-df_global[,c(5,1:4)]
row.names(df_global)<-NULL

write_excel_csv2(df_global, paste0(dir_Resultados,"/tablas_Col_def/Especiales_Stats.csv"))


t_html<-datatable(df_global, 
          options = list(pageLength = 35 , 
                         paging=T,        
                         language = list( search = "Buscar:",
                                          lengthMenu = "Mostrar _MENU_ entradas")))  %>%
  formatRound(3:5, 2)

saveWidget(t_html, file = paste0(dir_Resultados,"/tablas departamento/",names(l_tablas)[x] ,".html"))))##




list_rasters




