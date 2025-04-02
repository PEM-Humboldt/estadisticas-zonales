# Título: Prioridades de la Restauración en el país
#
# Descripción: Obtener cifras de la prioridades de restauración a nivel departament y  por zonas de manejo espacial en su jurisdicción
#
# Autor(es): Alejandra Narváez Vallejo
#


#*******************************************************************************
# librerías o dependencias -----------------------------------------------------
#*******************************************************************************

## lectura de datos  ####

library (sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(patchwork)
library(DT)



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

#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

SisRef <- 9377
nombre_capas<-c("Dep_COL","ndfyb", "runap", "CNegras" , "Resguardos","RCampesinas")


#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

# Capas Vector

capas_files<-list.files(dir_Datos_Or, recursive=T, pattern= "shp$", full.names = T)

capas_st<-lapply(capas_files[1:5],CargarProyectar) # revisar  que el orden sea el correcto("Dep_COL", "runap", "CNegras" , "Resguardos","RCampesinas")
# [1] "Datos/Originales/MGN2023_DPTO_POLITICO/MGN_ADM_DPTO_POLITICO.shp"                                               
# [2] "Datos/Originales/SHP_COMUNIDADES_NEGRAS/COMUNIDADES_NEGRAS_GCSmagna.shp"                                        
# [3] "Datos/Originales/xn--Resguardos_Indgenas-shp-ffc/d138eda3-19b9-4546-a41c-d5ef69c5f00a2020329-1-qzm77a.r5v1d.shp"
# [4] "Datos/Originales/Zonas_Reserva_Campesina93/Zonas_Reserva_Campesina.shp"                                         
# [5] "Datos/Originales/ZonasReservaCampesina/ZRC_2019.shp"      

capas_st[[6]]<- st_read(capas_files[[6]])# reservas campesinas e cargo por separado porque presentaba problemas de proyección

names(capas_st)<-nombre_capas


# Capas raster

Intg0<-rast(file.path(dir_Datos_Or, "mapa_restauracion", "mapa_restauracion.tif"))

#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## Capa de análisis ####

# proyectar raster ---

# se verifica si los raster ya existen , de lo contrario se generan y guardan

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm, "/", "Restauracion9377", ".tif ")

path_base <- paste0(dir_Datos_Intm, "/", "rbase_30_noSA", ".tif ")


# creación de raster base

if (file.exists(path_base)) {
  r_base<-rast( file.path(dir_Datos_Intm,"rbase_30_noSA.tif"))
} else {
  r_base<- rast(xmin=4330516, ymin= 1090467, xmax= 5684465, ymax= 2935222,  crs="EPSG:9377", res=30)
  values(r_base)<-1
  
}

# Proyectar 

if (file.exists(raster_paths)) {
  IntgP <- rast(raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  IntgP <-  terra::project(Intg0,r_base,
                          filename = raster_paths)
  
}
      
names( IntgP)<- "Restauracion"


# La capa a analizar no debe tener NAS , necesario para el cálculo de áreas. los NAs tomarán la categoría NoCat

Intg_0p<- IntgP

Intg_0p[is.na(Intg_0p)]<- 8  
levels(Intg_0p)<- data.frame(value=c(1:8),nombre= c("Moderada", "Alta", "Muy Alta", "Preservación", "Baja", "Título Minero", "Minería erosión", "NoCat")) 


#plot(Intg_0p)
#plot(IntgP)

## Capas vectoriales ####

# Rasterizar Zonas

# Ruta del archivo raster

raster_paths <- paste0(dir_Datos_Intm,"/",nombre_capas,".tif ")


# Arreglar los campos para rasterizar

capas_st$Dep_COL$dpto_ccdgo<- as.numeric(capas_st$Dep_COL$dpto_ccdgo)

capas_st$ndfyb$id_NF <- as.numeric(factor(capas_st$ndfyb$Nombre))

capas_st$CNegras$CNCODIGO<- as.numeric(capas_st$CNegras$CNCODIGO)

capas_st$Resguardos$id_RI <- as.numeric(factor(capas_st$Resguardos$NOMBRE_RES))

capas_st$RCampesinas$id_RC <- as.numeric(factor(capas_st$RCampesinas$Nombre))



# atributos a usar para rasterizar

atributo_rast<- c("dpto_ccdgo",
                  "id_NF",
                  "id_pnn",
                  "CNCODIGO",
                  "id_RI",
                  "id_RC")


cat_rast<- c("dpto_cnmbr",
             "Nombre",
             "nombre",
             "CNNOMBRE",
             "NOMBRE_RES",
             "Nombre"
             
)


# rasterizar capas vectoriales y asignar niveles

raster_capas <-setNames(lapply(seq_along(raster_paths), rasterizar), nombre_capas)

levels(raster_capas$Dep_COL)<- capas_st$Dep_COL[c(atributo_rast[1], cat_rast[1])]%>%st_drop_geometry()
levels(raster_capas$ndfyb)<- capas_st$ndfyb[c(atributo_rast[2], cat_rast[2])]%>%st_drop_geometry()
levels(raster_capas$runap)<- capas_st$runap[c(atributo_rast[3], cat_rast[3])]%>%st_drop_geometry()
levels(raster_capas$CNegras)<- capas_st$CNegras[c(atributo_rast[4], cat_rast[4])]%>%st_drop_geometry()
levels(raster_capas$Resguardos)<- unique(capas_st$Resguardos[c(atributo_rast[5], cat_rast[5])]%>%st_drop_geometry())
levels(raster_capas$RCampesinas)<- capas_st$RCampesinas[c(atributo_rast[6], cat_rast[6])]%>%st_drop_geometry()


nombre_capasCor<-c("Dep_COL","NDFyB", "RUNAP", "CNegras" , "RIndígen","RCampes") # corregir el nombre de la capa para los resultados
raster_capas <-   setNames(raster_capas,c(nombre_capasCor))

## Crear máscara de integridad de zonas no especiales #### 


raster_path <- paste0(dir_Datos_Intm,"/","mask_NoEspecial",".tif ")

if (file.exists(raster_path)) {
  exmask0 <- rast(raster_path)
  
} else {
  exmask0 <- Reduce(function(x, y) mask(x, y, inverse = TRUE), raster_capas[2:6], init = Intg)
  
  # Guardar el raster resultante en un archivo
  writeRaster(exmask0, raster_path)
}


exmask0 <- setNames(exmask0,"noEspecial")

#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************


list_deptos<- capas_st[[1]] %>% split(., .$dpto_ccdgo)

# Construcción de listas

Stat_reclass <- data.frame()
Stat_reclass_dept <- data.frame()
Stat_reclass_dept_Z <- data.frame()


i=1


for ( i in c(1:27,29:length(list_deptos))){
  
  #for ( i in 1:3){  
  
  
  Nombre_dept<- list_deptos[[i]]$dpto_cnmbr
  print(Nombre_dept)
  
 ## cortar el raster de departamentos al departamento de la iteración ####
  
  dept<-crop(raster_capas$Dep_COL, list_deptos[[i]])
  
  # Crear un raster temporal donde solo el valor actual es 1 y el resto es NA
  temp_raster <- dept == Nombre_dept
  temp_raster[temp_raster == 0] <- NA  # Convertir 0 a NA
  
  dept <- temp_raster
 
  ## areas de estudio para las capas: De análisis y las zonas incluyendo la mascara de áreas no Especiales  ####
   
  r_aoi_reclass<-crop(Intg_0p, dept) %>% mask(dept)
  raster_capas_m<-crop(c(rast(raster_capas[2:6]),exmask0), dept) %>% mask(dept)
  
  
  ## calcular estadísticas globales por departamentos ####
  
  frq_reclass <- terra::freq(r_aoi_reclass)
  
  tem_Stat_reclass <- group_by(frq_reclass, layer) %>% # creo que esta línea puede desaparecer
      mutate(
      Departamento = Nombre_dept,
      Porcentaje = round(prop.table(count) * 100, 3)
    ) %>%
    rename(Conteo = count,
           Categorías = value)
  
  
  # guardar en Stat_reclass acumulando en cada iteración
  Stat_reclass <- rbind (Stat_reclass, tem_Stat_reclass) %>%
    ungroup()
  
  
## Calcular estadísticas zonales para las zonas especiales y no especiales ####
  

zonalDiscreto <- function( zonasAOI){
  
    mask(r_aoi_reclass,zonasAOI) %>% 
    freq() %>% 
        mutate(
      Zona = names(zonasAOI),
      Municipio = Nombre_dept,
      Porcentaje = round(prop.table(count) * 100, 3)
      
    ) %>%
    rename(Conteo = count,
           Categorías = value)
}


C_ZM0<-lapply(raster_capas_m,zonalDiscreto)

temp_dept <- bind_rows(C_ZM0)

# guardar acumulando en cada iteración
Stat_reclass_dept <- rbind (Stat_reclass_dept, temp_dept)
 
### Gráficas por departamento y zona####

cat_interes <- c( "Baja","Moderada", "Alta", "Muy Alta" )

# Definir la paleta de colores
colores <- c(
  "Baja" = "#1f77b4",
  "Moderada" = "#2ca02c",
  "Alta" = "#ff7f0e",
  "Muy Alta" = "#d62728"
)


# Preparar tabla
Stat_reclass_dept1 <-Stat_reclass_dept %>% 
  filter (Categorías %in% cat_interes)%>%
  mutate(Area_ha=(res(Intg_0p)^2)*Conteo/10000,
         Categorías=factor(Categorías, levels= cat_interes),
         Zona=factor(Zona, levels= c(nombre_capasCor, "noEspecial"))
  ) %>% 
  rename(Departamento=Municipio)


# Gráfico 1: Área (ha)
g1 <- ggplot(Stat_reclass_dept1) +
  geom_bar(aes(x = Zona, y = Area_ha, fill = Categorías), stat = "identity") +
  scale_fill_manual(values = colores) +
  labs(title = Nombre_dept, x = "", y = "Área (ha)", fill = "Priorización") +
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank())
g1
# Gráfico 2: Porcentaje
g2 <- ggplot(Stat_reclass_dept1) +
  geom_bar(aes(x = Zona, y = Porcentaje, fill = Categorías), stat = "identity") +
  scale_fill_manual(values = colores) +
  labs(x = "", y = "Porcentaje de Zona", fill = "Priorización") +
  ylim(0, 100)+
  theme_bw()

g2
# Combinar gráficos con una sola leyenda
g3 <- g1 / g2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")


ggsave(paste0(dir_Resultados,"/Gráficas Departamentos/",Nombre_dept, "_restauración.png"),plot=g3, width = 12, height =  11, units="cm")


## Calcular estadísticas zonales para las unidades de las zonas especiales ####


# frecuencias para las unidades de cada zona.

DeptZonas <- function(Zonas){
  # combinar las capas en una tabla
rk <- data.frame(c(r_aoi_reclass,Zonas)) %>% 
  na.omit()

# hacer calculos de frecuencias
a <- rk %>% 
  count(across(2),across(1)) %>%
  rename(Unidad = 1,
         Conteo = n) %>% 
  group_by(Unidad) %>% 
  mutate(
    Porcentaje = round(prop.table(Conteo) * 100, 3),
    Departamento= Nombre_dept,
    Zona=names(Zonas) )%>% 
  filter(nombre %in% c("Moderada", "Alta", "Muy Alta", "Baja"))
}

D_ZM0<-lapply(raster_capas_m[[1:5]],DeptZonas)

temp_dept <- bind_rows(D_ZM0)
# guardar acumulando en cada iteración
Stat_reclass_dept_Z <- rbind (Stat_reclass_dept_Z, temp_dept)

}

#****************************************************************************
# Organizar las tablas y guardarlas ####
#****************************************************************************

## tabla general departamento ####

Stat_reclass1<-filter(Stat_reclass, Categorías %in% cat_interes)%>%
  mutate(Area_ha=(res(Intg_0p)^2)[1]*Conteo/10000)

Stat_reclass1 <- dplyr::select(Stat_reclass1, Departamento, Categorías, Conteo, Porcentaje, Area_ha)

# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_reclass1, paste0(dir_Resultados,"/tablas_Col_def/Restauracion_dept_tot.csv"))


t_html<-datatable(Stat_reclass1[c(1:2,4:5)], 
                  options = list(pageLength = 35 , 
                                 paging=T,        
                                 language = list( search = "Buscar:",
                                                  lengthMenu = "Mostrar _MENU_ entradas")))%>%
  formatRound(4, 0, dec.mark = ",", mark = ".")%>%
  formatRound(column= 3,dec.mark = ",", mark = ".")

t_html

saveWidget(t_html, file = paste0(dir_Resultados,"/tablas_Col_def/Restauracion_dept_tot.html"))##


## tabla general departamento y Zona ####

Stat_reclass_dept1 <-Stat_reclass_dept %>% 
  filter (Categorías %in% cat_interes)%>%
  mutate(Area_ha=(res(Intg_0p)^2)*Conteo/10000,
         Categorías=factor(Categorías, levels= cat_interes),
         Zona=factor(Zona, levels= c(nombre_capasCor, "noEspecial"))
  ) %>% 
  rename(Departamento=Municipio)

Stat_reclass_dept1 <- dplyr::select(Stat_reclass_dept1, Departamento, Zona, Categorías,  Porcentaje, Area_ha)

# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_reclass_dept1, paste0(dir_Resultados,"/tablas_Col_def/Restauracion_dept_Zona.csv"))


t_html<-datatable(Stat_reclass_dept1, 
                  options = list(pageLength = 35 , 
                                 paging=T,        
                                 language = list( search = "Buscar:",
                                                  lengthMenu = "Mostrar _MENU_ entradas")))%>%
  formatRound(5, 0, dec.mark = ",", mark = ".")%>%
  formatRound(column= 4,dec.mark = ",", mark = ".")

t_html

saveWidget(t_html, file = paste0(dir_Resultados,"/tablas_Col_def/Restauracion_dept_Zona.html"))##

## tabla general departamento y Zona y unidades ####

Stat_reclass_dept_Z1 <- Stat_reclass_dept_Z %>% 
  rename(Categorías= nombre) %>% 
    mutate(Area_ha=(res(Intg_0p)^2)[1]*Conteo/10000,
         Categorías=factor(Categorías, levels= cat_interes))
         
Stat_reclass_dept_Z1 <- dplyr::select(Stat_reclass_dept_Z1, Departamento, Zona, Unidad, Categorías,  Porcentaje, Area_ha)

# Guardar la información de las estadísticas zonales

write_excel_csv2(Stat_reclass_dept_Z1, paste0(dir_Resultados,"/tablas_Col_def/Restauracion_dept_Zona_Unidad.csv"))


t_html<-datatable(Stat_reclass_dept_Z1, 
                  options = list(pageLength = 35 , 
                                 paging=T,        
                                 language = list( search = "Buscar:",
                                                  lengthMenu = "Mostrar _MENU_ entradas")))%>%
  formatRound(6, 0, dec.mark = ",", mark = ".")%>%
  formatRound(column= 5,dec.mark = ",", mark = ".")

t_html

saveWidget(t_html, file = paste0(dir_Resultados,"/tablas_Col_def/Restauracion_dept_Zona_Unidad.html"))##


