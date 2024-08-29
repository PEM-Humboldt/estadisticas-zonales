# falta ver como  se pude paralelizar


#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

list_deptos<- Col %>% split(., .$dpto_ccdgo)

# Construcción de listas

list_rasters<- list()

df_global<- data.frame()

df_runap<-data.frame()
df_negros<-data.frame()
df_resguardos<-data.frame()
df_campesino<-data.frame()


i=2


#for ( i in c(1:27,29:length(list_deptos))){
  
for ( i in 1:10){  
  
  
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

