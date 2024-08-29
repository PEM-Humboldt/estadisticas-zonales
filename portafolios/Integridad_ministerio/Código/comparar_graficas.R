#****************************************************************************
# Analisis por departamento ----------------------------
#****************************************************************************

exmask0[exmask0==0]<-NA

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


i=1


for ( i in c(1:27,29:length(list_deptos))){
  
  Nombre_dept<- list_deptos[[i]]$dpto_cnmbr
  print(Nombre_dept)
  
  ## Corte de área de estudio ####
  
  #r_Col_aoi<-definicionAOI(r_Col,i) # parece se obsoleto
  r_runap_aoi<-definicionAOI(r_runap,i)
  r_negros_aoi<-definicionAOI(r_Negras,i)
  r_resguardos_aoi<-definicionAOI(r_resguardo, i)
  r_campesinas_aoi<-definicionAOI(r_campesinas, i)
  r_Intg0_aoi<-definicionAOI(Intg_0, i)

  
   ## Para estadísticos globales ####
  
  # Se usa r_Intg_aoi porque se necesitan todos lo pixels para el cálculo de áreas
  # máscaras de capas para el boxplot 
  
  mask_IntRunap<-mask(r_Intg0_aoi,r_runap_aoi ) # temporalmente fuera
  mask_IntNegros<-mask(r_Intg0_aoi,r_negros_aoi )# temporalmente fuera
  mask_IntResguardo<-mask(r_Intg0_aoi,r_resguardos_aoi )# temporalmente fuera
  mask_IntCampesinas<-mask(r_Intg0_aoi,r_campesinas_aoi )
  exmask<- definicionAOI(exmask0, i)
  
  
  # construcción de contenedor
  
  Contenedor<-c(mask_IntRunap,mask_IntNegros,mask_IntResguardo,mask_IntCampesinas, exmask)
  
  names(Contenedor)<- c("RUNAP","CNegra","Resguardo","RCampesina", "NoEspecial")
  
  # Crear el box plot
  
  png(filename=paste0("Resultados/Gráficas Departamentos/",Nombre_dept), width=555)
  
  boxplot(Contenedor)
  
  dev.off()
  
  gc()
  
}
