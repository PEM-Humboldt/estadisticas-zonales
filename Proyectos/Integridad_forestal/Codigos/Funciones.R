#' @title Cagar y proyectar capas
#'
#' @description Lee capas vector y las unifica a una proyección determinada
#' @details \code{st_data} used in \code{\link{dec_celltype}} must be a \code{matrix} object, each column representing a spot, each row representing a gene.
#' @return A matrix.
#' @export
#' @examples st_data_demo <- demo_st_data()

CargarProyectar<-function(archivo_Vector){
  
  capa<-st_read(archivo_Vector)
  
  if (st_crs(capa)$epsg != st_crs(SisRef)) {
    print(st_crs(capa)$epsg != SisRef)
    capa<- st_transform(capa, crs= SisRef)
    
  } else 
    return(capa)
}



#' @title Cagar y proyectar capas
#'
#' @description Lee capas vector y las unifica a una proyección determinada
#' @details \code{st_data} used in \code{\link{dec_celltype}} must be a \code{matrix} object, each column representing a spot, each row representing a gene.
#' @return A matrix.
#' @export
#' @examples st_data_demo <- demo_st_data()

rasterizar<-function(x){
  if (file.exists(raster_paths[x])) {
    r_capa <- rast(raster_paths[x])
  } else {
    # Si el raster no existe, rasterizar y guardar el resultado
    r_capa <- capas_st[[x]] %>%
      rasterize(y = r_base, field =atributo_rast [x], filename=raster_paths[x])
    
    # Guardar el raster resultante en un archivo
    #  writeRaster(r_Col, raster_path)
  }
}




#' @title prepara la capa al área de estudio
#'
#' @description Demo data of st_data.
#' @details \code{st_data} used in \code{\link{dec_celltype}} must be a \code{matrix} object, each column representing a spot, each row representing a gene.
#' @return A matrix.
#' @export
#' @examples st_data_demo <- demo_st_data()


definicionAOI<-function(capa, i){
  aoi<- capa %>% crop(list_deptos[[i]])%>% mask(list_deptos[[i]])
}




# ejecuta la estadística zonal y exporta el resultado

zonalAnalisis<-function(capa, tipo ){
  capa_intg<-zonal(r_Intg0_aoi, capa, fun= "mean", na.rm=T )%>% mutate(Integridad=round(Integridad))%>% arrange(desc(Integridad))
 # write_excel_csv2(capa_intg, paste0("Resultados/tablas departamento/", Nombre_dept, tipo))# corregir ruta
  return (capa_intg)
}

# marca con el nombre del departmento el paquete conteniendo la capa en la  lista 
listaMascaras<- function(lista,capa){
  
  lista[[i]]<- capa
  names(lista)[i]<-Nombre_dept
  return (lista)
  
}


# Calcular estadísticas zonales y las mascaras de zonas para integridad
#capa<-raster_capas[[2]]

zonalMask <- function(capa){
  cat(names(capa))
  raster_aoi<-definicionAOI(capa,i)
  
  if( !all(is.na(values(raster_aoi)))){
  
  raster_intg<-data.frame( Dpto=Nombre_dept,zonalAnalisis( raster_aoi))
  print("entre")
  
  }else{
    raster_intg<-data.frame( )
  }
  ## Para estadísticos globales ####
  
  # Se usa r_Intg_aoi porque se necesitan todos lo pixels para el cálculo de áreas
  # máscaras de capas para el boxplot 
  
  mask_Int_raster<-mask(r_Intg0_aoi,raster_aoi ) 
  
  return(list(raster_intg,mask_Int_raster))
  
}




# calcular estatisticas globales por departamentos
# función para calular mediana
mediana_fun <- function(x) {
  median(x, na.rm = TRUE)
}

# función para todas las estadísticas

StatRaster<-function(capa){
  
  resumen <- terra::global(capa, fun = c("mean","std"), na.rm = TRUE)
  
  resumen_median <- global(capa, fun = mediana_fun)
  
  resumen <- cbind(Nombre_dept,resumen[1], resumen_median,resumen[2])
  # Renombrar las columnas para que tengan nombres más descriptivos
  names(resumen) <- c("Dpto","Promedio", "Mediana", "Desviación estandar")
  
  return(resumen)
  
}

