#' @title Cargar y proyectar capas
#' @description Lee una capa vectorial desde un archivo y la transforma a una proyección determinada.
#' @details Esta función carga una capa vectorial desde un archivo y la transforma a una proyección especificada por el sistema de referencia de coordenadas `SisRef`. Si la capa ya está en la proyección deseada, no se realiza ninguna transformación.
#' @param archivo_Vector Una cadena de texto que representa la ruta al archivo de la capa vectorial que se va a cargar.
#' @param SisRef Un objeto de referencia de coordenadas (CRS) en formato EPSG que especifica la proyección deseada.
#' @return Un objeto de clase `sf` que representa la capa vectorial transformada a la proyección deseada.
#' @export
#' @examples

CargarProyectar<-function(archivo_Vector){
  
  capa<-st_read(archivo_Vector)
  
  if (st_crs(capa)$epsg != st_crs(SisRef)) {
    print(st_crs(capa)$epsg != SisRef)
    capa<- st_transform(capa, crs= SisRef)
    
  } else 
    return(capa)
}


#' @title Rasterizar una capa y guardar el resultado
#' @description Rasteriza una capa vectorial a partir de un índice especificado y guarda el raster resultante en un archivo. Si el archivo raster ya existe, lo carga en lugar de volver a rasterizarlo.
#' @details La función verifica si el archivo raster ya existe en la ruta especificada. Si el archivo existe, se carga utilizando `rast()`. Si no existe, la función rasteriza la capa especificada utilizando `rasterize()` y guarda el resultado en la ruta especificada por `raster_paths`. La función también guarda el raster resultante en un archivo utilizando `writeRaster()`.
#' @param x Un índice entero que especifica qué capa vectorial de la lista `capas_st` se debe rasterizar y qué ruta de archivo en `raster_paths` se debe utilizar.
#' @return Un objeto de clase `SpatRaster` que representa el raster resultante, ya sea cargado desde un archivo existente o recién creado.
#' @export
#' @examples

rasterizar<-function(x){
  if (file.exists(raster_paths[x])) {
    r_capa <- rast(raster_paths[x])
  } else {
    # Si el raster no existe, rasterizar y guardar el resultado
    r_capa <- capas_st[[x]] %>%
      rasterize(y = r_base, field =atributo_rast [x], filename=raster_paths[x])
    
    # Guardar el raster resultante en un archivo
      writeRaster(r_capa, raster_paths[x])
  }
  
  return(r_capa)
}




#' @title Prepara la capa al área de estudio
#' @description Recorta y enmascara una capa espacial a un área de interés específica (AOI).
#' @details Esta función toma una capa espacial y la recorta y enmascara de acuerdo a un área de interés definida en \code{list_deptos}. El área de interés se especifica mediante un índice \code{i} que selecciona un elemento de \code{list_deptos}.
#' @param capa Un objeto espacial (por ejemplo, una capa raster o vectorial) que se desea recortar y enmascarar.
#' @param i Un entero que indica el índice del área de interés en \code{list_deptos} a utilizar para recortar y enmascarar la capa.
#' @return Un objeto espacial recortado y enmascarado al área de interés especificada.
#' @export
#' @examples 


definicionAOI<-function(capa, i){
  aoi<- capa %>% crop(list_deptos[[i]])%>% mask(list_deptos[[i]])
}


#' @title Ejecuta la estadística zonal y exporta el resultado
#' @description Calcula la estadística zonal de una capa espacial y exporta el resultado como un archivo CSV.
#' @details Esta función aplica una función estadística (media) a una capa de entrada sobre una capa de integridad regional. Se realiza un redondeo en la columna "Integridad" y se ordena de forma descendente.
#' @param capa Un objeto espacial (raster) que se utilizará para el análisis zonal.
#' @param tipo Un texto que indica el tipo de análisis o el nombre del archivo de salida.
#' @return Un data frame con la integridad promedio para cada zona.
#' @export
#' @examples 

zonalAnalisis <- function(capa, tipo) {
  capa_intg <- zonal(r_Intg0_aoi, capa, fun = "mean", na.rm = TRUE) %>% 
    mutate(Integridad = round(Integridad)) %>% 
    arrange(desc(Integridad))
  # write_excel_csv2(capa_intg, paste0("Resultados/tablas departamento/", Nombre_dept, tipo)) # corregir ruta
  return(capa_intg)
}

#' @title Marca con el nombre del departamento el paquete conteniendo la capa en la lista
#' @description Añade una capa a una lista y asigna un nombre basado en el nombre del departamento.
#' @details Esta función agrega una capa espacial a una lista y nombra el elemento de la lista según el nombre del departamento correspondiente.
#' @param lista Una lista de capas espaciales.
#' @param capa Un objeto espacial (raster o vector) que se añadirá a la lista.
#' @return La lista actualizada con la capa añadida y nombrada.
#' @export
#' @examples 

listaMascaras <- function(lista, capa) {
  lista[[i]] <- capa
  names(lista)[i] <- Nombre_dept
  return(lista)
}

#' @title Calcular estadísticas zonales y las máscaras de zonas para integridad
#' @description Calcula estadísticas zonales y genera máscaras de zonas para una capa de integridad.
#' @details Esta función recorta una capa espacial a un área de interés y calcula las estadísticas zonales si existen valores en el área recortada.
#' @param capa Un objeto espacial (raster) utilizado para calcular estadísticas zonales.
#' @return Una lista que contiene un data frame con las estadísticas zonales y un raster de máscara.
#' @export
#' @examples 

zonalMask <- function(capa) {
  cat(names(capa))
  raster_aoi <- definicionAOI(capa, i)
  
  if (!all(is.na(values(raster_aoi)))) {
    raster_intg <- data.frame(Dpto = Nombre_dept, zonalAnalisis(raster_aoi))
    print("entre")
  } else {
    raster_intg <- data.frame()
  }
  
  # Máscara de capas para el boxplot
  mask_Int_raster <- mask(r_Intg0_aoi, raster_aoi) 
  
  return(list(raster_intg, mask_Int_raster))
}

#' @title Calcular estadísticas globales por departamentos
#' @description Calcula estadísticas globales como la media, mediana y desviación estándar para cada departamento.
#' @details Esta función utiliza métodos globales para calcular estadísticas de resumen sobre capas espaciales, agregando resultados por departamento.
#' @param capa Un objeto espacial (raster) utilizado para calcular estadísticas globales.
#' @return Un data frame con estadísticas globales por departamento.
#' @export
#' @examples 
#' # Supongamos que 'capa' es un raster de entrada.
#' resultado <- StatRaster(capa)
StatRaster <- function(capa) {
  resumen <- terra::global(capa, fun = c("mean", "std"), na.rm = TRUE)
  resumen_median <- global(capa, fun = mediana_fun)
  resumen <- cbind(Nombre_dept, resumen[1], resumen_median, resumen[2])
  
  # Renombrar las columnas para que tengan nombres más descriptivos
  names(resumen) <- c("Dpto", "Promedio", "Mediana", "Desviación estandar")
  
  return(resumen)
}

#' @title Función para calcular la mediana
#' @description Calcula la mediana de un vector numérico ignorando los valores NA.
#' @details Esta función es utilizada internamente por otras funciones para calcular la mediana de un conjunto de valores.
#' @param x Un vector numérico.
#' @return La mediana del vector numérico.
#' @export
#' @examples 

mediana_fun <- function(x) {
  median(x, na.rm = TRUE)
}

