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


#' # Rasterizar capas vectoriales y asignar niveles

if (file.exists(raster_paths)) {
  r_aoi <- rast(raster_paths)
} else {
  # Si el raster no existe, rasterizar y guardar el resultado
  r_aoi <- capas_st %>%
    rasterize(y = r_base,
              field = atributo_rast [1],
              filename = raster_paths)
  
}

levels(r_aoi) <- capas_st[c(atributo_rast, cat_rast)] %>% st_drop_geometry()


