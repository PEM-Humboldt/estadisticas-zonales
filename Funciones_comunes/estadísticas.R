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

