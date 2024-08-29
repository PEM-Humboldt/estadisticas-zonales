# Preparación de capas preliminar

# integridad

# creación de raster base
r_base<- rast(xmin=4330516, ymin= 1090467, xmax= 5684465, ymax= 2935222,  crs="EPSG:9377", res=30)

r_base[]<-1

writeRaster(r_base, "Datos/rbase_30_noSA.tif", datatype="INT1S", overwrite=T)

Intg<-rast(here::here("Datos", "Integridad ecologica NASA", "Integrity_inde_x100.tif")) %>% terra::project(rasterCol)
names(Intg)<- "Integridad"

writeRaster(Intg, "Datos/Integrida309377.tif", overwrite=T)


# Rasterizar
r_Col<- Col%>%
  rasterize( r_base, field="dpto_cnmbr")
writeRaster(r_Col, "Datos/Dep_COL.tif", overwrite=T)

r_runap<- runap%>%
  rasterize( r_base, field="nombre")
writeRaster(r_runap, "r_runap.tif", overwrite=T)

r_Negras<- Negras%>%
  rasterize( r_base, field="CNNOMBRE")
writeRaster(r_Negras, "Datos/r_Negras.tif", overwrite=T)

r_resguardo<- resguardo%>%
  rasterize( r_base, field="NOMBRE_RES")
writeRaster(r_resguardo, "Datos/r_resguardo.tif", overwrite=T)

r_campesinas<- campesinas%>%
  rasterize( r_base, field="Nombre")
writeRaster(r_campesinas, "Datos/r_campesinas.tif", overwrite=T)