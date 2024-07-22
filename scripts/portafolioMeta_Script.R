

# Cargar librerias ####
packages_list<-list("magrittr", "dplyr", "plyr", "pbapply", "rstac", "gdalcubes", "sf", "terra", "raster", "stringi", "ncdf4", "ggplot2")
lapply(packages_list, library, character.only = TRUE)   

Sys.setlocale(category = "LC_ALL", locale="Spanish") 



# Definir funciones ####
stac_metadata_function<-function(format){
  metadata<- jsonlite::fromJSON(format)$bands %>% purrr::map("attributes_band") %>%
    {lapply(names(.), function(x) {
      as.data.frame(.[[x]]) %>% {if(nrow(.)<1){.[1, "layer"]<-x;. }else{mutate(.,band =x)}}
    })} %>% rbind.fill()
}

stac_mask_function<-function(dir_colection, wkt_polygon, epsg_polygon, bbox_polygon, resolution,
                             aggregation = c("min", "max", "mean", "median", "first"),
                             join_layers=F,
                             join_fun= c("binary", "count", "sum", "min", "max", "mean", "median" )
){
  
  # Cargar coleccion
  layers <- list.files(dir_colection, "\\.tif$", recursive = TRUE, full.names = TRUE)
  json_colleciton_file <- list.files(dir_colection, "\\.json$", recursive = TRUE, full.names = TRUE)
  image_collection<- gdalcubes::create_image_collection(files= layers, format= json_colleciton_file)
  image_metadata<- stac_metadata_function(format = json_colleciton_file)
  
  # Establecer parametros del cubo
  cube_collection<- gdalcubes::cube_view(srs = epsg_polygon,  extent = list(t0 = gdalcubes::extent(image_collection)$t0, t1 = gdalcubes::extent(image_collection)$t1,
                                                                            left = bbox_polygon[1], right = bbox_polygon[3],
                                                                            top = bbox_polygon[4], bottom = bbox_polygon[2]),
                                         dx = resolution[1], dy = resolution[2], dt = "P1Y", aggregation = "first",
                                         keep.asp= T)
  
  # Cargar parametros de cubo
  cube <- gdalcubes::raster_cube(image_collection, cube_collection)
  cube_mask<- gdalcubes::filter_geom(cube, geom= wkt_polygon, srs = epsg_polygon )
  
  # Descargar cubo
  fn = tempfile(fileext = ".nc"); gdalcubes::write_ncdf(cube_mask, fn)
  nc <- nc_open(fn); vars <- names(nc$var)
  
  # Filtrar capas vacias
  nc <- nc_open(fn); vars <- names(nc$var)
  values_mask <- pblapply(vars[5:length(vars)], function(x) {
    freq<- data.frame(table(ncvar_get(nc, x))); if(nrow(freq)>0){data.frame(layer=x, value= freq$Var1, count= freq$Freq)}else{NULL}
  }   ) %>% rbind.fill()
  
  
  # Descargar cubo filtrado
  cube_filter<- select_bands(cube_mask, unique(values_mask$layer))
  fn2 = tempfile(fileext = ".nc"); gdalcubes::write_ncdf(cube_filter, fn2)
  filter_mask<- terra::rast(fn2)
  
  
  # Operaciones entre capas
  if(isTRUE(join_layers)){
    filter_mask<-{ if(join_fun %in% "binary"){
      sum(filter_mask, na.rm = T) %>% terra::classify( cbind(-Inf,Inf,1))
    } else {
      terra::app(filter_mask, join_fun, na.rm=T)
    } } %>% setNames(jsonlite::fromJSON(json_colleciton_file)[["description"]])
    metadata_stack<- dplyr::select(image_metadata, -band) %>% distinct() %>% dplyr::mutate( band= names(filter_mask))
  } else {
    metadata_stack<- list(data.frame(layer= seq_along(names(filter_mask)), band= names(filter_mask) ), image_metadata) %>% join_all()
  }
  
  return(list(stack= filter_mask, metadata_stack= metadata_stack))
}




## Cargar area de estudio ####
x<- sf::st_read("~/studyArea.shp")
  


# Estimaciones por portafolio ####

# 1 Definir area de estudio
vector_polygon <-  terra::vect(x) %>% terra::aggregate()  %>%  terra::project(sp::CRS( paste0("+init=epsg:", 3395) )) %>% terra::as.polygons()
wkt_polygon <- geom(vector_polygon, wkt=TRUE) %>% {  paste0( "MULTIPOLYGON (",paste(gsub("POLYGON ", "", .), collapse=  ", "),")")  }

epsg_polygon<- 3395; resolution<- 1000 # Ajustar parametros geograficos
resolution_crs<- raster::raster(raster::extent(seq(4)),crs= "+init=epsg:3395", res= resolution) %>% raster::projectRaster( crs = sf::st_crs(epsg_polygon)$proj4string ) %>% 
  raster::res()

vector_polygon<- terra::vect(wkt_polygon, crs= sf::st_crs(epsg_polygon)$proj4string ) 
crs_polygon<- terra::crs(vector_polygon)
box_polygon<-  sf::st_bbox(vector_polygon)


# 2 Cargar coleccion de portafolios
portfolios_collection<- stac_mask_function(dir_colection = "~/Catalogs/Biotablero_collections/Portfolios/Biotablero_portfolios_collection",
                                                wkt_polygon= wkt_polygon,
                                                epsg_polygon= crs_polygon,
                                                resolution= resolution_crs,
                                                aggregation= "first",
                                                join_layers= F,
                                                join_fun= "binary",
                                                bbox_polygon= box_polygon
)

# 3 Cargar colecciones de targets

# Listar jsons de colecciones disponibles
dirs_targets<- list.dirs(list.files("~/Biotablero_collections/Targets", "\\.json$", recursive = TRUE, full.names = TRUE)) %>% 
  dirname()
dirs_targets 

# definir parametros de ajuste de colecciones al area de estudio
setwd("~/Biotablero_collections/Targets")
list_parameters<- list(
  list(name= "Biotablero_targets_almacenamiento_agua_collection", join_layers= T, aggregation= "first", join_fun= "binary"),
  list(name= "Biotablero_targets_almacenamiento_carbono_collection", join_layers= T, aggregation= "max", join_fun= "sum"),
  list(name= "Biotablero_targets_conectividad_collection", join_layers= T, aggregation= "first", join_fun= "binary"),
  list(name= "Biotablero_targets_deforestacion_evitada_collection", join_layers= T, aggregation= "first", join_fun= "binary"),
  list(name= "Biotablero_targets_ecosistemas_amenazados_collection", join_layers= T, aggregation= "first", join_fun= "binary"),
  list(name= "Biotablero_targets_restauracion_collection", join_layers= T, aggregation= "first", join_fun= "binary")
)



# Cargar colecciones
targets_colleciton<- pblapply(list_parameters, function(x) {
  
  stac_mask_function(dir_colection = x$name,
                     wkt_polygon= wkt_polygon,
                     epsg_polygon= crs_polygon,
                     resolution= resolution_crs,
                     aggregation= x$aggregation,
                     join_layers= x$join_layers,
                     join_fun= x$join_fun,
                     bbox_polygon= box_polygon
  )
  
  
})

targets_metadata<- targets_colleciton %>% purrr::map("metadata_stack") %>% rbind.fill()
targets_spat_raster<- targets_colleciton %>% purrr::map("stack") %>% rast() 


# 4 Estimacion de metricas

# Estimación de metas en area de estudio
target_geofences<- as.data.frame(targets_spat_raster) %>% colSums(na.rm = T) %>% 
  {data.frame(band= names(.), target_geofence= .)} %>% list(targets_metadata) %>%  join_all() %>% dplyr::select(-c(band)) 

# Estimación de cruce de meta por portafolio en área de estudio

# Cruzar portafolios con targets
targets_ports<- pblapply(portfolios_collection$stack, function(x) {
  interseccion<- terra::mask(targets_spat_raster, x)
  
  port_data<- data.frame(band= names(x)) %>% list(portfolios_collection$metadata_stack) %>% join_all() %>% dplyr::select(-c(layer, band)) 

  intersection_data<- as.data.frame(interseccion) %>% colSums(na.rm=T) %>% 
    {data.frame(band= names(.), value_target= .)} %>% list(targets_metadata) %>%  join_all() %>% dplyr::select(-c("band"))  %>% 
    cbind(port_data)
  
  }) %>% rbind.fill()

targets_ports<- pblapply(portfolios_collection$stack, function(x) {
  interseccion<- terra::mask(targets_spat_raster, x)
}) %>% setNames(names(portfolios_collection$stack))

## Cargar todas las capas
#i<- names(portfolios_collection$stack)[1]
#j<- names(targets_spat_raster)[1]
#k<- targets_ports[[i]][[j]]

name<- "StudyArea"
  
name_dir<- "~/output/StudyArea"
portfolio_name<- file.path(name_dir, "portfolios"); dir.create(portfolio_name, recursive = T)
targets_name<- file.path(name_dir, "targets"); dir.create(targets_name, recursive = T)
intersect_name<- file.path(name_dir, "intersect"); dir.create(intersect_name, recursive = T)

lapply(names(portfolios_collection$stack), function(i) {setwd(portfolio_name) ; portfolios_collection$stack[[i]] %>% writeRaster(paste0(paste(c(name, "port", i), collapse = "_"), ".tif") %>% iconv('utf8','ascii/translit'), overwrite=T  )  })
lapply(names(targets_spat_raster), function(j) {setwd(targets_name) ; targets_spat_raster[[j]] %>% writeRaster(paste0(paste(c(name, "target", j), collapse = "_"), ".tif") %>% iconv('utf8','ascii/translit'), overwrite=T  ) })


estimaciones<- lapply(names(portfolios_collection$stack), function(i){  print(i)
  
  lapply(names(targets_spat_raster), function(j){ print(j)
    
    x<- portfolios_collection$stack[[i]] %>% stars::st_as_stars()
    y<- targets_spat_raster[[j]] %>% stars::st_as_stars()
    z<- targets_ports[[i]][[j]] %>% stars::st_as_stars()
    
    
    port_data<- portfolios_collection$stack[[i]] %>% as.data.frame() %>% colSums(na.rm=T) %>%  {.*(prod(resolution_crs)) /1000000}
    target_data<- targets_spat_raster[[j]] %>% as.data.frame() %>% colSums(na.rm=T) %>%  {sum(., na.rm=T)}
    intersect_data<- targets_ports[[i]][[j]] %>% as.data.frame() %>% colSums(na.rm=T) %>%  {sum(., na.rm=T)}
    
    
    study_area<- st_as_sf(vector_polygon)
    
    port_plot<-   ggplot() + geom_sf(data= study_area)+
      geom_stars(data = x,  show.legend = F)+ scale_fill_distiller(na.value = NA)+ 
      theme_void()  + ggtitle(paste("portfolio\n", i, ":\n", round(port_data), "km2"))+
      theme(plot.title = element_text(size = 8))
    
    target_plot<-   ggplot() + geom_sf(data= study_area)+
      geom_stars(data = y,  show.legend = (j %in% "Almacenamiento_de_carbono") )+ scale_fill_distiller(na.value = NA, direction = 1) +
      guides(fill = guide_legend(title = NULL))+  theme_void() +
      ggtitle(paste("target\n", j, ":\n", round(target_data), (if(j %in% "Almacenamiento_de_carbono"){"Gton"}else{"km2"})))+
      theme(plot.title = element_text(size = 8))
    
    
    summ_data<- data.frame(portfolio= i, target= j, unit= (if(j %in% "Almacenamiento_de_carbono"){"Gton"}else{"km2"}) ,
                           portafolio_unit_value = port_data,
                           target_unit_value= target_data,
                           intersect_unit_value= intersect_data,
                           intersect_prop = (intersect_data/target_data)
    ) %>% dplyr::mutate(perc_contribution_port_to_target= paste0(round((intersect_prop)*100), "%") )
    
    
    
    intersect_plot<-  ggplot() +geom_sf(data= study_area)+
      geom_stars(data = z,  show.legend = (j %in% "Almacenamiento_de_carbono") )+ scale_fill_distiller(na.value = NA, direction = 1) +
      guides(fill = guide_legend(title = NULL)) +   theme_void() +
      ggtitle(paste("intersect", ":\n", round(intersect_data), (if(j %in% "Almacenamiento_de_carbono"){"Gton"}else{"km2"}), "\n", 
                    summ_data$perc_contribution_port_to_target, "port in target"))+
      theme(plot.title = element_text(size = 8))
    
    plot_data<- ggarrange(plotlist = list(port_plot, target_plot, intersect_plot), nrow = 1)
    
    

    setwd(intersect_name)
    targets_ports[[i]][[j]] %>% writeRaster(paste0(paste(c(name,i, j), collapse = "_"), ".tif") %>% iconv('utf8','ascii/translit'), overwrite=T)

    
    
    ggsave( paste0(paste(c("summ_data", name,i, j), collapse = "_"), ".jpeg"),
            plot=  plot_data, limitsize = F, width = 8, height = 8)

    list(plot_data=plot_data, summ_data=summ_data)
    
    }) %>% setNames(paste0("target_", names(targets_spat_raster)))
  }) %>% setNames(paste0("port_", names(portfolios_collection$stack)))


summ_estimaciones<- purrr::map( unlist(estimaciones, recursive = F), "summ_data") %>% rbind.fill() %>% dplyr::mutate(sitio= name) %>% 
  dplyr::relocate("sitio", .before = 1)



final_list_plots<- unlist(estimaciones, recursive = F) 
graficos <- purrr::map( final_list_plots, "plot_data")

first_plot<- ggplot(summ_estimaciones, aes(x = target, y = intersect_prop, fill = portfolio, group=portfolio)) +
  geom_bar(stat = "identity", position = "dodge")+ scale_y_continuous(labels = percent_format(), limits = c(0,1))+
  geom_text(aes(y=0.8, label= paste("meta", round(target_unit_value), unit)), size= 3)+
  geom_text(aes(y=0, label = paste(portfolio, perc_contribution_port_to_target)), position = position_dodge(width = 0.9),  hjust = -0, size= 2.5)+
  ylab("Aporte de portafolio por meta") + ggtitle(paste0("Estimacion de indicadores de aporte de portafolio a metas: ", name))+
  theme(legend.position = "bottom", text = element_text(size = 9), axis.text.y = element_text(angle = 45, hjust = 1))+
  coord_flip()


# exportar resultados ####


# Plot inicial
setwd(name_dir)

pdf(paste0(paste(c("portfolios",2023, "estimaciones", name), collapse = "_"), ".pdf"))

print(first_plot)

for (i in 1:length(graficos)) {
  print(graficos[[i]])
}

dev.off()



# Tabla final
full_table<- list(target_geofences, targets_ports) %>% join_all() %>% dplyr::rowwise() %>% 
  dplyr::mutate(target_geofences= if(target_units_short == "km2"){
  target_geofence*(prod(resolution_crs)) /1000000}else{target_geofence},
  value_portfolio= value_target/target_geofence) %>% data.frame(st_drop_geometry(x[1,c("geofence_id", "geofence_name", "geofence_type")])) %>% 
  dplyr::relocate(c("geofence_id", "geofence_type", "geofence_name", "portfolio_id", "portfolio_name",
                    "target_id", "target_name", "value_target", "target_geofence", "value_portfolio" ), .after = 1) %>% 
  arrange(geofence_id, portfolio_id, target_id) %>% dplyr::mutate(portfolios_target_id= seq(nrow(.))) %>% dplyr::relocate("portfolios_target_id",1)


test_expert<- full_table %>% {data.frame(depto= .$geofence_name, portfolio_name= .$portfolio_name, portfolio_area_km2= .$target_geofence,
                               target_name= .$target_name, target_area_km2= .$value_target,
                               aporte_port_a_meta= .$value_portfolio
                                 )}


final_table<- dplyr::select(full_table, c("portfolios_target_id", "geofence_id", "geofence_type", "portfolio_id", "target_id", "value_target", "target_geofence", "value_portfolio"))

write.csv(summ_estimaciones, paste0(paste(c("portfolios",2023, "estimaciones", name), collapse = "_"), ".csv"))

write.csv(full_table, paste(st_drop_geometry(x[1,c("geofence_id", "geofence_name", "geofence_type")]), collapse = "_") %>% paste0("_full.csv"))
write.csv(final_table, paste(st_drop_geometry(x[1,c("geofence_id", "geofence_name", "geofence_type")]), collapse = "_") %>% paste0("_final.csv"))
