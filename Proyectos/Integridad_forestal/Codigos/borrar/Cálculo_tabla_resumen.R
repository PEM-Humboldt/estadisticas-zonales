# Preparación de tabla resumen

# Crear tabla con todas las áreas ####

areas <- readRDS("Resultados/tablas_Col/exC_area")
areas1 <- readRDS("Resultados/tablas_Col/area_negros")
areas2 <- readRDS("Resultados/tablas_Col/resguardos_area")
areas3 <- readRDS("Resultados/tablas_Col/runap_area")
areas4 <- readRDS("Resultados/tablas_Col/Campesinas_area")


# crear una tabla con las areas de todas la regiones y el area total del departamento

areas0 <- list(areas, areas1, areas2, areas3, areas4)

tr <- areas0 %>%
  plyr::join_all(by = "Dpto")

# reasignar nombres a las columnas

names(tr) <- c(
  "Dpto",
  "NoEspecial_ha",
  "CNegros_ha",
  "Resguardos_ha",
  "RUNAP_ha", 
  "RCampesinas_ha"
)

# calcular la totalidad de los pixeles conformando el departamento

tr <- tr %>%
  mutate(total = rowSums(select(., where(is.numeric)), na.rm = T))



# Creacion de la tabla


raster_values <- readRDS("Resultados/tablas_Col/runap_valores")
raster_values <- readRDS("Resultados/tablas_Col/resguardos_valores")
raster_values <- readRDS("Resultados/tablas_Col/exC_valores")
raster_values <- readRDS("Resultados/tablas_Col/campesinas_valores")
raster_values <- readRDS("Resultados/tablas_Col/negros_valores")


# elaborar la tabla
Col_resumen <- raster_values %>%
  group_by(Dpto) %>%
  summarise(
    Promedio = mean(Value),
    Mediana = median(Value),
    `Desviación estandar` = sd(Value)
  )


Col_resumen1 <- Col_resumen %>%
  left_join(tr) %>%
  mutate(`% de Area de Departamento` = round(CNegros_ha / total * 100, 1)) %>%
  dplyr::select(
    "Dpto",
    "Promedio",
    "Mediana",
    "Desviación estandar",
    "CNegros_ha",
    `% de Area de Departamento`
  )


names(Col_resumen1)


write_excel_csv2(Col_resumen1, file = "Resultados/tablas_Col_def/CNegras.csv")

# tabla en formato html

datatable(Col_resumen1,
  options = list(
    pageLength = 33,
    paging = F,
    language = list(search = "Buscar:")
  )
) %>%
  formatRound(2:4, 2)
