# Preparación de tabla resumen

areas <- read_csv2("Resultados/tablas_Col_def/NoEspecial.csv")[,1:2]
areas1 <- read_csv2("Resultados/tablas_Col_def/CNegras.csv")[,1:2]
areas2 <- read_csv2("Resultados/tablas_Col_def/Resguardos.csv")[,1:2]
areas3 <- read_csv2("Resultados/tablas_Col_def/Runap.csv")[,1:2]
areas4 <- read_csv2("Resultados/tablas_Col_def/RCampesinas.csv")[,1:2]

# crear una tabla con las areas de todas la regiones y el area total del departamento

areas0 <- list(areas, areas1, areas2, areas3, areas4)

tr <- areas0 %>%
  plyr::join_all(by = "Dpto")

names(tr) <- c(
  "Dpto",
  "NoEspecial",
  "CNegras",
  "Resguardos",
  "RUNAP", 
  "RCampesinas"
)



write_excel_csv2(tr, file = "Resultados/tablas_Col_def/ComparativaPromedios.csv")

# tabla en formato html

datatable(tr,
          options = list(
            pageLength = 33,
            paging = F,
            language = list(search = "Buscar:")
          )
) %>%
  formatRound(2:6, 2)
