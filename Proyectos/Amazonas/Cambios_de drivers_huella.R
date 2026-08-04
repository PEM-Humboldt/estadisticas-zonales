library(terra)

# =========================
# 1. Cargar raster
# =========================
lu_2024   <- rast(paste0(dir_Resultados, "/LUA2024.tif"))
lu_2025   <- rast(paste0(dir_Resultados, "/LUA2025.tif"))

frag_2024 <- rast(paste0(dir_Resultados, "/fragA2024.tif"))
frag_2025 <- rast(paste0(dir_Resultados, "/fragAregion20242025.tif"))
plot(frag_2024)
plot(frag_2025)

vias_2024 <- rast(paste0(dir_Resultados, "/ViasA2024old.tif"))
vias_2025 <- rast(paste0(dir_Resultados, "/ViasA2025.tif"))

# =========================
# 2. Calcular cambios
# =========================
cambio_lu   <- lu_2025 - lu_2024
cambio_frag <- frag_2025 - frag_2024
cambio_vias <- vias_2025 - vias_2024

cambio_vias <- crop(cambio_vias,cambio_lu)
cambio_vias <- mask(cambio_vias,cambio_lu)


cambio_frag <- crop(cambio_frag,cambio_lu)
cambio_frag <- mask(cambio_frag,cambio_lu)

plot(cambio_lu)
plot(cambio_frag)
plot(cambio_vias)
# =========================
# 3. Métricas de cambio
# =========================

# Promedio del cambio
mean_lu   <- global(cambio_lu, mean, na.rm=TRUE)[1]
mean_frag <- global(cambio_frag, mean, na.rm=TRUE)[1]
mean_vias <- global(cambio_vias, mean, na.rm=TRUE)[1]

# Número de píxeles que cambiaron
pix_luu   <- global(cambio_lu > 0, sum, na.rm=TRUE)[1]
pix_fragu <- global(cambio_frag> 0, sum, na.rm=TRUE)[1]
pix_viasu <- global(cambio_vias> 0, sum, na.rm=TRUE)[1]

# Número de píxeles que cambiaron
pix_lud   <- global(cambio_lu < 0, sum, na.rm=TRUE)[1]
pix_fragd <- global(cambio_frag < 0, sum, na.rm=TRUE)[1]
pix_viasd <- global(cambio_vias < 0, sum, na.rm=TRUE)[1]



# Número de píxeles que cambiaron
pix_lu   <- global(cambio_lu != 0, sum, na.rm=TRUE)[1]
pix_frag <- global(cambio_frag != 0, sum, na.rm=TRUE)[1]
pix_vias <- global(cambio_vias != 0, sum, na.rm=TRUE)[1]

# Magnitud total del cambio (más importante)
total_lu   <- global(abs(cambio_lu), sum, na.rm=TRUE)[1]
total_frag <- global(abs(cambio_frag), sum, na.rm=TRUE)[1]
total_vias <- global(abs(cambio_vias), sum, na.rm=TRUE)[1]

# =========================
# 4. Resumen en tabla
# =========================
resultados <- data.frame(
  Variable = c("LU", "Fragmentación", "Vías"),
  Promedio_Cambio = c(mean_lu$mean, mean_frag$mean, mean_vias$mean),
  Pixeles_Que_Cambiaronu = c(pix_luu$sum, pix_fragu$sum, pix_viasu$sum),
  Pixeles_Que_Cambiarond = c(pix_lud$sum, pix_fragd$sum, pix_viasd$sum),
  Pixeles_Que_Cambiaron = c(pix_lu$sum, pix_frag$sum, pix_vias$sum),
  Cambio_Total = c(total_lu$sum, total_frag$sum, total_vias$sum)
)

print(resultados)

# =========================
# 5. ¿Cuál cambió más?
# =========================
mayor_cambio <- resultados[which.max(resultados$Cambio_Total), ]
print("Variable con mayor cambio:")
print(mayor_cambio)