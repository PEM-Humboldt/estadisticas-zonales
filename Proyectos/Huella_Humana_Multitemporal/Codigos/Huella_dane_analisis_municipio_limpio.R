# Título: IHEH en la altillanura
#
# Descripción: análisis multitemporal de la huella espacial humana por municipios Cruzando con la formación de valor agregado del Dane
#
# Autor(es): Alejandra Narváez Vallejo
#
# Por hacer o  corregir:

## Clasificar la huella por susus rangos . Verificar los rangos de la reclasiisifcacion, se incluye el mayor o el menor
## quedé en los gráficos, revisar dimensiones
## falta hacer el readme

# Por hacer o  corregir: Observaciones

## Código en desarrollo


#*******************************************************************************
# librerías o dependencias -----------------------------------------------------
#*******************************************************************************

## lectura de datos  ####

library (sf)
library(terra)
library(dplyr)
library(ggplot2)
#library(formattable)
library(readr)
#library(tidyr)
library(DT)
#library(alluvial)
library(ggalluvial)
library(htmlwidgets)
library(readxl)
library(purrr)
library(stringr)
library(tidyr)
library(corrplot)
#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(), "..", ".."))

dir_Datos_Or <- file.path("Datos/Originales")
dir_Datos_Intm<- file.path ("Datos/Intermedios")
dir_Resultados <- file.path("Resultados")


#**********************************************************
# Cargar Variables de importancia -------------------------
#**********************************************************

Años <- c("2018", "2020", "2022")
Años_numero <- 3


Unidad_analsis <- "Municipio" # Departamento" Municipio, Bioma
atributo_rast <- c("MpCodigo")

#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************
#*

archivo <- file.path(dir_Datos_Or,"DANE/VA_mun_corriente_milesmillonesCOP.xlsx")

# 1. Obtener nombres de las hojas
hojas <- excel_sheets(archivo)

# 2. Leer todas las hojas y unirlas
tabla_final <- map_dfr(hojas, function(h) {
  read_excel(archivo, sheet = h) %>%
    mutate(Año = h)  # agrega el nombre de la hoja como columna
}) %>% 
  rename( "VA"= "Valor agregado\r\n")


Stat_values <- read.csv2( paste0(dir_Resultados, "/IHEHcorine_stats_", Unidad_analsis,".csv"))
Stat_reclass <- read.csv2( paste0(dir_Resultados, "/IHEHcorine_clases_",Unidad_analsis,".csv"))


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## tablas iheh  ####

homogenizarCodigo <- function(Stat){
  str_pad(
    as.character(Stat[[atributo_rast]]),
    width = 5,
    side = "left",
    pad = "0"
  )
}

Stat_values[[atributo_rast]] <- homogenizarCodigo(Stat_values)
Stat_reclass[[atributo_rast]] <- homogenizarCodigo(Stat_reclass)


## completar con información económica ####


ecoMergeIheh <- function(Stat){
  merge(Stat,tabla_final, 
        by.x =c(atributo_rast, "Año"), 
        by.y=c( "Código Municipio", "Año") )
}


Eco_iheh <- ecoMergeIheh(Stat_values) %>% 
  mutate(VA1area= `Actividades primarias *`/area_cal_km2,
         VA2area= `Actividades secundarias **`/area_cal_km2,
         VA3area= `Actividades terciarias ***`/area_cal_km2, 
         VATarea= VA/area_cal_km2 ) %>% 
  
  mutate(
    Piso_termico = case_when(
      MpAltitud < 1000 ~ "Cálido",
      MpAltitud >= 1000 & MpAltitud < 2000 ~ "Templado",
      MpAltitud >= 2000 & MpAltitud < 3000 ~ "Frío",
      MpAltitud >= 3000 & MpAltitud < 4000 ~ "Páramo",
      MpAltitud >= 4000 ~ "Nival",
      TRUE ~ NA_character_
    ),
    Piso_termico = factor(Piso_termico, levels = c("Cálido", "Templado","Frío", "Páramo", "Nival")))


Eco_iheh_rcl <- ecoMergeIheh(Stat_reclass)%>% 
  mutate(VA1area= `Actividades primarias *`/area_cal_km2,
         VA2area= `Actividades secundarias **`/area_cal_km2,
         VA3area= `Actividades terciarias ***`/area_cal_km2, 
         VATarea= VA/area_cal_km2 )%>%
  mutate(
    Piso_termico = case_when(
      MpAltitud < 1000 ~ "Cálido",
      MpAltitud >= 1000 & MpAltitud < 2000 ~ "Templado",
      MpAltitud >= 2000 & MpAltitud < 3000 ~ "Frío",
      MpAltitud >= 3000 & MpAltitud < 4000 ~ "Páramo",
      MpAltitud >= 4000 ~ "Nival",
      TRUE ~ NA_character_
    ),
    Piso_termico = factor(Piso_termico, levels = c("Cálido", "Templado","Frío", "Páramo", "Nival")))


#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

#### prueba con las ponderadas por areas #########

Eco_iheh %>%
  tidyr::pivot_longer(cols = c(min, median, mean,max , sd),
                      names_to = "variable",
                      values_to = "valor") %>%
  ggplot(aes(x = VATarea, y = valor)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_grid(Año ~ variable, scales = "free_y") +
  labs(title = "Relación entre VA e indicadores IHEH por año",
       x = "Valor agregado (log)",
       y = "Valor")


### Cálculo correlaciones####

## Organizando la correlación####

# Preparar datos
df_plot <- Eco_iheh %>%
  select(- "Actividades primarias *", -"Actividades secundarias **", -"Actividades terciarias ***", -"VA") %>% 
  pivot_longer(
    cols = c(min, median, mean, max, sd),
    names_to = "variable",
    values_to = "valor_iheh"
  ) %>%
  pivot_longer(
    cols = c(VA1area, VA2area, VA3area, VATarea),
    names_to = "Actividades",
    values_to = "VA_Actividades"
  ) %>%
  mutate(
    variable = factor(variable,
                      levels = c("min", "median", "mean", "max", "sd")),
    log_VA = log10(VA_Actividades+0.000001)
  )

# Calcular correlaciones + significancia
df_cor <- df_plot %>%
  group_by(Actividades, variable, Año) %>%
  summarise(
    r_pearson  = cor(log_VA, valor_iheh, method = "pearson", use = "complete.obs"),
    p_pearson  = cor.test(log_VA, valor_iheh, method = "pearson")$p.value,
    r_spearman = cor(log_VA, valor_iheh, method = "spearman", use = "complete.obs"),
    p_spearman = cor.test(log_VA, valor_iheh, method = "spearman")$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    sig_p = ifelse(p_pearson < 0.05, "*", ""),
    sig_sp = ifelse(p_spearman < 0.05, "*", "")
  )


df_cor_text <- df_cor %>% filter(Año==2018)

# Gráfica con asterisco de significancia
df_plot %>% 
  filter(Año==2018) %>% 
  ggplot( aes(x = VA_Actividades+0.000001, y = valor_iheh)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  facet_grid(Actividades ~ variable, scales = "free_y") +
  geom_text(
    data = df_cor_text,
    aes(
      x = Inf, y = Inf,
      label = paste0("r=", round(r_pearson, 1),sig_p,
                     "\nρ=", round(r_spearman, 1),
                     sig_sp)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Relación entre VA (log) e IHEH con correlaciones y significancia",
    x = "VA (log10)",
    y = "Valor IHEH"
  )


#### corplot #######

cor_mat <-df_cor%>% 
  
  filter(Año==2022) %>% 
  select(Actividades,variable,r_pearson) %>% 
  tidyr::pivot_wider(
    names_from = variable,
    values_from = r_pearson
  ) %>%
  tibble::column_to_rownames("Actividades") %>%
  as.matrix()



corrplot(cor_mat, 
         method = "circle",
         #col=col,
         col=brewer.pal(n = 8, name = "RdYlBu"),
         col.lim=c(0.3,1),
         is.corr=F,
         tl.col = "black",
         tl.srt = 45)



# Calcular correlaciones + significancia + altura ####

df_corA <- df_plot %>%
  
  filter(variable %in% c("sd", "mean")) %>% 
  
  group_by(Actividades, variable, Año, Piso_termico) %>%
  summarise(
    r_pearson  = cor(log_VA, valor_iheh, method = "pearson", use = "complete.obs"),
    p_pearson  = cor.test(log_VA, valor_iheh, method = "pearson")$p.value,
    r_spearman = cor(log_VA, valor_iheh, method = "spearman", use = "complete.obs"),
    p_spearman = cor.test(log_VA, valor_iheh, method = "spearman")$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    sig_p = ifelse(p_pearson < 0.05, "*", ""),
    sig_sp = ifelse(p_spearman < 0.05, "*", "")
  )


# Gráfica con asterisco de significancia


df_cor_text <- df_corA %>% 
  filter(Año==2018 & Actividades == "VATarea")

#install.packages("ggpmisc")
library(ggpmisc)

df_plot %>% 
  filter(Año==2018 & Actividades == "VATarea" & variable %in% c("mean", "sd")) %>% 
  ggplot(aes(x = VA_Actividades + 0.000001, y = valor_iheh)) +
  
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  
  facet_grid(Piso_termico ~ variable) +
  
  # ✅ Ajuste lineal
  geom_smooth(method = "lm", se = FALSE) +
  
  # ✅ Fórmula del modelo en la gráfica
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    label.x = "right",
    label.y = "bottom",
    size = 3
  ) +
  
  # Correlaciones que ya tienes
  geom_text(
    data = df_cor_text,
    aes(
      x = Inf, y = Inf,
      label = paste0(
        "r=", round(r_pearson, 1), sig_p
      )
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "Relación entre VA (log) e IHEH con correlaciones y fórmula lineal",
    x = "VA (log10)",
    y = "Valor IHEH"
  )


## pisos y tipos de actividades

df_plot %>% 
  filter(Año==2018 & variable %in% c("mean")) %>% 
  ggplot(aes(x = VA_Actividades + 0.000001, y = valor_iheh)) +
  
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  
  facet_grid( Piso_termico ~ Actividades
    #vars(Piso_termico,                  , Actividades)
    ) +
  
  # ✅ Ajuste lineal
  geom_smooth(method = "lm", se = FALSE) +
  
  # ✅ Fórmula del modelo en la gráfica
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    label.x = "right",
    label.y = "bottom",
    size = 3
  ) +
  
  # Correlaciones que ya tienes
  geom_text(
    data = df_cor_text,
    aes(
      x = Inf, y = Inf,
      label = paste0(
        "r=", round(r_pearson, 1), sig_p
      )
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "Relación entre VA (log) e IHEH con correlaciones y fórmula lineal",
    x = "VA (log10)",
    y = "Valor IHEH"
  )


## ECO_iheh_rcl #######

### Cálculo correlaciones####

## Organizando la correlación####

# Preparar datos
df_plot_rcl <- Eco_iheh_rcl %>%
  select(- "Actividades primarias *", -"Actividades secundarias **", -"Actividades terciarias ***", -"VA") %>% 
  pivot_longer(
    cols = c(VA1area, VA2area, VA3area, VATarea),
    names_to = "Actividades",
    values_to = "VA_Actividades"
  ) %>%
  mutate(
    Categorías = factor(Categorías,
                        levels = c("Natural", "Bajo", "Medio", "Alto", "Muy Alto")),
    
    log_VA = log10(VA_Actividades+0.000001)
  )


# Calcular correlaciones + significancia
df_cor_rcl <- df_plot_rcl %>%
  group_by(Actividades, Categorías, Año) %>%
  summarise(
    r_pearson  = cor(log_VA, Porcentaje, method = "pearson", use = "complete.obs"),
    p_pearson  = cor.test(log_VA, Porcentaje, method = "pearson")$p.value,
    r_spearman = cor(log_VA, Porcentaje, method = "spearman", use = "complete.obs"),
    p_spearman = cor.test(log_VA, Porcentaje, method = "spearman")$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    sig_p = ifelse(p_pearson < 0.05, "*", ""),
    sig_sp = ifelse(p_spearman < 0.05, "*", "")
  )

df_cor_rcl_text <- df_cor_rcl %>% filter(Año==2018)

df_plot_rcl %>%
  filter(Año==2018) %>% 
  
  ggplot(aes(x = VA_Actividades+0.000001, y = Porcentaje)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  facet_grid(Actividades ~ Categorías)+
  geom_text(
    data = df_cor_rcl_text,
    aes(
      x = Inf, y = Inf,
      label = paste0("r=", round(r_pearson, 1),sig_p,
                     "\nρ=", round(r_spearman, 1),
                     sig_sp)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Relación entre VA (log) e IHEH con correlaciones y significancia",
    x = "VA (log10)",
    y = "Porcentaje de área"
  )


#### corplot #######

# Correlación por actividades

cor_mat_rcl <-df_cor_rcl%>% 
  
  filter(Año==2022) %>% 
  select(Actividades,Categorías,r_pearson) %>% 
  tidyr::pivot_wider(
    names_from = Categorías,
    values_from = r_pearson
  ) %>%
  tibble::column_to_rownames("Actividades") %>%
  as.matrix()


library(RColorBrewer)
corrplot(cor_mat_rcl, 
         method = "circle",
         #col=col,
         col=brewer.pal(n = 8, name = "RdYlBu"),
         
         col.lim=c(-0.9,0.9),
         is.corr=F,
         tl.col = "black",
         tl.srt = 45)

# Correlación por años

cor_mat_rcl <-df_cor_rcl%>% 
  
  filter(Actividades=="VATarea") %>% 
  select(Año,Categorías,r_pearson) %>% 
  tidyr::pivot_wider(
    names_from = Categorías,
    values_from = r_pearson
  ) %>%
  tibble::column_to_rownames("Año") %>%
  as.matrix()



corrplot(cor_mat_rcl, 
         method = "circle",
         #col=col,
         col=brewer.pal(n = 8, name = "RdYlBu"),
         
         col.lim=c(-0.9,0.9),
         is.corr=F,
         tl.col = "black",
         tl.srt = 45)



## classes_ altura+ correlaciones ####

### Cálculo correlaciones####

## Organizando la correlación####


# Calcular correlaciones + significancia
df_cor_rclA <- df_plot_rcl %>%
  group_by(Actividades, Categorías, Año, Piso_termico) %>%
  summarise(
    r_pearson  = cor(log_VA, Porcentaje, method = "pearson", use = "complete.obs"),
    p_pearson  = cor.test(log_VA, Porcentaje, method = "pearson")$p.value,
    r_spearman = cor(log_VA, Porcentaje, method = "spearman", use = "complete.obs"),
    p_spearman = cor.test(log_VA, Porcentaje, method = "spearman")$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    sig_p = ifelse(p_pearson < 0.05, "*", ""),
    sig_sp = ifelse(p_spearman < 0.05, "*", "")
  )



# Gráfica con asterisco de significancia


df_cor_rctext <- df_cor_rclA %>% 
  filter(Año==2018 & Actividades == "VATarea")



df_plot_rcl %>% 
  filter(Año==2018 & Actividades == "VATarea") %>% 
  ggplot(aes(x = VA_Actividades+0.000001 , y = Porcentaje)) +
  
  geom_point(alpha = 0.1) +
  
  scale_x_log10() +
  facet_grid(Piso_termico ~ Categorías) +
  
  # ✅ Ajuste lineal
  geom_smooth(method = "lm", se = FALSE) +
  
  # ✅ Fórmula del modelo en la gráfica
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    label.x = "right",
    label.y = "bottom",
    size = 3
  ) +
  
  # Correlaciones que ya tienes
  geom_text(
    data = df_cor_rctext,
    aes(
      x = Inf, y = Inf,
      label = paste0(
        "r=", round(r_pearson, 1), sig_p
      )
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "Relación entre VA (log) e IHEH con correlaciones y fórmula lineal",
    x = "VA (log10)",
    y = "Porcentaje de área"
  )

## entrar en detalle en piso calido ####

df_plot_rcl_calido <- df_plot_rcl %>% 
  filter(MpAltitud<=1000 & Actividades =="VATarea") %>% 
  mutate(
    rango_altura = cut(
      MpAltitud,
      breaks = seq(0, 1000, by = 250),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("0-250", "250-500", "500-750", "750-1000")
    )
  )


df_cor_rclAC <- df_plot_rcl_calido %>%
  group_by(Actividades, Categorías, Año, rango_altura) %>%
  summarise(
    
    # número de pares válidos
    n = sum(complete.cases(log_VA, Porcentaje)),
    
    # -------- Pearson --------
    r_pearson = ifelse(
      n >= 3,
      cor(log_VA, Porcentaje, method = "pearson", use = "complete.obs"),
      NA_real_
    ),
    
    p_pearson = ifelse(
      n >= 3,
      cor.test(log_VA, Porcentaje, method = "pearson")$p.value,
      NA_real_
    ),
    
    # -------- Spearman --------
    r_spearman = ifelse(
      n >= 3,
      cor(log_VA, Porcentaje, method = "spearman", use = "complete.obs"),
      NA_real_
    ),
    
    p_spearman = ifelse(
      n >= 3,
      cor.test(log_VA, Porcentaje, method = "spearman")$p.value,
      NA_real_
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    sig_p  = ifelse(!is.na(p_pearson)  & p_pearson  < 0.05, "*", ""),
    sig_sp = ifelse(!is.na(p_spearman) & p_spearman < 0.05, "*", "")
  )

df_cor_rclAC_text <- df_cor_rclAC %>% 
  filter(Año==2018 & Actividades == "VATarea")



df_plot_rcl_calido %>% 
  filter(Año==2018 & Actividades == "VATarea") %>% 
  ggplot(aes(x = VA_Actividades+0.000001 , y = Porcentaje)) +
  
  geom_point(alpha = 0.1) +
  
  scale_x_log10() +
  facet_grid(rango_altura ~ Categorías) +
  
  # ✅ Ajuste lineal
  geom_smooth(method = "lm", se = FALSE) +
  
  # ✅ Fórmula del modelo en la gráfica
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    label.x = "right",
    label.y = "bottom",
    size = 3
  ) +
  
  # Correlaciones que ya tienes
  geom_text(
    data = df_cor_rclAC_text,
    aes(
      x = Inf, y = Inf,
      label = paste0(
        "r=", round(r_pearson, 1), sig_p
      )
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "Relación entre VA (log) e IHEH con correlaciones y fórmula lineal",
    x = "VA (log10)",
    y = "Porcentaje de área"
  )



#****************************************************************************
# cambio huella ####
#****************************************************************************
## ECO_iheh #######

#### prueba con las ponderadas por areas #########

### Cálculo correlaciones####

## Organizando la correlación####
# Parece que con el cambio no se pueden establecer con relaciones directas entre la variable de valor entregado y promedio de la huella ni tampoco se pueden establecer las relaciones entre la variable valor agregado y las diferentes cambios en las coberturas de las categorías de intensidad de HUELLA

# Preparar datos


tabla_cambios <- Eco_iheh %>% 
  select(Departamento,MpCodigo, MpNombre, MpCategor, MpAltitud, mean, VATarea, VA1area, VA2area, VA3area, Año, Piso_termico) %>% 
  pivot_wider(
    names_from = Año,
    values_from = c(mean, VATarea, VA1area, VA2area, VA3area),
    names_sep = "_"
  ) %>%
  mutate(
    # Cambios absolutos en mean
    Dmean_18_20 = mean_2020 - mean_2018,
    Dmean_20_22 = mean_2022 - mean_2020,
    Dmean_18_22 = mean_2022 - mean_2018,
    
    # Porcentaje de cambio en mean
    Pmean_18_20 = ifelse(mean_2018 == 0, NA, (Dmean_18_20 / mean_2018) * 100),
    Pmean_20_22 = ifelse(mean_2020 == 0, NA, (Dmean_20_22 / mean_2020) * 100),
    Pmean_18_22 = ifelse(mean_2018 == 0, NA, (Dmean_20_22 / mean_2018) * 100),
    
    # Cambios absolutos en VATarea
    Dvt_18_20 = VATarea_2020 - VATarea_2018,
    Dvt_20_22 = VATarea_2022 - VATarea_2020,
    Dvt_18_22 = VATarea_2022 - VATarea_2018,
    
    # Porcentaje de cambio en VATarea
    Pvt_18_20 = ifelse(VATarea_2018 == 0, NA, (Dvt_18_20 / VATarea_2018) * 100),
    Pvt_20_22 = ifelse(VATarea_2020 == 0, NA, (Dvt_20_22 / VATarea_2020) * 100),
    Pvt_18_22 = ifelse(VATarea_2018 == 0, NA, (Dvt_20_22 / VATarea_2018) * 100),
    
    # relacion de cambio
    
        D = ifelse(Pvt_18_22 == 0, NA, Pmean_18_22 / Pvt_18_22),
        
        categoria_cambio = case_when(
          
          # 🌟 Ideal
          Pvt_18_22 > 0 & Pmean_18_22 < 0 ~ "Desacoplamiento absoluto (mejor)",
          
          # Crecimiento con presión
          #Pvt_18_22 > 0 & Pmean_18_22 > 0 & D < 1 ~ "Desacoplamiento relativo (maso)",
          #Pvt_18_22 > 0 & Pmean_18_22 > 0 & D >= 1 ~ "Crecimiento con alta presión (medio  mal)" ,
          Pvt_18_22 > 0 & Pmean_18_22 > 0  ~ "Desacoplamiento relativo (maso)",
          
          
          # Contracción general
          Pvt_18_22 < 0 & Pmean_18_22 < 0 ~ "Declive económico y mejora ambiental",
          
          # 🚨 Peor escenario
          Pvt_18_22 < 0 & Pmean_18_22 > 0 ~ "Degradación sin desarrollo (peor)",
          
          TRUE ~ "Sin clasificación"
        )
      )

revisar <- tabla_cambios %>% 
  filter(categoria_cambio =="Desacoplamiento absoluto (mejor)")
save(revisar,file= file.path(dir_Resultados,"mun_desacoplados.csv"))

save(tabla_cambios, file= file.path(dir_Resultados,"tabla_cambios.RData"))
table(tabla_cambios$categoria_cambio)

tabla_cambios %>% 
  count( categoria_cambio,Piso_termico) %>% 

  
ggplot(
       aes(y = categoria_cambio,
           x = n,
           fill = Piso_termico)) +
  geom_col(position = "fill", width = 0.7) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    y = "",
    x = "Proporción",
    fill = "Piso térmico"
  ) +
  theme_bw(base_size = 13)
  
  
    
    
ggplot(tabla_cambios) +
  geom_bar(aes(y=categoria_cambio))
  

plot(log(Pmean_18_22) ~ log(Pvt_18_22), data = tabla_cambios)



# visualizar 

# Municipio
ruta_archivo <- file.path(dir_Datos_Or,                     "MUNICIPIOS/Carto100000_Colombia_DI_2022_gpkg/Carto100000_Colombia_DI_2022.gpkg")
st_layers(ruta_archivo)

# cargar y proyectar en el sistema de referencia definido
capas_st <- st_read(ruta_archivo,layer="Limite_Municipal_Poligono") 
dpto <- st_read(file.path(dir_Datos_Or ,"MGN2023_DPTO_POLITICO/MGN_ADM_DPTO_POLITICO.shp")) %>% st_transform(st_crs(capas_st))

capas_descople <- capas_st %>% 
  filter(MpNombre %in% revisar$MpNombre)


plot(st_geometry(dpto))

plot(
  capas_descople,
  add = TRUE,
  col = rgb(0.5, 0.7, 1, 0.4),   # azul claro con transparencia
  border = "blue",              # borde azul
  lwd = 0.7                     # línea más delgada
)
plot(st_geometry(dpto), add=T, col=NA )


#### grafica todas las categorias


tc_x_unir <- tabla_cambios %>% 
  select(MpCodigo,MpNombre,categoria_cambio)

capas_st <- merge(capas_st, tc_x_unir)




capas_st$categoria_cambio <- factor(capas_st$categoria_cambio, levels=c(  "Desacoplamiento absoluto (mejor)" ,
                                                                             "Desacoplamiento relativo (maso)",
                                                                             "Degradación sin desarrollo (peor)" ,
                                                                             "Declive económico y mejora ambiental"))

ggplot() +
  geom_sf(data = capas_st, aes(fill = categoria_cambio), color = "blue") +
  geom_sf(data = dpto, fill = NA, color = "black", linewidth = 0.1)+
  scale_fill_manual(values = c(
    "Desacoplamiento absoluto (mejor)" = "#1a9641",
    "Desacoplamiento relativo (maso)" = "#fdae61",
    "Degradación sin desarrollo (peor)" = "#d7191c",
    "Declive económico y mejora ambiental" = "#2c7bb6"

  )) +
  theme_minimal()


