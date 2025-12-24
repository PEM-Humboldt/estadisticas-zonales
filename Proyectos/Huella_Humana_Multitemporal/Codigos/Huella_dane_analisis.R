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
         VATarea= VA/area_cal_km2 )

Eco_iheh_rcl <- ecoMergeIheh(Stat_reclass)%>% 
  mutate(VA1area= `Actividades primarias *`/area_cal_km2,
         VA2area= `Actividades secundarias **`/area_cal_km2,
         VA3area= `Actividades terciarias ***`/area_cal_km2, 
         VATarea= VA/area_cal_km2 )

#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

## ECO_iheh #######
Eco_iheh %>% 
  filter(Año==2018) %>% 

  ggplot( aes(x = VA, y = mean)) +
  geom_point()




Eco_iheh %>%
  tidyr::pivot_longer(cols = c(min, median, mean,max , sd),
               names_to = "variable",
               values_to = "valor") %>%
  ggplot(aes(x = VA, y = valor)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_grid(Año ~ variable, scales = "free_y") +
  labs(title = "Relación entre VA e indicadores IHEH por año",
       x = "Valor agregado (log)",
       y = "Valor")


Eco_iheh %>%
  filter(Año==2018) %>% 
  tidyr::pivot_longer(cols = c(min, median, mean,max , sd),
                      names_to = "variable",
                      values_to = "valor_iheh") %>%
  tidyr::pivot_longer(cols = c(`Actividades primarias *`, `Actividades secundarias **`,`Actividades terciarias ***` ),
                      names_to = "Actividades",
                      values_to = "VA_Actividades")  %>%
  mutate(
    variable = factor(variable,
                      levels = c("min", "median", "mean", "max", "sd")))%>%
  
  ggplot(aes(x = VA_Actividades, y = valor_iheh)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_grid(Actividades ~ variable, scales = "free_y")+
  labs(title = "Relación entre VA * actividades e indicadores IHEH para 2018",
       x = "VA (log)",
       y = "Valor_iheh")

#### prueba con las ponderadas por areas #########


var_plot <- "sd"

Eco_iheh %>%
  filter(Año == 2018) %>%
  ggplot(aes(x = VATarea, y = .data[[var_plot]])) +
  geom_point() +
  #scale_x_sqrt()+
  scale_x_log10() +
  labs(title = var_plot)



Eco_iheh %>%
  filter(Año == 2018) %>%                                  # filtra el año
  tidyr::pivot_longer(cols = c(min, max, sd, median, mean),        # pasa a formato largo
                      names_to  = "variable",
                      values_to = "valor") %>%
  ggplot(aes(x = VATarea, y = valor)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Relación entre VA e indicadores IHEH (2018)",
       x = "Valor agregado (log)",
       y = "Valor")



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


Eco_iheh %>%
  tidyr::pivot_longer(cols = c(min, median, mean,max , sd),
                      names_to = "variable",
                      values_to = "valor") %>%
  ggplot(aes(x = VA3area, y = valor)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_grid(Año ~ variable, scales = "free_y") +
  labs(title = "Relación entre VA e indicadores IHEH por año",
       x = "Valor agregado (log)",
       y = "Valor")


Eco_iheh %>%
  filter(Año==2018) %>% 
  tidyr::pivot_longer(cols = c(min, median, mean,max , sd),
                      names_to = "variable",
                      values_to = "valor_iheh") %>%
  tidyr::pivot_longer(cols = c(VA1area, VA2area,VA3area,VATarea ),
                      names_to = "Actividades",
                      values_to = "VA_Actividades") %>%
  mutate(
    variable = factor(variable,
                      levels = c("min", "median", "mean", "max", "sd")))%>%
  ggplot(aes(x = VA_Actividades, y = valor_iheh)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_grid(Actividades ~ variable, scales = "free_y")+
  labs(title = "Relación entre VA * actividades e indicadores IHEH para 2018",
       x = "VA (log)",
       y = "Valor_iheh")


### Cálculo correlaciones####

## Organizando la correlación####

# Preparar datos
df_plot <- Eco_iheh %>%
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


# Gráfica con asterisco de significancia
df_plot %>% 
   filter(Año==2018) %>% 
ggplot( aes(x = VA_Actividades+0.000001, y = valor_iheh)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  facet_grid(Actividades ~ variable, scales = "free_y") +
  geom_text(
    data = df_cor,
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

## ECO_iheh_rcl #######

### Cálculo correlaciones####

## Organizando la correlación####

# Preparar datos
df_plot_rcl <- Eco_iheh_rcl %>%
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



df_plot_rcl %>%
  filter(Año==2018) %>% 
 
  ggplot(aes(x = VA_Actividades+0.000001, y = Porcentaje)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  facet_grid(Actividades ~ Categorías)+
  geom_text(
    data = df_cor_rcl,
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
  select(MpCodigo, MpNombre, MpCategor, MpAltitud, mean, VATarea, VA1area, VA2area, VA3area, Año) %>% 
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
    Pvt_18_22 = ifelse(VATarea_2018 == 0, NA, (Dvt_20_22 / VATarea_2018) * 100)
  ) %>% 
  mutate(
    Piso_termico = case_when(
      MpAltitud < 1000 ~ "Cálido",
      MpAltitud >= 1000 & MpAltitud < 2000 ~ "Templado",
      MpAltitud >= 2000 & MpAltitud < 3000 ~ "Frío",
      MpAltitud >= 3000 & MpAltitud < 4000 ~ "Páramo",
      MpAltitud >= 4000 ~ "Nival",
      TRUE ~ NA_character_
    )
  )


library(dplyr)

promedios <- tabla_cambios %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

promedios





tabla_cambios %>%
  #filter(Año == 2018) %>%
  ggplot(aes(x = Dvt_18_22 , y =  Dmean_18_22)) +
  geom_point() +
  #facet_wrap(vars(Piso_termico))+
  #scale_x_sqrt()+
  #scale_x_log10() +
  labs(title = var_plot)


tabla_cambios %>%
  #filter(MpCategor == 1) %>%
  ggplot(aes(x = Pvt_18_22, y =  Pmean_18_22)) +
  geom_point() +
  facet_wrap(vars(Piso_termico, MpCategor))+
  #scale_x_sqrt()+
  scale_x_log10() +
  labs(title = var_plot)

tabla_cambios %>%
  #filter(MpCategor == 1) %>%
  ggplot() +
  geom_boxplot(aes(x=Categorías, y= PConteo_20_22)) +
  # facet_wrap(vars(Categorías))+
  #scale_x_sqrt()+
  labs(title = var_plot)



tc_filter <- tabla_cambios %>%
  mutate(id = row_number()) %>% 
  filter(Pmean_18_22 <= 0) 

# 153 municipios mantienen una huella promedio constante o disminuye a pesar de haber un incrementado el valor agregado
# Total de municipios 1119

tc_filter%>%
  ggplot( ) +
  geom_point(aes(x=id,y =  Pmean_18_22), col="blue") +
  geom_point(aes(x=id,y =  Pvt_18_22), col="red") +
  #geom_point(col="blue") +
  #facet_wrap(vars(Piso_termico, MpCategor))+
  #scale_x_sqrt()+
  #scale_x_log10() +
  labs(title = var_plot)




# intento con reclass natural y muy alto primero

# Preparar datos

tabla_cambios_rcl <- Eco_iheh_rcl %>% 
  select(MpCodigo,MpNombre,MpCategor,MpAltitud, Categorías, Conteo, VATarea, Año) %>% 
  # Pasar a formato ancho y calcular cambios
  pivot_wider(
    names_from = Año,
    values_from = c(Conteo,VATarea),
    names_sep = "_"
  )%>%
  mutate(
    # Cambios absolutos en Conteo
    DConteo_18_20 = Conteo_2020 - Conteo_2018,
    DConteo_20_22 = Conteo_2022 - Conteo_2020,
    DConteo_18_22 = Conteo_2022 - Conteo_2018,
    
    # Porcentaje de cambio en Conteo
    PConteo_18_20 = ifelse(Conteo_2018 == 0, NA, (DConteo_18_20 / Conteo_2018) * 100),
    PConteo_20_22 = ifelse(Conteo_2020 == 0, NA, (DConteo_20_22 / Conteo_2020) * 100),
    PConteo_18_22 = ifelse(Conteo_2018 == 0, NA, (DConteo_20_22 / Conteo_2018) * 100),
    
    # Cambios absolutos en VATarea
    Dvt_18_20 = VATarea_2020 - VATarea_2018,
    Dvt_20_22 = VATarea_2022 - VATarea_2020,
    Dvt_18_22 = VATarea_2022 - VATarea_2018,
    
    # Porcentaje de cambio en VATarea
    Pvt_18_20 = ifelse(VATarea_2018 == 0, NA, (Dvt_18_20 / VATarea_2018) * 100),
    Pvt_20_22 = ifelse(VATarea_2020 == 0, NA, (Dvt_20_22 / VATarea_2020) * 100),
    Pvt_18_22 = ifelse(VATarea_2018 == 0, NA, (Dvt_20_22 / VATarea_2018) * 100)
  ) %>% 
  mutate(
    Piso_termico = case_when(
      MpAltitud < 1000 ~ "Cálido",
      MpAltitud >= 1000 & MpAltitud < 2000 ~ "Templado",
      MpAltitud >= 2000 & MpAltitud < 3000 ~ "Frío",
      MpAltitud >= 3000 & MpAltitud < 4000 ~ "Páramo",
      MpAltitud >= 4000 ~ "Nival",
      TRUE ~ NA_character_
    )
  )




tabla_cambios_rcl %>%
  #filter(Categorías=="Natural") %>%
  ggplot(aes(x = Dvt_18_22+200 , y =  DConteo_18_22)) +
  geom_point() +
  facet_wrap(vars(Categorías))+
  #scale_x_sqrt()+
  #scale_y_log10() +
  scale_x_log10() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = var_plot)


tabla_cambios_rcl %>%
  #filter(MpCategor == 1) %>%
  ggplot(aes(x = Pvt_18_22+200, y =  PConteo_18_22+200)) +
  geom_point() +
  facet_wrap(vars(Categorías))+
  #scale_x_sqrt()+
  scale_y_log10() +
  scale_x_log10() +
  labs(title = var_plot)


tabla_cambios_rcl %>%
  #filter(MpCategor == 1) %>%
  ggplot() +
  geom_boxplot(aes(x=Categorías, y= PConteo_20_22)) +
  #facet_wrap(vars(Categorías))+
  #scale_x_sqrt()+
  labs(title = var_plot)

