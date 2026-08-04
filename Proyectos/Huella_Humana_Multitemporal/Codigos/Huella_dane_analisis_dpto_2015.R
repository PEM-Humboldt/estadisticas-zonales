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

Años <- c("2015")
Años_numero <- 1


Unidad_analsis <- "Departamento" # Departamento" Municipio, Bioma
atributo_rast <- c("dpto_ccdgo") # MpCodigo

#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************
#*

archivo <- file.path(dir_Datos_Or,"DANE/PIB_dept_milesmillonesCOP.xlsx")

# 1. Obtener nombres de las hojas
hojas <- excel_sheets(archivo)

# 2. Leer todas las hojas y unirlas
tabla_final <- map_dfr(hojas, function(h) {
  read_excel(archivo, sheet = h) %>%
    mutate(Info = h)  # agrega el nombre de la hoja como columna
})%>% 
  rename( "Dep_ID"= "Código Departamento (DIVIPOLA)")


Stat_values <- read.csv2( paste0(dir_Resultados, "/IHEH_stats_2015", Unidad_analsis,".csv"))
Stat_reclass <- read.csv2( paste0(dir_Resultados, "/IHEH_clases_2015",Unidad_analsis,".csv"))


#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

## pib arreglar formato
tabla_final <- tabla_final %>%
  select(1,2,13, 21) %>% 
  pivot_longer(cols=starts_with("2"),names_to="Año", values_to="PIB")

tabla_final <- tabla_final %>%
  pivot_wider(names_from="Info", values_from="PIB")



## tablas iheh  ####

homogenizarCodigo <- function(Stat,cifras){ # muccicipio= 5 departamento=2
  str_pad(
    as.character(Stat[[atributo_rast]]),
    width = 2,
    side = "left",
    pad = "0"
  )
}

Stat_values[[atributo_rast]] <- homogenizarCodigo(Stat_values)
Stat_reclass[[atributo_rast]] <- homogenizarCodigo(Stat_reclass)

Stat_values$Año <- 2015

## completar con información económica ####


ecoMergeIheh <- function(Stat){
  merge(Stat,tabla_final, 
        by.x =c(atributo_rast, "Año"), 
        by.y=c( "Dep_ID", "Año") )
}


Eco_iheh <- ecoMergeIheh(Stat_values) %>% 
  mutate(Corriente_area= corriente/dpto_narea , #  dpto_narea,
         constante_area= constante/dpto_narea   , #dpto_narea
  )


# Corregir el área porque está en formato texto


Eco_iheh_rcl <- ecoMergeIheh(Stat_reclass) %>% 
  mutate(
    Corriente_area = corriente / dpto_narea,
    constante_area = constante / dpto_narea
  )

#****************************************************************************
# Análisis por departamento ----------------------------
#****************************************************************************

## ECO_iheh #######
Eco_iheh %>% 
  filter(Año==2015) %>% 
  
  ggplot( aes(x = constante, y = mean)) +
  geom_point()




Eco_iheh %>%
  tidyr::pivot_longer(cols = c(min, median, mean,max , sd),
                      names_to = "variable",
                      values_to = "valor") %>%
  ggplot(aes(x = constante_area, y = valor)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_grid(Año ~ variable, scales = "free_y") +
  labs(title = "Relación entre VA e indicadores IHEH por año",
       x = "Valor agregado (log)",
       y = "Valor")


Eco_iheh %>%
  filter(Año==2015) %>% 
  tidyr::pivot_longer(cols = c(min, median, mean,max , sd),
                      names_to = "variable",
                      values_to = "valor_iheh") %>%
  tidyr::pivot_longer(cols = c(corriente, constante, `PIB hab` ),
                      names_to = "Info",
                      values_to = "PIB")  %>%
  mutate(
    variable = factor(variable,
                      levels = c("min", "median", "mean", "max", "sd")))%>%
  
  ggplot(aes(x = PIB, y = valor_iheh)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_grid(Info ~ variable, scales = "free_y")+
  labs(title = "Relación entre VA * actividades e indicadores IHEH para 2018",
       x = "VA (log)",
       y = "Valor_iheh")

#### prueba con las ponderadas por areas #########


var_plot <- "mean"

Eco_iheh %>%
  filter(Año == 2015) %>%
  ggplot(aes(x = constante_area, y = .data[[var_plot]])) +
  geom_point() +
  #scale_x_sqrt()+
  scale_x_log10() +
  labs(title = var_plot)



Eco_iheh %>%
  filter(Año == 2015) %>%                                  # filtra el año
  tidyr::pivot_longer(cols = c(min, max, sd, median, mean),        # pasa a formato largo
                      names_to  = "variable",
                      values_to = "valor") %>%
  ggplot(aes(x = constante_area, y = valor)) +
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
  ggplot(aes(x = constante_area, y = valor)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_grid(Año ~ variable, scales = "free_y") +
  labs(title = "Relación entre VA e indicadores IHEH por año",
       x = "Valor agregado (log)",
       y = "Valor")


Eco_iheh %>%
  filter(Año==2015) %>% 
  tidyr::pivot_longer(cols = c(min, median, mean,max , sd),
                      names_to = "variable",
                      values_to = "valor_iheh") %>%
  tidyr::pivot_longer(cols = c(Corriente_area, constante_area, "PIB hab"      ),
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
    cols = c(Corriente_area, constante_area),
    names_to = "Info",
    values_to = "PIB"
  ) %>%
  mutate(
    variable = factor(variable,
                      levels = c("min", "median", "mean", "max", "sd")),
    log_VA = log10(PIB+0.000001)
  )

# Calcular correlaciones + significancia
df_cor <- df_plot %>%
  group_by(Info, variable, Año) %>%
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
  filter(Año==2015) %>% 
  ggplot( aes(x = PIB+0.000001, y = valor_iheh)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  facet_grid(Info ~ variable, scales = "free_y") +
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
  
  filter(Año==2015) %>% 
  select(Info,variable,r_pearson) %>% 
  tidyr::pivot_wider(
    names_from = variable,
    values_from = r_pearson
  )%>%
  tibble::column_to_rownames("Info") %>%
  as.matrix()


library(RColorBrewer)
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


df_plot_rcl <- Eco_iheh_rcl %>%
  select(-corriente,-constante,-Corriente_area) %>% 
  pivot_longer(
    cols = c(constante_area, "PIB hab"),
    names_to = "Info",
    values_to = "PIB"
  ) %>%
  mutate(
    Categorías = factor(Categorías,
                        levels = c("Natural", "Bajo", "Medio", "Alto", "Muy Alto")),
    
    log_VA = log10(PIB+0.000001)
  )



# Calcular correlaciones + significancia
df_cor_rcl <- df_plot_rcl %>%
  group_by(Info, Categorías, Año) %>%
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
  filter(Año==2015) %>% 
  
  ggplot(aes(x = PIB+0.000001, y = Porcentaje)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  facet_grid(Info ~ Categorías)+
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

# Correlación por años

cor_mat_rcl <-df_cor_rcl%>% 
  
  filter(Info == "constante_area") %>% 
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
         
         #col.lim=c(-0.9,0.9),
         #         is.corr=F,
         tl.col = "black",
         tl.srt = 45)

