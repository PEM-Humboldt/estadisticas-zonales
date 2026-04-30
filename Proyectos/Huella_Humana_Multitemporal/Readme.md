Análisis multitemporal de la huella espacial humana
================
En este código se realiza un análisis multitemporal (1970, 1990, 2000, 2015, 2018) de la huella espacial humana por municipios en la altillanura colombiana. 
Se calculan estadísticas zonales (promedio, mediana, desviación estándar) de los valores de huella y estadisticos zonales para obtener la frecuencias de categorias de intensidad de  IHEH. La categorización de IHEH tambien se efectúa en el código 

Los resultados se guardan en dos data frames:

Stat_values: Contiene estadísticas zonales (promedio, mediana, desviación estándar) para cada departamento y año .
Stat_reclass: Contiene la frecuencia y porcentaje de categorías de reclasificación para cada departamento y año.

En la última sección del código las tablas se organizan para su exportación en formatos .csv y html para tener tablas interactivas que faciliten la exploración. Seguidamente se preparan y exportan gráficas de los datos que muestren la evolución de la IHEH a través de los años y permita comparar  entre municipios.

Proporciona información sobre cómo los cambios en la presión y el impacto de las actividades humanas están ejerciendo presión sobre la
biodiversidad en estas áreas. 

## Organizar directorio de trabajo

<a id="ID_seccion1"></a>
Las entradas de ejemplo de este ejercicio están almacenadas en
[aquí](https://drive.google.com/file/d/1vIRruDHrKeZGSdZ-2AEdU9O6roJ0267x/view?usp=drive_link).
Una vez descargadas y descomprimida, reemplaze la carpeta “Originales” en el directorio Datos del proyecto.
El directorio del proyecto está organizado de esta manera que facilita la ejecución del
código:

    Códigos
    │- Huella_Altillanura
    │- Huella_comparar_gassert.R
    │- Huella_comparar_mu.R
    │- Huella_comparar_wcs.R
    │- Huella_dane_analisis.R
    │- Huella_dane_analisis_dpto.R
    │- Huella_dane_analisis_dpto_2015.R
    │- Huella_dane_analisis_mun_bio.R
    │- Huella_dane_analisis_municipio_limpio.R
    │- Huella_MT&bioma_IHEH_nueva.R
    │- Huella_MT_dept.R
    │- Huella_MT_dept_IHEH_nueva.R
    │- temp_Huella_MT_dept_2015.R  
    └-Datos
    │ │
    │ └- Originales: replaze aquí los datos que bajo
    │ │   │
    │ │   
    │ └- Intermedios
    │     │     
    |
    └- Resultados


# Descripción de códigos
- Huella_comparar_gassert.R: Compara la huella de Gassert con la huella institucional; calcula correlaciones y ajusta modelos lineales.
- Huella_comparar_mu.R: Repite el análisis comparativo (correlaciones y modelos) usando el modelo “mu”.
- Huella_comparar_wcs.R: Comparación de huellas con el modelo WCS (Eric Sanderson), incluyendo correlaciones y regresión lineal.
- Huella_dane_analisis.R: Análisis general de huella vs. variables DANE; versión inicial conservada como respaldo.
- Huella_dane_analisis_dpto.R: Evalúa la huella por departamento en relación con el valor agregado (precios corrientes y constantes).
- Huella_dane_analisis_dpto_2015.R: Mismo análisis departamental, pero usando el modelo de huella de Correa para el año 2015.
- Huella_dane_analisis_mun_bio.R: Análisis de huella a nivel municipal incorporando criterios/agrupaciones biofísicas.
- Huella_dane_analisis_municipio_limpio.R:Análisis municipal depurado; enfocado en valor agregado a precios corrientes.
- Huella_MT&bioma_IHEH_nueva.R:Integra métricas de huella con biomas para la nueva versión del IHEH.
- Huella_MT_dept.R:Calcula estadísticas zonales de la huella (versión antigua de Correa) a nivel departamental.
- Huella_MT_dept_IHEH_nueva.R:Estadísticas zonales a nivel departamental para la nueva versión del IHEH.
temp_Huella_MT_dept_2015.R:Script temporal de procesamiento (2015); funcionalidad no clara, candidato a eliminación.

