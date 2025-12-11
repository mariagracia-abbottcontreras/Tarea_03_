# Tarea_03_
Tarea 3 para el curso ICP5006-1 "Medición y análisis dimensional de datos políticos".  (Nuevo repositorio porque no sé por qué no se actualizaba el anterior).


# Análisis del Rendimiento Escolar en Chile (2024)
**Ministerio de Educación – Datos oficiales de Rendimiento por alumno**


---

## 0. Introducción

La presente tarea tiene como objetivo analizar cómo diversas características de los establecimientos educacionales —tales como su régimen de dependencia, ubicación geográfica, modalidad y contexto territorial— se relacionan con el rendimiento promedio de los estudiantes del sistema escolar chileno durante 2024. Se utilizó información oficial del Ministerio de Educación. Para ello se desarrolla un análisis estadístico integral que combina exploración descriptiva, detección de valores atípicos, contrastes de hipótesis y modelos de regresión.

Así, este trabajo quiso responder una serie de preguntas específicas que fueron surgiendo de manera secuencial durante el proceso analítico, de modo que el orden de las secciones refleja la lógica con la que se fue construyendo el razonamiento y las hipótesis asociadas. Es decir, no es el órden más "lógico", pero se quiso mantener así para mostrar este tren de pensamientos. Se deja la versión ordenada de manera más integral para el futuro.

A lo largo del documento se trabaja con pruebas estadísticas, visualizaciones y comparaciones entre grupos, permitiendo observar cómo ciertas desigualdades estructurales del sistema educativo chileno se manifiestan en los resultados académicos. De esta manera, el propósito general es identificar patrones, contrastar rendimiento entre distintos tipos de establecimientos y evaluar si las diferencias observadas son estadísticamente significativas.

### 0.1 Librerias necesarias

Antes de empezar lo primero será cargar las siguientes librerias.

```{r warning=F}
library(tidyverse) #manipular datos
library(stringr) #limpiar textos
library(stringi) #limpiar textos
library(gt) #generar tablas
library(ggplot2) #generar gráficos
library(scales) #generar gráficos
library(ggpubr) # ggboxplot 
library(broom) #para tidy()
library(rcompanion) #para cramerV()
library(modelsummary) # tablas de regresión amigables
library(factoextra) # graficos de PCA
```
