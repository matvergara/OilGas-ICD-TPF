# 🛢️ De la roca al hidrocarburo: ¿dónde hacer un pozo?

Análisis exploratorio y modelado predictivo sobre datos de yacimientos hidrocarburíferos de todo el mundo bajo el marco del **Trabajo Práctico Final** para la materia Introducción a la Ciencia de Datos de la UNSAM.

---

### 🧑‍💻 Autores

- **Bruno Inguanzo** · [@BrunoInz](https://github.com/BrunoInz)
- **Emanuel Pinasco** · [@manupinasco](https://github.com/manupinasco)
- **Javier Valdez** · [@javivaldez49](https://github.com/javivaldez49)
- **Matías Vergara** · [@matvergara](https://github.com/matvergara)

## 📚 Contexto del Proyecto

Para el año 2025, la demanda global de hidrocarburos sigue en aumento. Sin embargo, la producción muestra una tendencia decreciente debido al cierre de plataformas, la baja productividad y otros factores.
Frente a este escenario, una posible estrategia para reducir el desequilibrio entre oferta y demanda es optimizar la fase de _upstream_, identificando y explotando aquellos pozos con **mayor calidad de reservorio**.

## 🎯 Descripción del Problema
La calidad de un reservorio está determinada, en gran parte, por la porosidad de sus rocas, es decir, por la proporción de espacios vacíos donde puede almacenarse el hidrocarburo.
El problema es que medir la porosidad requiere perforaciones y estudios de subsuelo que representan cerca del **40% del costo total de exploración**. Esto vuelve ineficiente el proceso si el objetivo es mejorar el modelo de negocio y revertir el desequilibrio entre oferta y demanda, ya que se incurre en altos costos sin certezas previas.

## 🔧 Solución propuesta
Proponemos analizar si existen **variables geológicas que permitan predecir la porosidad de un reservorio** sin necesidad de perforación. En caso afirmativo, se podrá identificar configuraciones geológicas asociadas a reservorios de mayor o menor calidad, optimizando así la toma de decisiones en etapas tempranas del proceso exploratorio.

## 🛠️ Tecnologías Utilizadas
- **R 4.4.2**
- **RStudio**: IDE para desarrollo de código.
- **Tidyverse**: Limpieza, transformación y análisis de datos.
- **ggplot2**: Generación de gráficos exploratorios y visualizaciones personalizadas.
- **dplyr & stringr**: Procesamiento de datos y manipulación de texto.
- **sf, rnaturalearth y rnaturalearthdata**: Representación geoespacial de los yacimientos.
- **modelr**: Generación de modelos de regresión lineal.

## Conclusiones y Aprendizajes
