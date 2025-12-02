# 🛢️ De la roca al hidrocarburo: ¿dónde hacer un pozo?

Análisis exploratorio y modelado predictivo sobre datos de yacimientos hidrocarburíferos de todo el mundo bajo el marco del **Trabajo Práctico Final** para la materia *Introducción a la Ciencia de Datos* (LCD-UNSAM).

---

## 📚 Contexto

La extracción de hidrocarburos enfrenta un escenario cada vez más exigente: yacimientos maduros, variabilidad geológica y presión por mejorar la eficiencia. En este entorno, entender qué pozos pueden aportar mayor valor deja de ser un ejercicio técnico y se vuelve una necesidad estratégica. Este proyecto aborda ese desafío mediante el análisis de reservorios para destacar aquellos con **mejores condiciones productivas**.

## 🎯 Descripción del Problema
La calidad de un reservorio está determinada, en gran parte, por la porosidad de sus rocas (proporción de espacios vacíos donde puede almacenarse el hidrocarburo) expresado como porcentaje de las mismas.
El problema es que medir la porosidad requiere perforaciones y estudios de subsuelo que representan cerca del **40% del costo total de exploración**. Esto vuelve ineficiente el proceso, ya que implica incurrir en altos costos sin certezas previas, dificultando así la optimización del modelo de negocio.

## 💡 Solución propuesta
Se buscó **predecir la porosidad de un reservorio** a partir de variables geológicas disponibles antes de perforar, como la litología, la profundidad o el espesor. Para ello, se construyó un modelo de regresión lineal que permite estimar esta propiedad clave sin incurrir en los elevados costos de exploración directa.
Esta estrategia reduce el riesgo económico y técnico asociado a la perforación de pozos con baja calidad de reservorio, optimizando así la toma de decisiones en etapas tempranas del proceso exploratorio.

## 📊 Resultados principales

    
## 📁 Estructura del proyecto
```
OilGas-ICD-TPF/
├── data/                       # Datasets utilizados
│   └── raw/ 
        ├── oil_test.csv
│       └── train_oil.csv
├── reports/                     # Gráficos y visualizaciones
├── src/                        # Código fuente
├── notebooks/
├── models/
├── requirements.txt                   
├── README.md
└── LICENSE  
```

## 📩 Datos utilizados
Los datos provienen de un desafío de *Kaggle*, orientado al aprendizaje automático aplicado a yacimientos de petróleo y gas.
Fuente: https://www.kaggle.com/competitions/oilgas-field-prediction/data

## 🛠️ Tecnologías Utilizadas
- **Python 3.13.1**
- **VSCode**

## 📈 Metodología
1. **Preparación de datos**: se unificaron datasets, se tradujeron variables al español y se realizó una limpieza intensiva para garantizar la coherencia y completitud de los registros.

2. **Transformaciones y reagrupamientos**: se agruparon categorías geológicas, se convirtieron unidades al sistema métrico y se aplicaron transformaciones logarítmicas y categorizaciones para mejorar la modelización.

3. **Análisis exploratorio**: se utilizaron visualizaciones para identificar patrones entre la porosidad y distintas variables geológicas (como litología, profundidad, espesor, etc.).

4. **Modelado predictivo**: se construyeron modelos de regresión lineal múltiple, evaluando distintas especificaciones con inclusión progresiva de variables e interacciones.

5. **Evaluación del modelo**: se validaron supuestos del modelo y se analizaron los residuos para asegurar un buen ajuste.

## 🧠 Conclusiones y Aprendizajes


## 🧑‍💻 Contacto
Estoy abierto a recibir ideas, sugerencias o comentarios! Podes contactarme por LinkedIn o mail.
[LinkedIn](https://www.linkedin.com/in/matiasvergaravicencio/) · [hola.matiasv@gmail.com](mailto:hola.matiasv@gmail.com)
