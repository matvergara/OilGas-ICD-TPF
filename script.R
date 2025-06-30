# ============================================================
# Trabajo Practico Final - Introducción a la Ciencia de Datos - 2C 2025
# Predicción de calidad de reservorios de hidrocarburos a partir de variables geológicas.
# Código fuente del análisis realizado
# ============================================================


# ============================================================
# INSTALACIÓN DE PAQUETES NECESARIOS
# ============================================================
install.packages(c("rnaturalearth", "rnaturalearthdata", "tidyverse", "ggplot2", "sf"))

# ============================================================
# CARGA DE LIBRERÍAS NECESARIAS
# ============================================================
library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(modelr)
library(ggh4x)

# ============================================================
# IMPORTACION DE DATASETS
# ============================================================
df_train <- read_csv("data/train_oil.csv")
df_test <- read_csv("data/oil_test.csv")
df <- full_join(df_train, df_test)

# ============================================================
# LIMPIEZA Y TRANSFORMACIÓN INICIAL DE VARIABLES
# ============================================================

# 1. Renombrado de columnas
# Se traducen los nombres originales del dataset a español y se estandariza el formato.
df <- df %>% rename(
  campo                 = `Field name`,
  unidad_reservorio     = `Reservoir unit`,
  pais                  = `Country`,
  region                = `Region`,
  cuenca                = `Basin name`,
  regimen_tectonico     = `Tectonic regime`,
  latitud               = `Latitude`,
  longitud              = `Longitude`,
  operadora             = `Operator company`,
  plataforma            = `Onshore/Offshore`,
  tipo_hidrocarburo     = `Hydrocarbon type`,
  estado_reservorio     = `Reservoir status`,
  estructura_geologica  = `Structural setting`,
  profundidad           = `Depth`,
  periodo_geologico     = `Reservoir period`,
  litologia             = `Lithology`,
  espesor_bruto         = `Thickness (gross average ft)`,
  espesor_neto          = `Thickness (net pay average ft)`,
  porosidad             = `Porosity`,
  permeabilidad         = `Permeability`
)

# 2. Relleno de valores faltantes (NA)
# - Completamos región, longitud y latitud usando variables relacionadas como campo, país, cuenca y unidad.
# - Esto mejora la integridad de los datos y permite usar el mapa posteriormente sin pérdida de registros.

df <- df %>%
  group_by(campo) %>%
  fill(pais, region, longitud, latitud, .direction = "downup") %>%
  ungroup() %>%
  group_by(pais) %>%
  fill(region, .direction = "downup") %>%
  ungroup() %>%
  group_by(unidad_reservorio) %>%
  fill(region, .direction = "downup") %>%
  ungroup() %>%
  group_by(cuenca) %>%
  fill(region, .direction = "downup") %>%
  ungroup()

# 3. Eliminación de variables no relevantes
# Se descartan campos que no aportan al objetivo del análisis (nombre del campo, operadora, etc.)
df <- df %>%
  mutate(
    campo = NULL,
    unidad_reservorio = NULL,
    pais = NULL,
    cuenca = NULL,
    operadora = NULL,
    plataforma = NULL,
    estado_reservorio = NULL,
    estructura_geologica = NULL
  )

# 4. Traducción de categorías de "región"
# Mejora la presentación en los gráficos y facilita la lectura.
df <- df %>%
  mutate(region = case_when(
    region == "FORMER SOVIET UNION" ~ "EX UNIÓN SOVIÉTICA",
    region == "LATIN AMERICA"       ~ "AMÉRICA LATINA",
    region == "MIDDLE EAST"         ~ "MEDIO ORIENTE",
    region == "EUROPE"              ~ "EUROPA",
    region == "NORTH AMERICA"       ~ "AMÉRICA DEL NORTE",
    region == "AFRICA"              ~ "ÁFRICA",
    region == "FAR EAST"            ~ "LEJANO ORIENTE",
    TRUE ~ region
  ))

# 5. Reagrupamiento del régimen tectónico
# Clasificamos los regímenes en cuatro grandes grupos tectónicos:
# - CONVERGENTE, DIVERGENTE, TRANSFORMANTE y MIXTO
# - La clasificación se basa en la presencia de términos clave en la descripción original.

df <- df %>%
  filter(str_detect(regimen_tectonico, "COMPRESSION|INVERSION|TRANSPRESSION|STRIKE-SLIP|TRANSTENSION|EXTENSION")) %>%
  mutate(regimen_tectonico = case_when(
    str_detect(regimen_tectonico, "COMPRESSION|INVERSION") & str_detect(regimen_tectonico, "EXTENSION") ~ "MIXTO",
    str_detect(regimen_tectonico, "COMPRESSION|INVERSION") & str_detect(regimen_tectonico, "TRANSPRESSION|STRIKE-SLIP|TRANSTENSION") ~ "MIXTO",
    str_detect(regimen_tectonico, "EXTENSION") & str_detect(regimen_tectonico, "TRANSPRESSION|STRIKE-SLIP|TRANSTENSION") ~ "MIXTO",
    str_detect(regimen_tectonico, "COMPRESSION|INVERSION") ~ "CONVERGENTE",
    str_detect(regimen_tectonico, "TRANSPRESSION|STRIKE-SLIP|TRANSTENSION") ~ "TRANSFORMANTE",
    str_detect(regimen_tectonico, "EXTENSION") ~ "DIVERGENTE",
    TRUE ~ regimen_tectonico
  ))

# 6. Simplificación posterior de TRANSFORMANTE → MIXTO
# Justificado por bajo número de casos y coherencia con los criterios tectónicos.
df <- df %>%
  mutate(regimen_tectonico = ifelse(regimen_tectonico == "TRANSFORMANTE", "MIXTO", regimen_tectonico))


# ============================================================
# Reagrupamiento de períodos geológicos
# ============================================================

# Objetivo:
# Agrupar los 28 períodos geológicos en categorías más robustas 
# desde el punto de vista estadístico y geocronológico.

# Observaciones iniciales:
# - Sólo 9 de los 28 períodos tienen más de 6 observaciones.
# - 4 períodos superan las 40 observaciones.
# - Esta dispersión genera desequilibrios en el análisis.

# Estrategia considerada:
# - Primero evaluamos agrupar por eras (Precámbrica, Paleozoica, Mesozoica, Cenozoica),
#   pero descartamos esa opción por dos motivos:

#   1) Se pierden patrones relevantes al unir múltiples períodos en una sola era.
#   2) La Precámbrica sigue siendo escasa en datos incluso agrupada como era.

# Limpieza adicional:
# - Se eliminó "Archean" por tener solo 1 observación y gran distancia temporal.
# - Se descartaron "Mesozoic" y "Paleozoic" por estar mal clasificadas (son eras, no períodos).

# Resultado:
# - Se definieron 8 grupos cronológicos contiguos.
# - Estos reflejan tanto coherencia temporal como significancia estadística en el dataset.

df <- df %>%
  mutate(periodo_geologico=str_trim(periodo_geologico))%>%
  mutate(periodo_geologico = case_when(
    periodo_geologico %in% c("PROTEROZOIC", 
                         "PROTEROZOIC-CAMBRIAN",
                         "CAMBRIAN", 
                         "CAMBRIAN-ORDOVICIAN",
                         "ORDOVICIAN", 
                         "SILURIAN", 
                         "DEVONIAN-PERMIAN",
                         "DEVONIAN")~"PROTEROZOICO-DEVÓNICO",
    periodo_geologico %in% c("CARBONIFEROUS",
                                 "DEVONIAN-CARBONIFEROUS",
                                 "CAMBRIAN-ORDOVICIAN/CARBONIFEROUS",
                         "CARBONIFEROUS-PERMIAN",
                         
                                 "CARBONIFEROUS-CRETACEOUS"
                                 )~"CARBONÍFERO",
    periodo_geologico %in% c("PERMIAN", 
              "PERMIAN-TRIASSIC"
              
              )~"PÉRMICO",
    periodo_geologico%in% c("TRIASSIC","TRIASSIC-JURASSIC")~"TRIÁSICO",
    periodo_geologico %in% c("JURASSIC","JURASSIC-CRETACEOUS")~"JURÁSICO",
    periodo_geologico %in% c("CRETACEOUS","CRETACEOUS-PALEOGENE","PALEOZOIC-CRETACEOUS")~"CRETÁCICO",
    periodo_geologico%in% c("PALEOGENE","PALEOGENE-NEOGENE")~"PALEÓGENO",
    periodo_geologico=="NEOGENE"~"NEÓGENO",
    TRUE~"OTROS"))%>%
  filter(periodo_geologico!="OTROS")

# ============================================================
# LIMPIEZA Y TRANSFORMACIÓN DE VARIABLES GEOLÓGICAS
# ============================================================

# 1. Reagrupamiento de la litología
# Agrupamos múltiples tipos específicos de sandstone y limestone bajo esas categorías generales.

df <- df %>%
  mutate(
    litologia = case_when(
      str_detect(litologia, "SANDSTONE") ~ "SANDSTONE",
      str_detect(litologia, "LIMESTONE") ~ "LIMESTONE",
      TRUE ~ litologia
    )
  )

# 2. Traducción de litologías al español
df <- df %>%
  mutate(litologia = case_when(
    litologia == "SANDSTONE"     ~ "ARENISCA",
    litologia == "LIMESTONE"     ~ "CALIZA",
    litologia == "DOLOMITE"      ~ "DOLOMITA",
    litologia == "CONGLOMERATE"  ~ "CONGLOMERADO",
    litologia == "SILTSTONE"     ~ "LUTITA LIMOSA",
    litologia == "CHALK"         ~ "CRETA",
    litologia == "SHALE"         ~ "LUTITA",
    litologia == "VOLCANICS"     ~ "ROCA VOLCÁNICA",
    litologia == "DIATOMITE"     ~ "DIATOMITA",
    litologia == "CHERT"         ~ "SÍLEX",
    litologia == "BASEMENT"      ~ "BASAMENTO",
    TRUE ~ litologia
  ))

# 3. Reducción de categorías poco representadas
# Por criterio estadístico, unificamos las litologías menos frecuentes bajo "OTRAS"
df <- df %>%
  mutate(litologia = case_when(
    litologia %in% c("ARENISCA", "CALIZA", "DOLOMITA") ~ litologia,
    TRUE ~ "OTRAS"
  ))

# 4. Filtrado de tipos de hidrocarburos
# Se eliminan hidrocarburos no convencionales con muy baja representación (BITUMEN, CO2, etc.)
df <- df %>%
  filter(tipo_hidrocarburo %in% c("OIL", "GAS", "GAS-CONDENSATE"))

# 5. Validación y filtrado de espesor neto
# - Se eliminan filas con espesor neto mayor al bruto (sin sentido físico).
# - Se eliminan registros con espesor neto igual a 0 (probable error de carga).
df <- df %>%
  filter(espesor_neto <= espesor_bruto, espesor_neto > 0)

# 6. Eliminación de outlier extremo en porosidad
# Se elimina el valor 55 por estar fuera del rango físico esperado (0–30%) y fuera de IQR*1.5
df <- df %>%
  filter(porosidad < 55)

# 7. Conversión de unidades a sistema métrico
# Se convierten profundidad y espesor bruto de pies a kilómetros
df <- df %>%
  mutate(
    profundidad    = profundidad * 0.0003048,
    espesor_bruto  = espesor_bruto * 0.0003048
  )

# 8. Filtrado de valores extremos en permeabilidad
# Se conserva el 95% de los registros (distribución recortada) para evitar sesgos de cola
df <- df %>%
  filter(permeabilidad < quantile(permeabilidad, 0.95))

# 9. Conversión de variables categóricas a factores
# Se definen los niveles ordenados para las eras geológicas
df <- df %>%
  mutate(
    regimen_tectonico = factor(regimen_tectonico),
    tipo_hidrocarburo = factor(tipo_hidrocarburo),
    litologia         = factor(litologia),
    periodo_geologico = factor(periodo_geologico,
                               levels = c(
                                 "PROTEROZOICO-DEVÓNICO",
                                 "CARBONÍFERO",
                                 "PÉRMICO",
                                 "TRIÁSICO",
                                 "JURÁSICO",
                                 "CRETÁCICO",
                                 "PALEÓGENO",
                                 "NEÓGENO")
    )
  )

# ============================================================
# TRANSFORMACIONES PARA MEJORAR EL AJUSTE DEL MODELO
# ============================================================

# 1. Log-transformación del espesor bruto
# - La variable espesor bruto presenta una distribución sesgada a la derecha.
# - Este comportamiento es esperable según el dominio: los espesores varían ampliamente por contexto estructural.
# - Aplicamos el logaritmo natural para estabilizar la varianza y reducir la influencia de outliers.
# - Esto mejora el ajuste en un modelo lineal al captar relaciones no lineales.

df <- df %>%
  mutate(espesor_bruto_log = log(espesor_bruto))

# 2. Log-transformación de la permeabilidad
# - Durante el análisis exploratorio, observamos que la relación entre permeabilidad y porosidad no era lineal.
# - Por eso aplicamos log-transformación para permitir un mejor ajuste del modelo.

df <- df %>%
  mutate(permeabilidad_log = log(permeabilidad))

# ============================================================
# ANÁLISIS EXPLORATORIO
# ============================================================

# 1. Distribución del espesor bruto (original y transformado)

df %>%
  ggplot(aes(x = espesor_bruto)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribución del espesor bruto", x = "Espesor bruto [km]", y = "Densidad") +
  theme(
    plot.title      = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 14)),
    axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 14)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA)
  )

df %>%
  ggplot(aes(x = espesor_bruto_log)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribución del logaritmo del espesor bruto", x = "Logaritmo del espesor bruto", y = "Densidad") +
  theme(
    plot.title      = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 14)),
    axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 14)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA)
  )

# 2. Distribución de permeabilidad por litología

df %>%
  ggplot(aes(x = permeabilidad)) +
  geom_histogram(aes(y = after_stat(density), fill = litologia),
                 bins = 25, alpha = 0.3, color = "black", position = "stack") +
  labs(title = "Distribución de la permeabilidad por litología", x = "Permeabilidad [mD]", y = "Densidad")

# 3. Porosidad por período geológico

medianas_periodo <- df %>%
  filter(!is.na(periodo_geologico)) %>%
  group_by(periodo_geologico) %>%
  summarize(mediana = median(porosidad, na.rm = TRUE))

colores_periodo <- c(
  "CARBONÍFERO" = "#08306B", "PÉRMICO" = "#225EA8", "PROTEROZOICO-DEVÓNICO" = "#1D91C0",
  "CRETÁCICO" = "#41B6C4", "TRIÁSICO" = "#7FCDBB", "JURÁSICO" = "#A1D99B",
  "PALEÓGENO" = "#C7E9B4", "NEÓGENO" = "#EDF8B1"
)

ggplot(df, aes(x = porosidad, fill = periodo_geologico)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = medianas_periodo, aes(xintercept = mediana), linetype = "dashed", size = 0.5) +
  scale_fill_manual(values = colores_periodo) +
  labs(title = "Distribución de porosidad por período geológico", x = "Porosidad [%]", y = "Densidad") +
  ggh4x::facet_wrap2(~periodo_geologico, scales = "fixed", axes = "x", strip.position = "top") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 32, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 24, face = "bold", margin = margin(t = 24)),
    axis.title.y    = element_text(size = 24, face = "bold", margin = margin(r = 24)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA),
    legend.position = "none",
    ggh4x.panel_selector = list(
      cols = -1,
      rows = NULL,
      y.text = element_blank(),
      y.ticks = element_blank()
    )
  )

# 4. Porosidad por litología

medianas_litologia <- df %>%
  filter(!is.na(litologia)) %>%
  group_by(litologia) %>%
  summarize(mediana = median(porosidad, na.rm = TRUE))

ggplot(df, aes(x = porosidad, fill = litologia)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = medianas_litologia, aes(xintercept = mediana), linetype = "dashed", size = 0.5) +
  labs(title = "Distribución de porosidad por litología", x = "Porosidad [%]", y = "Densidad") +
  ggh4x::facet_wrap2(~litologia, scales = "fixed", axes = "x", strip.position = "top") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 32, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 24, face = "bold", margin = margin(t = 24)),
    axis.title.y    = element_text(size = 24, face = "bold", margin = margin(r = 24)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA),
    legend.position = "none",
    ggh4x.panel_selector = list(
      cols = -1,
      rows = NULL,
      y.text = element_blank(),
      y.ticks = element_blank()
    )
  )

# 5. Porosidad por profundidad (según litología)

df %>%
  ggplot(aes(x = profundidad, y = porosidad)) +
  geom_point(alpha = 0.5, size = 1.5, color = "#225EA8") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "#b60c0c", size = 1) +
  facet_wrap(~litologia, scales = "free") +
  labs(title = "Porosidad por profundidad según litología", x = "Profundidad [km]", y = "Porosidad [%]") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 26, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 18, face = "bold", margin = margin(t = 18)),
    axis.title.y    = element_text(size = 18, face = "bold", margin = margin(r = 18)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA))
  

# 6. Porosidad por permeabilidad (lineal y logarítmica)

df %>%
  ggplot(aes(x = permeabilidad, y = porosidad)) +
  geom_point(alpha = 0.5, size = 1.5, color = "darkblue") +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "#b60c0c", size = 1) +
  labs(title = "Porosidad por permeabilidad", x = "Permeabilidad [mD]", y = "Porosidad [%]") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 26, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 18, face = "bold", margin = margin(t = 18)),
    axis.title.y    = element_text(size = 18, face = "bold", margin = margin(r = 18)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA)
  )

df %>%
  ggplot(aes(x = permeabilidad_log, y = porosidad)) +
  geom_point(alpha = 0.5, size = 1.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "#b60c0c", size = 1) +
  labs(title = "Porosidad por logaritmo de la permeabilidad", x = "Log(permeabilidad)", y = "Porosidad [%]") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 26, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 18, face = "bold", margin = margin(t = 18)),
    axis.title.y    = element_text(size = 18, face = "bold", margin = margin(r = 18)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA)
  )

# 7. Porosidad por log espesor bruto según región

df %>%
  ggplot(aes(x = espesor_bruto_log, y = porosidad)) +
  geom_point(alpha = 0.5, size = 1.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  facet_wrap(~region, scales = "free") +
  labs(title = "Porosidad vs logaritmo del espesor bruto", subtitle = "Segmentado según región",
       x = "Logaritmo del espesor bruto", y = "Porosidad [%]") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "#003049"),
    plot.subtitle = element_text(size = 17, face = "bold", hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 14)),
    axis.title.y = element_text(margin = margin(r = 14)),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 15),
    strip.text = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA)
  )

# ============================================================
# MODELADO DE REGRESIÓN LINEAL MÚLTIPLE
# ============================================================

# ------------------------------------------------------------
# 1. Construcción progresiva del modelo
# ------------------------------------------------------------

# Modelos sucesivos con variables continuas y ANOVA
mod1 <- lm(porosidad ~ espesor_bruto_log, df)
summary(mod1)

mod2 <- lm(porosidad ~ espesor_bruto_log + profundidad, df)
summary(mod2)
anova(mod1, mod2)

mod3 <- lm(porosidad ~ espesor_bruto_log + profundidad + permeabilidad_log, df)
summary(mod3)
anova(mod2, mod3)

# Incorporación de variables categóricas
mod4 <- lm(porosidad ~ espesor_bruto_log + profundidad + permeabilidad_log + periodo_geologico, df)
summary(mod4)
anova(mod3, mod4)

mod5 <- lm(porosidad ~ espesor_bruto_log + profundidad + permeabilidad_log + periodo_geologico + litologia, df)
summary(mod5)
anova(mod4, mod5)

mod6 <- lm(porosidad ~ espesor_bruto_log + profundidad + permeabilidad_log + periodo_geologico + litologia + regimen_tectonico, df)
summary(mod6)
anova(mod5, mod6)  # Régimen tectónico no mejora el modelo → descartado

# ------------------------------------------------------------
# 2. Evaluación de interacciones significativas
# ------------------------------------------------------------

# Interacción espesor bruto x región
mod7 <- lm(porosidad ~ espesor_bruto_log:region + profundidad + permeabilidad_log + periodo_geologico + litologia, df)
summary(mod7)
anova(mod5, mod7)

# Agregamos interacción profundidad x litología
mod8 <- lm(porosidad ~ espesor_bruto_log:region + profundidad:litologia + permeabilidad_log + periodo_geologico + litologia, df)
summary(mod8)
anova(mod7, mod8)

# Evaluamos posible mejora con tipo de hidrocarburo
mod9 <- lm(porosidad ~ espesor_bruto_log:region + profundidad:litologia + permeabilidad_log + tipo_hidrocarburo + periodo_geologico + litologia, df)
summary(mod9)
anova(mod8, mod9)  # No significativa → descartado

# Evaluamos interacción permeabilidad x litología
mod10 <- lm(porosidad ~ espesor_bruto_log:region + profundidad:litologia + permeabilidad_log:litologia + periodo_geologico + litologia, df)
summary(mod10)
anova(mod8, mod10)  # No significativa → descartado

# ------------------------------------------------------------
# 3. Modelo final y diagnóstico de residuos
# ------------------------------------------------------------

mod_final <- mod8
summary(mod_final)

# Distribución de residuos
res <- data.frame(res = residuals(mod_final))
shapiro.test(res$res)  # p > 0.05 → no se rechaza normalidad

# Visualización: densidad de los residuos
res %>%
  ggplot(aes(x = res)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribución de residuos")

# Visualización: residuos vs predicciones
df %>%
  add_predictions(mod_final) %>%
  add_residuals(mod_final) %>%
  ggplot(aes(x = pred, y = resid)) +
  geom_abline(slope = 0, color = "#b60c0c", intercept = mean(res$res)) +
  geom_point(color = "darkblue") +
  labs(
    title = "Residuos vs predicciones",
    x = "Porosidad predicha [%]",
    y = "Residuo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "#003049"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 14)),
    axis.title.y = element_text(margin = margin(r = 14)),
    axis.text = element_text(size = 16)
  )

# Visualización: valores reales vs predichos
df %>%
  add_predictions(mod_final) %>%
  ggplot(aes(x = porosidad, y = pred)) +
  geom_abline(slope = 1, intercept = 0, color = "#b60c0c") +
  geom_point(color = "darkblue") +
  labs(
    title = "Predicciones vs valores reales de porosidad",
    x = "Porosidad real [%]",
    y = "Porosidad predicha [%]"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 23, face = "bold", hjust = 0.5, color = "#003049"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 14)),
    axis.title.y = element_text(margin = margin(r = 14)),
    axis.text = element_text(size = 16)
  )

# Visualización: porosidad vs permeabilidad con línea predicha
df %>%
  add_predictions(mod_final) %>%
  ggplot(aes(x = permeabilidad_log)) +
  geom_point(aes(y = porosidad), color = "black") +
  geom_abline(slope = 1.03002, intercept = 11.01348, color = "#b60c0c") +
  labs(
    title = "Porosidad vs logaritmo de la permeabilidad",
    x = "Logaritmo de la permeabilidad",
    y = "Porosidad [%]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 14)),
    axis.title.y = element_text(margin = margin(r = 14)),
    axis.text = element_text(size = 12)
  )

# ------------------------------------------------------------
# 4. Visualización de rectas ajustadas por grupo
# ------------------------------------------------------------

grid_permeabilidad <- data_grid(
  df,
  permeabilidad_log,
  profundidad         = seq_range(profundidad, n = 5),
  espesor_bruto_log   = seq_range(espesor_bruto_log, n = 5),
  litologia           = "ARENISCA",
  periodo_geologico   = "PALEÓGENO",
  region              = "AMÉRICA LATINA"
) %>% add_predictions(mod_final)

df %>%
  ggplot(aes(x = permeabilidad_log, y = porosidad)) +
  geom_point(alpha = 1, color = "royalblue") +
  geom_line(
    data = grid_permeabilidad,
    aes(y = pred, group = interaction(profundidad, espesor_bruto_log, periodo_geologico)),
    color = "tomato4"
  ) +
  labs(
    title = "Porosidad vs logaritmo de la permeabilidad (rectas ajustadas)",
    x = "Logaritmo de la permeabilidad",
    y = "Porosidad [%]"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "#003049"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 14)),
    axis.title.y = element_text(margin = margin(r = 14)),
    axis.text = element_text(size = 12)
  )

# ------------------------------------------------------------
# MAPA GLOBAL DE POZOS DEL DATASET
# ------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

df_sf <- df %>%
  filter(!is.na(longitud), !is.na(latitud)) %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

ggplot() +
  geom_sf(data = world, fill = "#F2F2F2", colour = "black", linewidth = .35) +
  geom_sf(data = df_sf, shape = 21, fill = "#386890", colour = "white", stroke = 0.8, size = 4, alpha = .8) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "#e6f1fa", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )