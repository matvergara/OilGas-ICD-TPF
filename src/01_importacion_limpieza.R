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

# 📘 1. Renombrado de columnas
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
