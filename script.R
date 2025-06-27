#### Oil/Gas Field Prediction Dataset

#### PREGUNTA: ¿Qué características geológicas definen un buen reservorio de hidrocarburos?

#### Instalar si no tenemos
install.packages(c("rnaturalearth", "rnaturalearthdata"))   # Grafico de mapas

#### Cargamos las librerias necesarias
library(tidyverse)    # Limpieza de datos y gráficos
library(sf)   # Grafico de mapas
library(ggplot2)    # Limpieza de datos y gráficos  
library(rnaturalearth)   # Grafico de mapas
library(rnaturalearthdata)   # Grafico de mapas
library(stringr)    # Limpieza de datos
library(modelr)   # Modelos de regresión lineal

#### Cargamos el Dataset
df_train <- read_csv("data/train_oil.csv")
df_test <- read_csv("data/oil_test.csv")
df <- full_join(df_train, df_test)

#### Modificamos los nombres de las columnas
df <- df %>% rename(
  campo               = `Field name`,
  unidad_reservorio   = `Reservoir unit`,
  pais                = `Country`,
  region              = `Region`,
  cuenca              = `Basin name`,
  regimen_tectonico   = `Tectonic regime`,
  latitud             = `Latitude`,
  longitud            = `Longitude`,
  operadora           = `Operator company`,
  plataforma           = `Onshore/Offshore`,
  tipo_hidrocarburo   = `Hydrocarbon type`,
  estado_reservorio   = `Reservoir status`,
  estructura_geologica = `Structural setting`,
  profundidad      = `Depth`, # pies
  periodo_geologico   = `Reservoir period`,
  litologia           = `Lithology`,
  espesor_bruto    = `Thickness (gross average ft)`, # pies
  espesor_neto    = `Thickness (net pay average ft)`, # pies
  porosidad       = `Porosity`, # porcentaje
  permeabilidad    = `Permeability` # milidarcy
)

#### VALORES NA Y ERRÓNEOS: rellenamos los NA de región a partir del país, la cuenca y la unidad de reservorio en las que hay coincidencias, por ejemplo: para algunos registros teníamos RUSSIA en país pero no región, a pesar de que si teníamos la región para ese país en otro registro, los codigos que siguen buscan esos valores comunes y reemplazan los NA. Tambien reemplazamos los NA de longitud y latitud (que nos sirven para hacer el mapa lo más preciso posible) utilizando el campo (descubrimos que esa es la variable a la que hacen referencia las coordenadas.

df <- df %>%
  group_by(campo) %>%
  fill(
    pais,
    region,
    longitud,
    latitud, 
    .direction = "downup") %>%
  ungroup()

df <- df %>%
  group_by(pais) %>%
  fill(
    region,
    .direction = "downup") %>%
  ungroup()

df <- df %>%
  group_by(unidad_reservorio) %>%
  fill(
    region,
    .direction = "downup") %>%
  ungroup()

df <- df %>%
  group_by(cuenca) %>%
  fill(
    region,
    .direction = "downup") %>%
  ungroup()

#### Sacamos las variables que no nos interesan
df <- df %>%
  mutate(campo = NULL,
         unidad_reservorio = NULL,
         pais = NULL,
         cuenca = NULL,
         operadora = NULL,
         plataforma = NULL,
         estado_reservorio = NULL,
         estructura_geologica=NULL
  )

#### Traducimos los nombres de las regiones
df <- df %>%
  mutate(region = case_when(
    region == "FORMER SOVIET UNION" ~ "EX UNIÓN SOVIÉTICA",
    region == "LATIN AMERICA" ~ "AMÉRICA LATINA",
    region == "MIDDLE EAST" ~ "MEDIO ORIENTE",
    region == "EUROPE" ~ "EUROPA",
    region == "NORTH AMERICA" ~ "AMÉRICA DEL NORTE",
    region == "AFRICA" ~ "ÁFRICA",
    region == "FAR EAST" ~ "LEJANO ORIENTE",
    TRUE ~ region
  ))

## Reagrupamos el regimen tectonico teniendo en cuenta los tipos de "limites de placas" correspondientes a los movimientos presentados, segun sean de tipo CONVERGENTE, DIVERGENTE, una combinación de ellos a lo largo de su historia (MIXTO) o TRANSFORMANTES
df <- df %>%
  filter(str_detect(regimen_tectonico, "COMPRESSION|INVERSION|TRANSPRESSION|STRIKE-SLIP|TRANSTENSION|EXTENSION")) %>%
  mutate(regimen_tectonico = case_when(
    str_detect(regimen_tectonico, "COMPRESSION|INVERSION") &
      str_detect(regimen_tectonico, "EXTENSION") ~ "MIXTO",
    str_detect(regimen_tectonico, "COMPRESSION|INVERSION") &
      str_detect(regimen_tectonico, "TRANSPRESSION|STRIKE-SLIP|TRANSTENSION") ~ "MIXTO",
    str_detect(regimen_tectonico, "EXTENSION") &
      str_detect(regimen_tectonico, "TRANSPRESSION|STRIKE-SLIP|TRANSTENSION") ~ "MIXTO",
    str_detect(regimen_tectonico, "COMPRESSION|INVERSION") ~ "CONVERGENTE",
    str_detect(regimen_tectonico, "TRANSPRESSION|STRIKE-SLIP|TRANSTENSION") ~ "TRANSFORMANTE",
    str_detect(regimen_tectonico, "EXTENSION") ~ "DIVERGENTE",
    TRUE ~ regimen_tectonico))

### Visualizando la cantidad de observaciones para cada grupo de regimen_tectonico notamos que la cantidad de TRANSFORMANTE es baja con respecto al total de la muestra, y por conocimiento de dominio puede incluirse en la categoría de regimen MIXTO
df <- df %>%
  mutate(
    regimen_tectonico = ifelse(
      regimen_tectonico == "TRANSFORMANTE",
      "MIXTO", 
      regimen_tectonico)
  )


#### Reagrupamos los periodos geologicos de acuerdo a criterios estadísticos (cant de observaciones) y cronológicos
# De los 28 períodos, sólo 9 tienen más de 6 observaciones, mientras que 4 de ellos tienen más de 40.
# Para balancearlos, decidimos agruparlos. Primero pensamos en agrupar por eras, pero dos razones nos inclinaron a agrupar por conjuntos de períodos contigüos cronológicamente:
# 1) Estudiando las correlaciones y distribuciones de las variables de interés separadas por esta categoría, observamos patrones que se pierden al unir varios períodos en una única era.
# 2) Al agrupar por eras, la Precámbrica continuaba teniendo muy pocas observaciones.
# Descartamos Archean por tener gran diferencia temporal con el resto y por tener una única observación.
# Descartamos Mesozoic y Paleozoic por estar mal catalogadas como períodos (son eras)

df%>%
  group_by(periodo_geologico)%>%
  summarise(count=n())%>%
  filter(count>6)

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

### Reagrupamos las categorias de la variable litología juntando los distintos tipos de SANDSTONE y LIMESTONE (mayor cantidad de apariciones)
df <- df %>%
  mutate(
    litologia = case_when(
      str_detect(litologia, "SANDSTONE") ~ "SANDSTONE",
      str_detect(litologia, "LIMESTONE") ~ "LIMESTONE",
      TRUE ~ litologia)
  )

### Traducimos el nombre de las categorias de la litologia
df <- df %>%
  mutate(litologia = case_when(
    litologia == "SANDSTONE" ~ "ARENISCA",
    litologia == "LIMESTONE" ~ "CALIZA",
    litologia == "DOLOMITE" ~ "DOLOMITA",
    litologia == "CONGLOMERATE" ~ "CONGLOMERADO",
    litologia == "SILTSTONE" ~ "LUTITA LIMOSA",
    litologia == "CHALK" ~ "CRETA",
    litologia == "SHALE"        ~ "LUTITA",
    litologia == "VOLCANICS"    ~ "ROCA VOLCÁNICA",
    litologia == "DIATOMITE"    ~ "DIATOMITA",
    litologia == "CHERT"        ~ "SÍLEX",
    litologia == "BASEMENT"     ~ "BASAMENTO",
    TRUE ~ litologia  # Mantiene NA o cualquier otro valor no listado
  ))

### Dado el fuerte desbalance observado en la variable litología —con 8 categorías representadas por menos de 10 registros— se decidió reagrupar dichas categorías bajo una nueva clase OTRAS, manteniendo únicamente aquellas con representación estadísticamente significativa para el análisis.
df <- df %>%
  mutate(litologia = case_when(
    litologia %in% c("ARENISCA", "CALIZA", "DOLOMITA") ~ litologia,
    TRUE ~ "OTRAS"
  ))

### Descartamos en tipo_hidrocarburo a BITUMEN (1), CARBON DIOXIDE (2), y METHANE HYDRATE (1) ya que son tipos especiales de hidrocarburos, concretamente no convencionales, que siguen procesos distintos a los hidrocarburos convencionales y representan una porción infima del dataset
df <- df %>% 
  filter(tipo_hidrocarburo %in% c("OIL", "GAS", "GAS-CONDENSATE"))

### Traducimos los tipos de hidrocarburo
df <- df %>%
  mutate(tipo_hidrocarburo = case_when(
    tipo_hidrocarburo == "OIL" ~ "PETRÓLEO",
    tipo_hidrocarburo == "GAS" ~ "GAS",
    tipo_hidrocarburo == "GAS-CONDENSATE" ~ "GAS CONDENSADO",
    TRUE ~ tipo_hidrocarburo  # Mantiene NA u otros valores
  ))

### Eliminamos filas con valores de espesor neto superior a espesor bruto ya que no tiene sentido físico, y las que tienen espesor neto nulo ya que probablemente sean errores de carga (no tiene sentido logico un yacimiento sin espesor explotable)
df <- df %>%
  filter(espesor_neto <= espesor_bruto)

df <- df %>% 
  filter(espesor_neto >0)

### Eliminamos valor 55 de porosidad por considerarlo outlier en base a: 1) conocimiento de dominio, según el cual los valores normales de porosidad tienen un rango aproximado de 0-30, y 2) aparece por fuera del rango +-1.5*IQR.
df%>%
  ggplot(aes(y=porosidad))+
  geom_boxplot(fill="lightblue")

df<-df%>%
  filter(porosidad<55)

### Convertimos la profundidad y el espesor bruto a kilometros para utilizar una unidad que nos resulte familiar
df <- df%>%
  mutate(
    profundidad = profundidad * 0.0003048,
    espesor_bruto=espesor_bruto * 0.0003048)

### Eliminamos 22 registros (permeabilidad mayor o igual a 2036 mD) para quedarnos con el 95% de los datos originales para los cuales tenemos valores de permeabilidad razonables
# Esto se basa en la distribución de la variable analizada más abajo
# Al eliminar el 5% derecho, de toda la cola larga a la derecha del diagrama de densidad nos quedamos únicamente con una distribución trimodal concentrada en los valores 1000md, 1500md y 2000md

df%>%
  ggplot(aes(x=permeabilidad))+
  geom_density(fill="lightblue")

df <- df %>% 
  filter(permeabilidad < quantile(permeabilidad, .95))

df%>%
  ggplot(aes(x=permeabilidad))+
  geom_density(fill="lightblue")

### Convertimos a factor las variables categóricas
df <- df%>%
  mutate(regimen_tectonico=factor(regimen_tectonico),
         tipo_hidrocarburo=factor(tipo_hidrocarburo),
         periodo_geologico=factor(periodo_geologico,
                                  levels = c(
                                    "PROTEROZOICO-DEVÓNICO",
                                    "CARBONÍFERO",
                                    "PÉRMICO",
                                    "TRIÁSICO",
                                    "JURÁSICO",
                                    "CRETÁCICO",
                                    "PALEÓGENO",
                                    "NEÓGENO")
                                  ),
         litologia=factor(litologia))

###### Conteo de variables

df %>% 
  group_by(litologia) %>% 
  summarise(n())

df %>% 
  group_by(region) %>% 
  summarise(n())

df %>% 
  group_by(regimen_tectonico) %>% 
  summarise(n())

df %>% 
  group_by(tipo_hidrocarburo) %>% 
  summarise(n())

df %>% 
  group_by(periodo_geologico) %>% 
  summarise(n())


######## ANÁLISIS EXPLORATORIO Y TRANSFORMACIONES

### DISTRIBUCIONES

### ESPESOR BRUTO
# La variable está fuertemente sesgada a derecha, con valores atípicos muy altos
# En el dominio del problema, esto tiene sentido: el espesor bruto es el volumen potencial de almacén de hidrocarburos, y varía enormemente dependiendo del contexto estructural
# Para estabilizar su varianza y reducir la influencia de los valores extremos, decidimos aplicar el logaritmo (a valores más altos, crece más lento)
# Así, esperamos capturar patrones no lineales en el marco de una regresión lineal

df%>%
  ggplot(aes(x=espesor_bruto))+
  geom_density(fill="lightblue")+
  labs(
    title = "Distribución del espesor bruto",
    x     = "Espesor bruto [km]",
    y     = "Densidad"
  )+
  theme(
    plot.title      = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 14)),
    axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 14)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA),
    #panel.background= element_rect(fill = "#f2f1ec", color = "#f2f1ec"),
    #plot.background = element_rect(fill = "#f2f1ec", color = NA),
    )

df<-df%>%
  mutate(espesor_bruto_log=log(espesor_bruto))

df%>%
  ggplot(aes(x=espesor_bruto_log))+
  geom_density(fill="lightblue")+
  labs(
    title = "Distribución del logaritmo del espesor bruto",
    x     = "Logaritmo del espesor bruto",
    y     = "Densidad"
  )+
  theme(
    plot.title      = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 14)),
    axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 14)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA),
    #panel.background= element_rect(fill = "#f2f1ec", color = "#f2f1ec"),
    #plot.background = element_rect(fill = "#f2f1ec", color = NA),
  )

### PERMEABILIDAD X LITOLOGÍA
df%>%
  ggplot(aes(x=permeabilidad))+
  geom_histogram(alpha=0.3,bins=25,aes(y=after_stat(density),fill=litologia),color="black",position="stack")+
  labs(
    title = "Distribución de la permeabilidad por la litología",
    x     = "Permeabilidad [mD]",
    y     = "Densidad"
  )


#### POROSIDAD x ERA GEOLÓGICA
library(ggh4x)

# Medianas por período
medianas_periodo <- df %>%
  filter(!is.na(periodo_geologico)) %>%
  group_by(periodo_geologico) %>%
  summarize(mediana = median(porosidad, na.rm = TRUE))

colores_periodo <- c(
  "CARBONÍFERO"            = "#08306B",  # azul muy oscuro
  "PÉRMICO"                = "#225EA8",  # azul oscuro
  "PROTEROZOICO-DEVÓNICO"  = "#1D91C0",  # azul intermedio oscuro
  "CRETÁCICO"              = "#41B6C4",  # turquesa oscuro
  "TRIÁSICO"               = "#7FCDBB",  # turquesa claro
  "JURÁSICO"               = "#A1D99B",  # verde-azulado claro
  "PALEÓGENO"              = "#C7E9B4",  # verde muy claro
  "NEÓGENO"                = "#EDF8B1"   # amarillo verdoso claro
)
ggplot(df, aes(x = porosidad, fill = periodo_geologico)) +
  geom_density(alpha = 0.4) +
  geom_vline(
    data = medianas_periodo,
    aes(xintercept = mediana),
    color     = "black",
    linetype  = "dashed",
    size      = 0.5
  ) +
  scale_fill_manual(values = colores_periodo) +
  labs(
    title = "Distribución de porosidad por período geológico",
    x     = "Porosidad [%]",
    y     = "Densidad"
  ) +
  ggh4x::facet_wrap2(
    ~ periodo_geologico,
    scales = "fixed",
    axes   = "x",
    strip.position = "top"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 32, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 24, face = "bold", margin = margin(t = 24)),
    axis.title.y    = element_text(size = 24, face = "bold", margin = margin(r = 24)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA),
    #panel.background= element_rect(fill = "#f2f1ec", color = "#f2f1ec"),
    #plot.background = element_rect(fill = "#f2f1ec", color = NA),
    legend.position = "none",
    ggh4x.panel_selector = list(
      cols = -1,
      rows = NULL,
      y.text = element_blank(),
      y.ticks = element_blank()
    )
  )


#### POROSIDAD x LITOLOGIA
# Medianas por litología
medianas_litologia <- df %>%
  filter(!is.na(litologia)) %>%
  group_by(litologia) %>%
  summarize(mediana = median(porosidad, na.rm = TRUE))

ggplot(df, aes(x = porosidad, fill = litologia)) +
  geom_density(alpha = 0.4) +
  geom_vline(
    data = medianas_litologia,
    aes(xintercept = mediana),
    color     = "black",
    linetype  = "dashed",
    size      = 0.5
  ) +
  labs(
    title = "Distribución de porosidad por litología",
    x     = "Porosidad [%]",
    y     = "Densidad"
  ) +
  ggh4x::facet_wrap2(
    ~ litologia,
    scales = "fixed",
    axes   = "x",
    strip.position = "top"
  ) +
  theme_minimal(base_size = 14) +  # aumenta tamaño base
  theme(
    plot.title      = element_text(size = 32, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 24, face = "bold", margin = margin(t = 24)),
    axis.title.y    = element_text(size = 24, face = "bold", margin = margin(r = 24)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA),
    #panel.background= element_rect(fill = "#f2f1ec", color = "#f2f1ec"),
    #plot.background = element_rect(fill = "#f2f1ec", color = NA),
    legend.position = "none",
    ggh4x.panel_selector = list(
      cols = -1,
      rows = NULL,
      y.text = element_blank(),
      y.ticks = element_blank()
    )
  )


#### POROSIDAD x PROFUNDIDAD (CON LITOLOGÍA)
df %>%
  ggplot(aes(x = profundidad, y = porosidad)) +
  geom_point(alpha = 0.5, size = 1.5, color = "#225EA8") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "#b60c0c", size = 1) +
  facet_wrap(~litologia, scales = "free") +
  labs(
    title = "Porosidad por profundidad según litología",
    x = "Profundidad [km]",
    y = "Porosidad [%]"
  )  +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 26, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 18, face = "bold", margin = margin(t = 18)),
    axis.title.y    = element_text(size = 18, face = "bold", margin = margin(r = 18)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA))

#### POROSIDAD x PERMEABILIDAD (CON Y SIN LOG)
df %>%
  ggplot(aes(x = permeabilidad, y = porosidad)) +
  geom_point(alpha = 0.5, size = 1.5, color = "darkblue") +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "#b60c0c", size = 1) +
  labs(
    title = "Porosidad por permeabilidad",
    x = "Permeabilidad [mD]",
    y = "Porosidad [%]"
  )  +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 26, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 18, face = "bold", margin = margin(t = 18)),
    axis.title.y    = element_text(size = 18, face = "bold", margin = margin(r = 18)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA))

df<-df%>%
  mutate(permeabilidad_log=log(permeabilidad))

df %>%
  ggplot(aes(x = permeabilidad_log, y = porosidad)) +
  geom_point(alpha = 0.5, size = 1.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "#b60c0c", size = 1) +
  labs(
    title = "Porosidad por el logaritmo de la permeabilidad",
    x = "Logaritmo de la permeabilidad",
    y = "Porosidad [%]"
  )  +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 26, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 18, face = "bold", margin = margin(t = 18)),
    axis.title.y    = element_text(size = 18, face = "bold", margin = margin(r = 18)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA))


### POROSIDAD X LOG DE ESPESOR BRUTO SEGUN REGIÓN
df %>%
  ggplot(aes(x = espesor_bruto_log, y = porosidad)) +
  geom_point(alpha = 0.5, size = 1.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  facet_wrap(~region, scales = "free") +
  labs(
    title = "Porosidad vs logaritmo del espesor bruto",
    subtitle = "Segmentado según región",
    x = "Logaritmo del espesor bruto",
    y = "Porosidad [%]"
  )  +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "#003049"),
    plot.subtitle = element_text(size = 17, face = "bold", hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 14)),
    axis.title.y = element_text(margin = margin(r = 14)),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 15),
    strip.text = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA))


### ANÁLISIS PROFUNDO: MODELADO

# Empezamos con la relación entre porosidad y las tres variables continuas, sumando cada parámetro respaldados por anova (analysis of variance)
mod1<-lm(porosidad~espesor_bruto_log,df)
summary(mod1)

mod2<-lm(porosidad~espesor_bruto_log+profundidad,df)
summary(mod2)
anova(mod1,mod2)

mod3<-lm(porosidad~espesor_bruto_log+profundidad+permeabilidad_log,df)
summary(mod3)
anova(mod2,mod3)

# Luego, pasamos a las variables categóricas identificadas como variables de interés en el análisis exploratorio
mod4<-lm(porosidad~espesor_bruto_log+profundidad+permeabilidad_log+periodo_geologico,df)
summary(mod4)
anova(mod3,mod4)

mod5<-lm(porosidad~espesor_bruto_log+profundidad+permeabilidad_log+periodo_geologico+litologia,df)
summary(mod5)
anova(mod4,mod5)

mod6<-lm(porosidad~espesor_bruto_log+profundidad+permeabilidad_log+periodo_geologico+litologia+regimen_tectonico,df)
summary(mod6)
anova(mod5,mod6) # En este caso, no es significativo el costo del parámetro asociado a la variable de régimen tectónico, por lo que resulta descartada

# Después probamos interacciones entre espesor bruto y región, y entre profundidad y litología
mod7<-lm(porosidad~espesor_bruto_log:region+profundidad+permeabilidad_log+periodo_geologico+litologia,df)
summary(mod7)

# Significativo
anova(mod5,mod7)

mod8<-lm(porosidad~espesor_bruto_log:region+profundidad:litologia+permeabilidad_log+periodo_geologico+litologia,df)
summary(mod8)

# Significativo
anova(mod7,mod8)

# También probamos si es significativa la relación entre el logaritmo de la permeabilidad y la litología
mod9<-lm(porosidad~espesor_bruto_log:region+profundidad:litologia+permeabilidad_log+tipo_hidrocarburo+periodo_geologico+litologia,df)
summary(mod9)

# No es significativa la mejora para sumar este parámetro
anova(mod8,mod9)

# Finalmente, probamos si es significativa la relación entre el logaritmo de la permeabilidad y la litología
mod10<-lm(porosidad~espesor_bruto_log:region+profundidad:litologia+permeabilidad_log:litologia+periodo_geologico+litologia,df)
summary(mod10)

# No es suficiente la mejora para sumar este parámetro
anova(mod8,mod10)

# Definimos nuestro modelo final de porosidad
mod_final<-mod8
summary(mod_final)

# Revisamos la distribución de los residuos
res <- data.frame(res = unlist(residuals(mod_final)))

res_mean<-mean(res$res)

# A pesar de no haberlo visto en la cursada, queríamos usar este test para chequear que los residuos tuvieran una distribución normal, dado que esta es una de las premisas de los modelos de regresión lineal
shapiro.test(res$res)

# Al dar un p valor>0.05, no se descarta la hipótesis nula (que los residuos tienen distribución normal). Lo chequeamos con el gráfico de densidad
res%>%
  ggplot(aes(x=res))+
  geom_density(fill="lightblue")

# Revisamos el gráfico de los residuos vs las predicciones y no encontramos ningún patrón reconocible
df%>%
  add_predictions(mod_final)%>%
  add_residuals(mod_final)%>%
  ggplot(aes(x=pred,y=resid))+
  geom_abline(slope=0,color="#b60c0c",intercept=res_mean)+
  geom_point(color="darkblue")+
  labs(
    title = "Residuos vs predicciones",
    x = "Porosidad predicha [%]",
    y = "Residuo"
  )  +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(size = 24, face = "bold", hjust = 0.5, color = "#003049" ),
    axis.title.x    = element_text(margin = margin(t = 14)),
    axis.title.y    = element_text(margin = margin(r = 14)),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text       = element_text(size = 16),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA))

df%>%
  add_predictions(mod_final)%>%
  add_residuals(mod_final)%>%
  ggplot(aes(x=porosidad,y=pred))+
  geom_abline(slope=1,color="#b60c0c",intercept=0)+
  geom_point(color="darkblue")+
  labs(
    title = "Predicciones vs valores reales de porosidad",
    x = "Porosidad predicha [%]",
    y = "Porosidad real [%]"
  )  +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(size = 23, face = "bold", hjust = 0.5, color = "#003049"),
    axis.title.x    = element_text(margin = margin(t = 14)),
    axis.title.y    = element_text(margin = margin(r = 14)),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text       = element_text(size = 16),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA))

  


df%>%
  add_predictions(mod_final)%>%
  ggplot(aes(x=permeabilidad_log))+
  geom_abline(slope=1.03002,color="#b60c0c",intercept= 11.01348 )+
  geom_point(color="black",aes(y=porosidad))+
  labs(
    title = "Residuos vs predicciones",
    x = "Porosidad predicha [%]",
    y = "Residuo"
  )  +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 14)),
    axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 14)),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA))

grid_permeabilidad <- data_grid(df,
                                permeabilidad_log,
  profundidad = seq_range(profundidad,n=5),
  espesor_bruto_log = seq_range(espesor_bruto_log,n=5),
  litologia = "ARENISCA",
  periodo_geologico="PALEÓGENO",
  region="AMÉRICA LATINA"
  #litologia,
  #periodo_geologico
)%>%
  add_predictions(mod_final)

df%>%
  ggplot(aes(x = permeabilidad_log,y=porosidad)) +
  geom_point(alpha=1,color="royalblue") +  # puntos reales
  geom_line(
    data = grid_permeabilidad,
    color="tomato4",
    aes(y=pred, group = interaction(profundidad, espesor_bruto_log,periodo_geologico))
  ) +
  labs(
    title = "Porosidad vs logaritmo de la permeabilidad (rectas ajustadas)",
    y = "Porosidad [%]",
    x = "Permeabilidad [mD]"
  )  +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(size = 22, face = "bold", hjust = 0.5, color = "#003049"),
    axis.title.x    = element_text(margin = margin(t = 14)),
    axis.title.y    = element_text(margin = margin(r = 14)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(face = "bold", size = 13),
    strip.background= element_rect(fill = "gray90", color = NA))





### PLUS: PLANISFERIO CON LOS RESERVORIOS DEL DATASET

###### Crear un mapa y mostrar los pozos existentes
### Obtener mapa base del mundo como objeto sf
world <- ne_countries(scale = "medium", returnclass = "sf")
### Convertir df a objeto sf (usa Longitud y Latitud)
df_sf <- df%>%
  filter(!is.na(longitud), !is.na(latitud)) %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
### Graficamos el mapa
ggplot() +
  geom_sf(data = world, fill = "#F2F2F2", colour = "black", linewidth = .35) + # Tierra en gris muy claro y bordes finos gris medio
  geom_sf(data = df_sf,  # Puntos: fill azul, halo blanco fino
          shape = 21,            # círculo con borde
          fill  = "#386890",     # interior
          colour= "white",       # halo (stroke)
          stroke = 0.8,          # grosor del halo
          size   = 4,
          alpha  = .8) +        # súper-puesto = más oscuro
  coord_sf(expand = FALSE) +     # ocupa toda la slide
  theme_void() +                 # sin ejes
  theme(
    plot.background  = element_rect(fill = "#e6f1fa", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )
### Guardamos el png
ggsave("mapa_yacimientos_transp.png", width = 16, height = 9, units = "in", dpi = 300, bg = "transparent")

