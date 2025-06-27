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

