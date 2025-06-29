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

