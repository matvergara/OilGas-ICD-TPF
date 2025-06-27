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
