# ------------------------------------------------------------
# Mapa global de pozos del dataset
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

ggsave("mapa_yacimientos_transp.png", width = 16, height = 9, units = "in", dpi = 300, bg = "transparent")