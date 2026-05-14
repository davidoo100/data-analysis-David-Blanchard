###############################################################################
# ELEVATION, SLOPE & ASPECT EXTRACTION — Capra ibex & Vaccinium myrtillus
# Region: Canton of Valais, Switzerland
# Reads occurrences directly from matrix_full.csv
###############################################################################

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(sf)
library(elevatr)
library(raster)
library(terra)
library(ggplot2)
library(dplyr)
library(geodata)

sf_use_s2(FALSE)

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/matrix_full.csv"

occ_valais <- read.csv(matrix_path)

cat("Matrix loaded —", nrow(occ_valais), "rows,", ncol(occ_valais), "cols\n")
table(occ_valais$species_short)

# Dossier figures
figures_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/figures"
dir.create(figures_path, showWarnings = FALSE)

# =============================================================================
# 3) LOAD VALAIS POLYGON
# =============================================================================

swiss_cantons    <- gadm(country = "CHE", level = 1, path = tempdir())
swiss_cantons_sf <- st_as_sf(swiss_cantons)
valais           <- swiss_cantons_sf[swiss_cantons_sf$NAME_1 == "Valais", ]

# =============================================================================
# 4) DOWNLOAD ELEVATION RASTER (z=9 for better resolution)
# =============================================================================

cat("Downloading DEM at z=9 (~150m resolution)...\n")
elevation_valais <- get_elev_raster(valais, z = 9)

# Convertir en terra pour calculer pente et aspect
elevation_terra <- rast(elevation_valais)

# Calculer la pente (en degrés)
slope_terra <- terrain(elevation_terra, v = "slope", unit = "degrees")

# Calculer l'aspect (en degrés, 0=Nord, 90=Est, 180=Sud, 270=Ouest)
aspect_terra <- terrain(elevation_terra, v = "aspect", unit = "degrees")

elev_df <- as.data.frame(elevation_valais, xy = TRUE) %>%
  rename(elevation = 3) %>%
  filter(!is.na(elevation))

elevation = ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = valais, fill = NA, color = "black", linewidth = 0.8) +
  scale_fill_gradientn(
    colours  = c("#2d6a4f", "#74c69d", "#d9ed92", "#ffd60a", "#e85d04", "#cccccc"),
    values   = scales::rescale(c(300, 800, 1500, 2200, 3000, 4500)),
    name     = "Elevation (m)"
  ) +
  coord_sf(xlim = c(6.77, 8.48), ylim = c(45.86, 46.66)) +
  labs(title = "Elevation raster — Canton of Valais (z=9)",
       x = "Longitude", y = "Latitude") +
  theme_classic(base_size = 12)
print(elevation)
# Visualisation de la pente
slope_df <- as.data.frame(slope_terra, xy = TRUE) %>%
  rename(slope = 3) %>%
  filter(!is.na(slope))

slope = ggplot() +
  geom_raster(data = slope_df, aes(x = x, y = y, fill = slope)) +
  geom_sf(data = valais, fill = NA, color = "black", linewidth = 0.8) +
  scale_fill_viridis_c(name = "Slope (°)", option = "magma") +
  coord_sf(xlim = c(6.77, 8.48), ylim = c(45.86, 46.66)) +
  labs(title = "Slope — Canton of Valais",
       x = "Longitude", y = "Latitude") +
  theme_classic(base_size = 12)
print(slope)
# =============================================================================
# 5) CONVERT OCCURRENCES TO SPATIAL POINTS
# =============================================================================

spatial_points <- SpatialPoints(
  coords      = occ_valais[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Visualisation des points sur le raster d'élévation
points_df <- data.frame(
  longitude     = occ_valais$longitude,
  latitude      = occ_valais$latitude,
  species_short = occ_valais$species_short
)

aspect = ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = valais, fill = NA, color = "black", linewidth = 0.8) +
  scale_fill_gradientn(
    colours = c("#2d6a4f", "#74c69d", "#d9ed92", "#ffd60a", "#e85d04", "#cccccc"),
    values  = scales::rescale(c(300, 800, 1500, 2200, 3000, 4500)),
    name    = "Elevation (m)"
  ) +
  geom_point(
    data  = points_df,
    aes(x = longitude, y = latitude, color = species_short),
    size  = 0.8, alpha = 0.7
  ) +
  scale_color_manual(
    values = c("Ibex" = "#e630c4", "Bilberry" = "#f10606"),
    name   = "Species"
  ) +
  coord_sf(xlim = c(6.77, 8.48), ylim = c(45.86, 46.66)) +
  labs(
    title = "Species occurrences on elevation map — Valais",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "right")
print(aspect)
# =============================================================================
# 6) EXTRACT ELEVATION, SLOPE AND ASPECT VALUES
# =============================================================================

elevation_values <- raster::extract(elevation_valais, spatial_points)

# Convertir les points pour terra::extract
points_terra <- vect(
  occ_valais,
  geom = c("longitude", "latitude"),
  crs  = "EPSG:4326"
)

slope_values  <- terra::extract(slope_terra,  points_terra)[, 2]
aspect_values <- terra::extract(aspect_terra, points_terra)[, 2]

# Catégoriser l'aspect en 4 orientations principales
aspect_cat <- cut(
  aspect_values,
  breaks = c(-1, 45, 135, 225, 315, 360),
  labels = c("North", "East", "South", "West", "North"),
  include.lowest = TRUE
)
# Simplifier en Nord/Sud pour l'analyse bouquetin
aspect_NS <- ifelse(aspect_values >= 135 & aspect_values <= 315, "South-facing", "North-facing")

cat("\nExtraction summary:\n")
cat("Elevation — min:", round(min(elevation_values, na.rm=TRUE)),
    "max:", round(max(elevation_values, na.rm=TRUE)), "m\n")
cat("Slope     — min:", round(min(slope_values, na.rm=TRUE)),
    "max:", round(max(slope_values, na.rm=TRUE)), "degrees\n")

# =============================================================================
# 7) ADD ALL VARIABLES TO THE TABLE
# =============================================================================

occ_valais_elev <- data.frame(
  occ_valais,
  elevation  = elevation_values,
  slope      = slope_values,
  aspect     = aspect_values,
  aspect_NS  = aspect_NS
)

# Statistiques par espèce
cat("\n=== STATISTICS BY SPECIES ===\n")
occ_valais_elev %>%
  filter(!is.na(elevation)) %>%
  group_by(species_short) %>%
  summarise(
    mean_elev   = round(mean(elevation),   0),
    median_elev = round(median(elevation), 0),
    min_elev    = round(min(elevation),    0),
    max_elev    = round(max(elevation),    0),
    mean_slope  = round(mean(slope, na.rm=TRUE), 1),
    pct_south   = round(mean(aspect_NS == "South-facing", na.rm=TRUE) * 100, 1)
  ) %>% print()

# =============================================================================
# 8) FIGURE 1 — Density plot élévation
# =============================================================================

p_density <- ggplot(
  occ_valais_elev %>% filter(!is.na(elevation)),
  aes(x = elevation, fill = species_short, color = species_short)
) +
  geom_density(alpha = 0.4, adjust = 1.5, linewidth = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title    = "Elevation distribution by species — Valais",
    subtitle = "Elevation extracted from AWS DEM (elevatr, z = 9)",
    x = "Elevation (m a.s.l.)", y = "Density",
    fill = "Species", color = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_density)
ggsave(file.path(figures_path, "fig_elevation_density.png"),
       p_density, width = 9, height = 6, dpi = 300)

# =============================================================================
# 9) FIGURE 2 — Boxplot élévation avec test statistique
# =============================================================================

# Test de Wilcoxon (non-paramétrique, plus adapté que t-test)
wilcox_result <- wilcox.test(
  elevation ~ species_short,
  data = occ_valais_elev %>% filter(!is.na(elevation))
)

cat("\n=== WILCOXON TEST: Elevation Ibex vs Bilberry ===\n")
print(wilcox_result)

p_value_label <- ifelse(wilcox_result$p.value < 0.001, "p < 0.001",
                 ifelse(wilcox_result$p.value < 0.01,  "p < 0.01",
                 ifelse(wilcox_result$p.value < 0.05,  "p < 0.05", "p > 0.05")))

p_boxplot <- ggplot(
  occ_valais_elev %>% filter(!is.na(elevation)),
  aes(x = species_short, y = elevation,
      fill = species_short, color = species_short)
) +
  geom_boxplot(alpha = 0.4, outlier.size = 0.8) +
  geom_jitter(width = 0.15, size = 0.5, alpha = 0.3) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  annotate("text", x = 1.5, y = max(occ_valais_elev$elevation, na.rm=TRUE) * 0.98,
           label = paste("Wilcoxon:", p_value_label),
           size = 4, color = "grey30", fontface = "italic") +
  labs(
    title    = "Elevation comparison by species — Valais",
    subtitle = paste("Wilcoxon rank-sum test:", p_value_label),
    x = "Species", y = "Elevation (m a.s.l.)",
    fill = "Species", color = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

print(p_boxplot)
ggsave(file.path(figures_path, "fig_elevation_boxplot.png"),
       p_boxplot, width = 7, height = 6, dpi = 300)

# =============================================================================
# 10) FIGURE 3 — Slope vs Elevation par espèce
# =============================================================================

p_slope_elev <- ggplot(
  occ_valais_elev %>% filter(!is.na(elevation), !is.na(slope)),
  aes(x = elevation, y = slope, color = species_short)
) +
  geom_point(size = 1.2, alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title    = "Slope vs Elevation by species — Valais",
    subtitle = "Ibex typically occupies steeper terrain than Bilberry",
    x = "Elevation (m a.s.l.)", y = "Slope (degrees)",
    color = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_slope_elev)
ggsave(file.path(figures_path, "fig_slope_vs_elevation.png"),
       p_slope_elev, width = 9, height = 6, dpi = 300)

# =============================================================================
# 11) FIGURE 4 — Aspect (orientation) par espèce
# =============================================================================

p_aspect <- ggplot(
  occ_valais_elev %>% filter(!is.na(aspect_NS)),
  aes(x = aspect_NS, fill = species_short)
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title    = "Slope aspect by species — Valais",
    subtitle = "South-facing slopes receive more solar radiation",
    x = "Aspect", y = "Number of occurrences",
    fill = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_aspect)
ggsave(file.path(figures_path, "fig_aspect_distribution.png"),
       p_aspect, width = 7, height = 6, dpi = 300)

# =============================================================================
# 12) UPDATE MATRIX
# =============================================================================

matrix_full <- read.csv(matrix_path)

new_cols_elev <- occ_valais_elev %>%
  dplyr::group_by(longitude, latitude, species_short) %>%
  dplyr::summarise(
    elevation = mean(elevation, na.rm = TRUE),
    slope     = mean(slope,     na.rm = TRUE),
    aspect    = mean(aspect,    na.rm = TRUE),
    aspect_NS = first(aspect_NS),
    .groups   = "drop"
  )

matrix_full <- matrix_full %>%
  left_join(new_cols_elev, by = c("longitude", "latitude", "species_short"))

write.csv(matrix_full, matrix_path, row.names = FALSE)
cat("Matrix updated —", ncol(matrix_full), "columns,", nrow(matrix_full), "rows\n")
cat("New columns: elevation, slope, aspect, aspect_NS\n")
