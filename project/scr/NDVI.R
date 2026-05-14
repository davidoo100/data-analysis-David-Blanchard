###############################################################################
# NDVI EXTRACTION FROM MODIS — Capra ibex & Vaccinium myrtillus
# Region: Canton of Valais, Switzerland
# Reads occurrences directly from matrix_full.csv
###############################################################################

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
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
# 4) EXPORT VALAIS POLYGON FOR APPEEARS UPLOAD
# =============================================================================

data_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data"

st_write(valais, file.path(data_path, "valais.geojson"), delete_dsn = TRUE)

plot(st_geometry(valais), col = "lightgray",
     main = "Canton du Valais — NDVI extraction zone")

# =============================================================================
# 5) READ ALL NDVI RASTERS
# =============================================================================

ndvi_path <- file.path(data_path, "NDVI")

manual_tif <- list.files(
  ndvi_path,
  pattern    = "NDVI.*\\.tif$",
  full.names = TRUE,
  recursive  = TRUE
)

cat("NDVI files found:", length(manual_tif), "\n")
print(manual_tif)

# Extraire les dates depuis les noms de fichiers (doyYYYYDDD)
extract_date <- function(filepath) {
  doy_str <- regmatches(filepath, regexpr("doy(\\d{7})", filepath))
  year    <- as.integer(substr(doy_str, 4, 7))
  doy     <- as.integer(substr(doy_str, 8, 10))
  as.Date(doy - 1, origin = paste0(year, "-01-01"))
}

file_dates <- sapply(manual_tif, extract_date)
file_dates <- as.Date(file_dates, origin = "1970-01-01")
cat("Dates found:", format(file_dates), "\n")

# =============================================================================
# 6) CLIP ALL RASTERS TO VALAIS AND COMPUTE MEAN
# =============================================================================

ndvi_stack  <- rast(manual_tif)
valais_vect <- vect(valais)

# Reprojeter le polygone dans le CRS du raster
valais_vect_proj <- project(valais_vect, crs(ndvi_stack))

# Crop + mask
ndvi_stack_valais <- crop(ndvi_stack,  valais_vect_proj)
ndvi_stack_valais <- mask(ndvi_stack_valais, valais_vect_proj)

# NDVI moyen sur toutes les dates
ndvi_mean <- mean(ndvi_stack_valais, na.rm = TRUE)

cat("NDVI summary (mean over all dates):\n")
print(summary(values(ndvi_mean)))

# =============================================================================
# 7) CONVERT OCCURRENCES TO SPATIAL POINTS
# =============================================================================

points_vect      <- vect(occ_valais, geom = c("longitude", "latitude"),
                          crs = "EPSG:4326")
points_vect_proj <- project(points_vect, crs(ndvi_mean))

# =============================================================================
# 8) EXTRACT MEAN NDVI + SEASONAL VARIABILITY
# =============================================================================

# NDVI moyen
ndvi_mean_values <- terra::extract(ndvi_mean, points_vect_proj)[, 2]

# NDVI par date (pour la variabilité saisonnière)
ndvi_by_date <- terra::extract(ndvi_stack_valais, points_vect_proj)

# NDVI max et min saisonniers
ndvi_max_values <- apply(ndvi_by_date[, -1], 1, max, na.rm = TRUE)
ndvi_min_values <- apply(ndvi_by_date[, -1], 1, min, na.rm = TRUE)
ndvi_range_values <- ndvi_max_values - ndvi_min_values

summary(ndvi_mean_values)

# =============================================================================
# 9) ADD NDVI VARIABLES TO TABLE
# =============================================================================

occ_valais_ndvi <- data.frame(
  occ_valais,
  NDVI_mean  = ndvi_mean_values,
  NDVI_max   = ndvi_max_values,
  NDVI_min   = ndvi_min_values,
  NDVI_range = ndvi_range_values
)

# Statistiques par espèce
cat("\n=== NDVI STATISTICS BY SPECIES ===\n")
occ_valais_ndvi %>%
  filter(!is.na(NDVI_mean)) %>%
  group_by(species_short) %>%
  summarise(
    mean_NDVI  = round(mean(NDVI_mean),  3),
    max_NDVI   = round(mean(NDVI_max),   3),
    min_NDVI   = round(mean(NDVI_min),   3),
    range_NDVI = round(mean(NDVI_range), 3)
  ) %>% print()

# =============================================================================
# FIGURE 1 — MAP: NDVI moyen + occurrences
# =============================================================================

# Reprojeter le raster en WGS84 pour ggplot
ndvi_mean_wgs84 <- project(ndvi_mean, "EPSG:4326")

ndvi_df <- as.data.frame(ndvi_mean_wgs84, xy = TRUE) %>%
  rename(NDVI = 3) %>%
  filter(!is.na(NDVI))

points_df <- data.frame(
  longitude     = occ_valais$longitude,
  latitude      = occ_valais$latitude,
  species_short = occ_valais$species_short
)

p_map <- ggplot() +
  geom_raster(data = ndvi_df, aes(x = x, y = y, fill = NDVI)) +
  geom_sf(data = valais, fill = NA, color = "white", linewidth = 0.8) +
  scale_fill_viridis_c(option = "viridis", name = "Mean NDVI") +
  geom_point(data = points_df,
             aes(x = longitude, y = latitude, color = species_short),
             size = 0.8, alpha = 0.7) +
  scale_color_manual(
    values = c("Ibex" = "#f700f7", "Bilberry" = "#ff0000"),
    name   = "Species"
  ) +
  coord_sf(xlim = c(6.77, 8.48), ylim = c(45.86, 46.66)) +
  labs(
    title    = "Mean NDVI with species occurrences — Valais",
    subtitle = "MODIS MOD13Q1 — Mean over summer 2025",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "right")

print(p_map)
ggsave(file.path(figures_path, "fig_ndvi_map.png"),
       p_map, width = 10, height = 7, dpi = 300)

# =============================================================================
# FIGURE 2 — Density plot NDVI moyen
# =============================================================================

p_density <- ggplot(
  occ_valais_ndvi %>% filter(!is.na(NDVI_mean)),
  aes(x = NDVI_mean, fill = species_short, color = species_short)
) +
  geom_density(alpha = 0.4, adjust = 1.5, linewidth = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title    = "Mean NDVI distribution by species — Valais",
    subtitle = "MODIS MOD13Q1 — Mean over summer 2025",
    x = "Mean NDVI", y = "Density",
    fill = "Species", color = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_density)
ggsave(file.path(figures_path, "fig_ndvi_density.png"),
       p_density, width = 9, height = 6, dpi = 300)

# =============================================================================
# FIGURE 3 — Boxplot NDVI avec test de Wilcoxon
# =============================================================================

wilcox_ndvi <- wilcox.test(
  NDVI_mean ~ species_short,
  data = occ_valais_ndvi %>% filter(!is.na(NDVI_mean))
)

cat("\n=== WILCOXON TEST: NDVI Ibex vs Bilberry ===\n")
print(wilcox_ndvi)

p_value_label <- ifelse(wilcox_ndvi$p.value < 0.001, "p < 0.001",
                 ifelse(wilcox_ndvi$p.value < 0.01,  "p < 0.01",
                 ifelse(wilcox_ndvi$p.value < 0.05,  "p < 0.05", "p > 0.05")))

p_boxplot <- ggplot(
  occ_valais_ndvi %>% filter(!is.na(NDVI_mean)),
  aes(x = species_short, y = NDVI_mean,
      fill = species_short, color = species_short)
) +
  geom_boxplot(alpha = 0.4, outlier.size = 0.8) +
  geom_jitter(width = 0.15, size = 0.5, alpha = 0.3) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  annotate("text", x = 1.5,
           y = max(occ_valais_ndvi$NDVI_mean, na.rm = TRUE) * 0.98,
           label = paste("Wilcoxon:", p_value_label),
           size = 4, color = "grey30", fontface = "italic") +
  labs(
    title    = "NDVI comparison by species — Valais",
    subtitle = paste("Wilcoxon rank-sum test:", p_value_label),
    x = "Species", y = "Mean NDVI",
    fill = "Species", color = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

print(p_boxplot)
ggsave(file.path(figures_path, "fig_ndvi_boxplot.png"),
       p_boxplot, width = 7, height = 6, dpi = 300)

# =============================================================================
# FIGURE 4 — Courbe temporelle NDVI par espèce
# =============================================================================

# Calculer le NDVI moyen par date et par espèce
temporal_data <- lapply(seq_along(file_dates), function(i) {
  date_vals <- terra::extract(ndvi_stack_valais[[i]], points_vect_proj)[, 2]
  data.frame(
    species_short = occ_valais$species_short,
    date          = file_dates[i],
    NDVI          = date_vals
  )
})

temporal_df <- bind_rows(temporal_data) %>%
  filter(!is.na(NDVI)) %>%
  group_by(species_short, date) %>%
  summarise(
    mean_NDVI = mean(NDVI, na.rm = TRUE),
    se_NDVI   = sd(NDVI, na.rm = TRUE) / sqrt(n()),
    .groups   = "drop"
  )

p_temporal <- ggplot(temporal_df,
  aes(x = date, y = mean_NDVI,
      color = species_short, fill = species_short)) +
  geom_ribbon(aes(ymin = mean_NDVI - se_NDVI,
                  ymax = mean_NDVI + se_NDVI),
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(
    title    = "Seasonal NDVI dynamics by species — Valais (2025)",
    subtitle = "Mean ± SE across all occurrence points — MODIS MOD13Q1 (16-day)",
    x = "Date", y = "Mean NDVI",
    color = "Species", fill = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position  = "bottom",
        axis.text.x      = element_text(angle = 45, hjust = 1))

print(p_temporal)
ggsave(file.path(figures_path, "fig_ndvi_temporal.png"),
       p_temporal, width = 10, height = 6, dpi = 300)

# =============================================================================
# FIGURE 5 — NDVI vs Elevation par espèce
# =============================================================================

# Joindre avec l'élévation si disponible dans la matrice
if ("elevation" %in% names(occ_valais)) {

  p_ndvi_elev <- ggplot(
    occ_valais_ndvi %>% filter(!is.na(NDVI_mean), !is.na(elevation)),
    aes(x = elevation, y = NDVI_mean, color = species_short)
  ) +
    geom_point(size = 1.2, alpha = 0.5) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
    labs(
      title    = "NDVI vs Elevation by species — Valais",
      subtitle = "NDVI decreases above the vegetation line (~2500m)",
      x = "Elevation (m a.s.l.)", y = "Mean NDVI",
      color = "Species"
    ) +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom")

  print(p_ndvi_elev)
  ggsave(file.path(figures_path, "fig_ndvi_vs_elevation.png"),
         p_ndvi_elev, width = 9, height = 6, dpi = 300)

} else {
  cat("Elevation not in matrix yet — run scr_elevation.R first for fig_ndvi_vs_elevation\n")
}

# =============================================================================
# UPDATE MATRIX
# =============================================================================

matrix_full <- read.csv(matrix_path)

new_cols_ndvi <- occ_valais_ndvi %>%
  dplyr::group_by(longitude, latitude, species_short) %>%
  dplyr::summarise(
    NDVI_mean  = mean(NDVI_mean,  na.rm = TRUE),
    NDVI_max   = mean(NDVI_max,   na.rm = TRUE),
    NDVI_min   = mean(NDVI_min,   na.rm = TRUE),
    NDVI_range = mean(NDVI_range, na.rm = TRUE),
    .groups    = "drop"
  )

matrix_full <- matrix_full %>%
  left_join(new_cols_ndvi,
            by = c("longitude", "latitude", "species_short"))

write.csv(matrix_full, matrix_path, row.names = FALSE)
cat("Matrix updated —", ncol(matrix_full), "columns,", nrow(matrix_full), "rows\n")
