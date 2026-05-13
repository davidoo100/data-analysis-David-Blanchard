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
library(geodata)

sf_use_s2(FALSE)

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/matrix_full.csv"

occ_valais <- read.csv(matrix_path)

cat("Matrix loaded —", nrow(occ_valais), "rows,", ncol(occ_valais), "cols\n")
table(occ_valais$species_short)

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

plot(st_geometry(valais), col = "lightgray", main = "Canton du Valais — NDVI extraction zone")

# =============================================================================
# 5) READ ALL NDVI RASTERS AND COMPUTE THE MEAN
# =============================================================================

manual_tif <- list.files(
  data_path,
  pattern    = "NDVI.*\\.tif$",
  full.names = TRUE,
  recursive  = TRUE
)

cat("NDVI files found:", length(manual_tif), "\n")
print(manual_tif)

# Empiler tous les rasters et calculer la moyenne
ndvi_stack  <- rast(manual_tif)
ndvi_raster <- mean(ndvi_stack, na.rm = TRUE)

plot(ndvi_raster, main = "Mean NDVI (all available dates) — MODIS MOD13Q1")

# =============================================================================
# 6) CLIP THE RASTER TO THE VALAIS BORDER
# =============================================================================

valais_vect      <- vect(valais)
valais_vect_proj <- project(valais_vect, crs(ndvi_raster))

ndvi_valais <- crop(ndvi_raster,  valais_vect_proj)
ndvi_valais <- mask(ndvi_valais,  valais_vect_proj)

plot(ndvi_valais, main = "NDVI raster — Canton du Valais")
plot(valais_vect_proj, add = TRUE, border = "white", lwd = 1.5)

# =============================================================================
# 7) CONVERT OCCURRENCES TO SPATIAL POINTS
# =============================================================================

points_vect      <- vect(occ_valais, geom = c("longitude", "latitude"),
                          crs = "EPSG:4326")
points_vect_proj <- project(points_vect, crs(ndvi_valais))

plot(ndvi_valais, main = "Species occurrences on NDVI raster — Valais")
plot(valais_vect_proj, add = TRUE, border = "white", lwd = 1.5)
plot(points_vect_proj, add = TRUE, pch = 16, cex = 0.5,
     col = ifelse(occ_valais$species_short == "Ibex", "#D95F02", "#e7f801"))
legend(
  x      = 640000, y = 5180000,
  legend = c("Ibex", "Bilberry"),
  col    = c("#D95F02", "#e7f801"),
  pch = 16, cex = 0.9, bg = "white", box.col = "grey50"
)

# =============================================================================
# 8) EXTRACT NDVI VALUES
# =============================================================================

ndvi_values <- terra::extract(ndvi_valais, points_vect_proj)

# Vérifier l'échelle — AppEEARS retourne déjà des valeurs entre -1 et 1
summary(ndvi_values[, 2])

# =============================================================================
# 9) ADD NDVI TO THE TABLE
# =============================================================================

occ_valais_ndvi <- data.frame(occ_valais, NDVI = ndvi_values[, 2])

occ_valais_ndvi %>%
  filter(!is.na(NDVI)) %>%
  group_by(species_short) %>%
  summarise(
    mean_NDVI   = round(mean(NDVI),   3),
    median_NDVI = round(median(NDVI), 3),
    min_NDVI    = round(min(NDVI),    3),
    max_NDVI    = round(max(NDVI),    3)
  )

# =============================================================================
# 10) VISUALIZATION — NDVI distribution by species
# =============================================================================

p_ndvi <- ggplot(
  occ_valais_ndvi %>% filter(!is.na(NDVI)),
  aes(x = NDVI, fill = species_short, color = species_short)
) +
  geom_density(alpha = 0.4, adjust = 1.5, linewidth = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title    = "NDVI distribution by species — Valais (MODIS MOD13Q1)",
    subtitle = "NDVI = Normalized Difference Vegetation Index (scale: -1 to 1)",
    x = "NDVI", y = "Density",
    fill = "Species", color = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_ndvi)
ggsave("fig_ndvi_distribution.png", p_ndvi, width = 9, height = 6, dpi = 300)

# =============================================================================
# 11) UPDATE MATRIX
# =============================================================================

matrix_full <- read.csv(matrix_path)

# Une seule valeur NDVI par coordonnée unique
new_cols_ndvi <- occ_valais_ndvi %>%
  group_by(longitude, latitude, species_short) %>%
  summarise(NDVI = mean(NDVI, na.rm = TRUE), .groups = "drop")

matrix_full <- matrix_full %>%
  left_join(new_cols_ndvi, by = c("longitude", "latitude", "species_short"))

write.csv(matrix_full, matrix_path, row.names = FALSE)
