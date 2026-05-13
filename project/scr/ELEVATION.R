###############################################################################
# ELEVATION EXTRACTION — Capra ibex & Vaccinium myrtillus
# Region: Canton of Valais, Switzerland
# Reads occurrences directly from matrix_full.csv
###############################################################################

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(sf)
library(elevatr)
library(raster)
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
# 4) DOWNLOAD ELEVATION RASTER FOR THE VALAIS
# =============================================================================

elevation_valais <- get_elev_raster(valais, z = 8)

plot(elevation_valais, 
     main = "Elevation raster — Canton of Valais",
     xlim = c(6.7, 8.5),
     ylim = c(45.6, 47.0))
plot(st_geometry(valais), add = TRUE, border = "black", lwd = 1.5)

# =============================================================================
# 5) CONVERT OCCURRENCES TO SPATIAL POINTS
# =============================================================================

spatial_points <- SpatialPoints(
  coords      = occ_valais[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

plot(elevation_valais, 
     main = "Elevation raster — Canton of Valais",
     xlim = c(6.7, 8.5),
     ylim = c(45.6, 47.0))
plot(st_geometry(valais), add = TRUE, border = "black", lwd = 1.5)

plot(spatial_points, add = TRUE, pch = 16, cex = 0.5,
     col = ifelse(occ_valais$species_short == "Ibex", "#D95F02", "#1B9E77"))
legend(
  x      = 8.1,
  y      = 46.7,
  legend = c("Ibex", "Bilberry"),
  col    = c("#D95F02", "#1B9E77"),
  pch    = 16, cex = 0.9,
  bg     = "white", box.col = "grey50"
)

# =============================================================================
# 6) EXTRACT ELEVATION VALUES
# =============================================================================

elevation_values <- raster::extract(elevation_valais, spatial_points)
head(elevation_values)

# =============================================================================
# 7) ADD ELEVATION TO THE TABLE
# =============================================================================

occ_valais_elev <- data.frame(occ_valais, elevation = elevation_values)

occ_valais_elev %>%
  filter(!is.na(elevation)) %>%
  group_by(species_short) %>%
  summarise(
    mean_elev   = round(mean(elevation),   0),
    median_elev = round(median(elevation), 0),
    min_elev    = round(min(elevation),    0),
    max_elev    = round(max(elevation),    0)
  )

# =============================================================================
# 8) VISUALIZATION — Elevation distribution by species
# =============================================================================

p_elev <- ggplot(
  occ_valais_elev %>% filter(!is.na(elevation)),
  aes(x = elevation, fill = species_short, color = species_short)
) +
  geom_density(alpha = 0.4, adjust = 1.5, linewidth = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title    = "Elevation distribution by species — Valais",
    subtitle = "Elevation extracted from AWS DEM (elevatr, z = 8)",
    x = "Elevation (m a.s.l.)", y = "Density",
    fill = "Species", color = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_elev)
ggsave("fig_elevation_distribution.png", p_elev, width = 9, height = 6, dpi = 300)

# =============================================================================
# 9) UPDATE MATRIX
# =============================================================================

matrix_full <- read.csv(matrix_path)

# Une seule valeur d'élévation par coordonnée unique
new_cols_elev <- occ_valais_elev %>%
  group_by(longitude, latitude, species_short) %>%
  summarise(elevation = mean(elevation, na.rm = TRUE), .groups = "drop")

matrix_full <- matrix_full %>%
  left_join(new_cols_elev, by = c("longitude", "latitude", "species_short"))

write.csv(matrix_full, matrix_path, row.names = FALSE)
