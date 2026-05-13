###############################################################################
# ECOSYSTEM EXTRACTION — Capra ibex & Vaccinium myrtillus
# Region: Canton of Valais, Switzerland
# Reads occurrences directly from matrix_full.csv
# Requires: WorldEcosystem.tif and WorldEcosystem_metadata_2.tsv
###############################################################################

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(geodata)

sf_use_s2(FALSE)

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/matrix_full.csv"

matrix_full <- read.csv(matrix_path)

cat("Matrix loaded —", nrow(matrix_full), "rows,", ncol(matrix_full), "cols\n")
table(matrix_full$species_short)

ecosystem_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/WorldEcosystem.tif"

# =============================================================================
# 3) LOAD VALAIS POLYGON
# =============================================================================

swiss_cantons    <- gadm(country = "CHE", level = 1, path = tempdir())
swiss_cantons_sf <- st_as_sf(swiss_cantons)
valais           <- swiss_cantons_sf[swiss_cantons_sf$NAME_1 == "Valais", ]

# =============================================================================
# 4) LOAD AND CROP ECOSYSTEM RASTER TO VALAIS
# =============================================================================

ecosystem_raster <- raster(ecosystem_path)
print(ecosystem_raster)

# Crop + mask directement avec le polygone Valais
r2               <- crop(ecosystem_raster, extent(valais))
ecosystem_valais <- mask(r2, as(valais, "Spatial"))

# Visualisation avec ggplot
eco_df <- as.data.frame(ecosystem_valais, xy = TRUE) %>%
  rename(ecosystem = 3) %>%
  filter(!is.na(ecosystem))

ecosystem_raster = ggplot() +
  geom_raster(data = eco_df, aes(x = x, y = y, fill = ecosystem)) +
  geom_sf(data = valais, fill = NA, color = "black", linewidth = 0.8) +
  scale_fill_viridis_c(name = "Ecosystem code") +
  coord_sf(xlim = c(6.77, 8.48), ylim = c(45.86, 46.66)) +
  labs(
    title = "Ecosystem Raster — Canton of Valais",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_size = 12)

print(ecosystem_raster)
## =============================================================================
# 5) CONVERT OCCURRENCES TO SPATIAL POINTS AND VISUALIZE
# =============================================================================

spatial_points <- SpatialPoints(
  coords      = matrix_full[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Convertir les points en data frame pour ggplot
points_df <- data.frame(
  longitude     = matrix_full$longitude,
  latitude      = matrix_full$latitude,
  species_short = matrix_full$species_short
)

occurences = ggplot() +
  geom_raster(data = eco_df, aes(x = x, y = y, fill = ecosystem)) +
  geom_sf(data = valais, fill = NA, color = "black", linewidth = 0.8) +
  geom_point(
    data  = points_df,
    aes(x = longitude, y = latitude, color = species_short),
    size  = 0.8, alpha = 0.7
  ) +
  scale_fill_viridis_c(name = "Ecosystem code") +
  scale_color_manual(
    values = c("Ibex" = "#D95F02", "Bilberry" = "#e2f90d"),
    name   = "Species"
  ) +
  coord_sf(xlim = c(6.77, 8.48), ylim = c(45.86, 46.66)) +
  labs(
    title = "Species occurrences on Ecosystem Map — Valais",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(10, 10, 10, 10))

print(occurences)

# =============================================================================
# 6) EXTRACT ECOSYSTEM VALUES AT EACH OCCURRENCE POINT
# =============================================================================

eco_values <- raster::extract(ecosystem_valais, spatial_points)

head(eco_values)

# =============================================================================
# 7) ADD ECOSYSTEM CODE TO THE TABLE
# =============================================================================

matrix_full_eco <- data.frame(matrix_full, eco_values)

head(matrix_full_eco)

# =============================================================================
# 8) LOAD ECOSYSTEM METADATA
# =============================================================================

metadata_eco <- read.delim(
  "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/WorldEcosystem_metadata_2.tsv"
)

head(metadata_eco)

# =============================================================================
# 9) MERGE ECOSYSTEM CODES WITH METADATA
# =============================================================================

matrix_full_eco <- merge(
  matrix_full_eco,
  metadata_eco,
  by.x = "eco_values",
  by.y = "Value",
  all.x = TRUE  # garder tous les points même si pas de correspondance
)

head(matrix_full_eco)
cat("Columns after merge:", names(matrix_full_eco), "\n")

# =============================================================================
# 10) VISUALIZATION — Observations per ecosystem type by species
# =============================================================================

# Nombre d'observations par catégorie climatique et espèce
p_eco <- ggplot(
  matrix_full_eco %>% filter(!is.na(Climate_Re)),
  aes(x = Climate_Re, fill = species_short)
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title = "Count of observations by climate category and species — Valais",
    x     = "Climate category",
    y     = "Number of observations",
    fill  = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_eco)
ggsave("fig_ecosystem_climate.png", p_eco, width = 10, height = 6, dpi = 300)

# Distribution par type de végétation (Landcover)
p_landcover <- ggplot(
  matrix_full_eco %>% filter(!is.na(Landcover)),
  aes(x = Landcover, fill = species_short)
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title = "Count of observations by landcover type and species — Valais",
    x     = "Landcover type",
    y     = "Number of observations",
    fill  = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p_landcover)
ggsave("fig_ecosystem_landcover.png", p_landcover, width = 10, height = 6, dpi = 300)

# Distribution par type de relief (Landforms)
p_landforms <- ggplot(
  matrix_full_eco %>% filter(!is.na(Landforms)),
  aes(x = Landforms, fill = species_short)
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title = "Count of observations by landform type and species — Valais",
    x     = "Landform type",
    y     = "Number of observations",
    fill  = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_landforms)
ggsave("fig_ecosystem_landforms.png", p_landforms, width = 9, height = 6, dpi = 300)

# =============================================================================
# 11) UPDATE MATRIX
# =============================================================================

# Une seule valeur par coordonnée unique
new_cols_eco <- matrix_full_eco %>%
  dplyr::group_by(longitude, latitude, species_short) %>%
  dplyr::summarise(
    eco_values  = first(eco_values),
    Temperature = first(Temperatur),
    Moisture    = first(Moisture),
    Landcover   = first(Landcover),
    Landforms   = first(Landforms),
    Climate_Re  = first(Climate_Re),
    W_Ecosystm  = first(W_Ecosystm),
    .groups     = "drop"
  )

matrix_full_updated <- read.csv(matrix_path)

matrix_full_updated <- matrix_full_updated %>%
  left_join(new_cols_eco, by = c("longitude", "latitude", "species_short"))

write.csv(matrix_full_updated, matrix_path, row.names = FALSE)
cat("Matrix updated —", ncol(matrix_full_updated), "columns,",
    nrow(matrix_full_updated), "rows\n")
cat("New columns: eco_values, Temperature, Moisture, Landcover,",
    "Landforms, Climate_Re, W_Ecosystm\n")



