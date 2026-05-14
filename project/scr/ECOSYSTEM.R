###############################################################################
# ECOSYSTEM EXTRACTION — Capra ibex & Vaccinium myrtillus
# Region: Canton of Valais, Switzerland
# Reads occurrences directly from matrix_full.csv
# Version 2 — chi-square test, map, treemap, heatmap, short labels
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
library(tidyr)

install.packages("treemapify")
library(treemapify)

sf_use_s2(FALSE)

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/matrix_full.csv"

matrix_full <- read.csv(matrix_path)

cat("Matrix loaded —", nrow(matrix_full), "rows,", ncol(matrix_full), "cols\n")
table(matrix_full$species_short)

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
# 4) LOAD AND CROP ECOSYSTEM RASTER TO VALAIS
# =============================================================================

ecosystem_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/WorldEcosystem.tif"

ecosystem_raster <- raster(ecosystem_path)
print(ecosystem_raster)

r2               <- crop(ecosystem_raster, extent(valais))
ecosystem_valais <- mask(r2, as(valais, "Spatial"))

# Convertir en data frame pour ggplot
eco_df <- as.data.frame(ecosystem_valais, xy = TRUE) %>%rename(ecosystem = 3) %>%filter(!is.na(ecosystem))

# =============================================================================
# 5) CONVERT OCCURRENCES TO SPATIAL POINTS
# =============================================================================

spatial_points <- SpatialPoints(coords      = matrix_full[, c("longitude", "latitude")],proj4string = CRS("+proj=longlat +datum=WGS84"))

points_df <- data.frame(longitude     = matrix_full$longitude,latitude      = matrix_full$latitude,species_short = matrix_full$species_short)

# =============================================================================
# 6) EXTRACT ECOSYSTEM VALUES
# =============================================================================

eco_values <- raster::extract(ecosystem_valais, spatial_points)
head(eco_values)

# =============================================================================
# 7) ADD ECOSYSTEM CODE TO TABLE
# =============================================================================

matrix_full_eco <- data.frame(matrix_full, eco_values)

# =============================================================================
# 8) LOAD METADATA AND MERGE
# =============================================================================

metadata_eco <- read.delim("/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/WorldEcosystem_metadata_2.tsv")

matrix_full_eco <- merge(matrix_full_eco,metadata_eco,by.x = "eco_values",by.y = "Value",all.x = TRUE)

# =============================================================================
# 9) CREATE SHORT LABELS for readability
# =============================================================================
# Combine Temperature + Landcover in a short label

matrix_full_eco <- matrix_full_eco %>%
  mutate(
    eco_short = paste(Temperatur, Landcover, sep = "\n"),
    climate_short = Climate_Re
  )

cat("\nMost common ecosystems:\n")
matrix_full_eco %>%count(climate_short, sort = TRUE) %>%head(10) %>%print()

# =============================================================================
# FIGURE 1 — MAP: Ecosystem raster + species occurrences
# =============================================================================

p_map <- ggplot() +
  geom_raster(data = eco_df, aes(x = x, y = y, fill = factor(ecosystem))) +
  geom_sf(data = valais, fill = NA, color = "black", linewidth = 0.8) +
  geom_point(
    data  = points_df,
    aes(x = longitude, y = latitude, color = species_short),
    size  = 0.8, alpha = 0.7
  ) +
  scale_fill_viridis_d(name = "Ecosystem code", guide = "none") +
  scale_color_manual(
    values = c("Ibex" = "#D95F02", "Bilberry" = "#ffd607"),
    name   = "Species"
  ) +
  coord_sf(xlim = c(6.77, 8.48), ylim = c(45.86, 46.66)) +
  labs(
    title    = "Species occurrences on Ecosystem Map — Valais",
    subtitle = "WorldEcosystems raster (Sayre et al.)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(10, 10, 10, 10),
        legend.position = "right")

print(p_map)
ggsave(file.path(figures_path, "fig_ecosystem_map.png"),
       p_map, width = 10, height = 7, dpi = 300)

# =============================================================================
# FIGURE 2 — BAR PLOT: Observations by climate category
# =============================================================================

p_climate <- ggplot(
  matrix_full_eco %>% filter(!is.na(Climate_Re)),
  aes(x = reorder(Climate_Re, -table(Climate_Re)[Climate_Re]),
      fill = species_short)
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title = "Observations by climate category and species — Valais",
    x = "Climate category", y = "Number of observations",
    fill = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(p_climate)
ggsave(file.path(figures_path, "fig_ecosystem_climate.png"),
       p_climate, width = 10, height = 6, dpi = 300)

# =============================================================================
# FIGURE 3 — TREEMAP: Proportion of ecosystem types per species
# =============================================================================

treemap_data <- matrix_full_eco %>%
  filter(!is.na(Landcover), !is.na(Temperatur)) %>%
  group_by(species_short, Temperatur, Landcover) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(label = paste(Temperatur, Landcover, sep = "\n"))

p_treemap_ibex <- ggplot(
  treemap_data %>% filter(species_short == "Ibex"),
  aes(area = n, fill = Landcover, label = label)
) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 10) +
  labs(title = "Ecosystem composition — Ibex", fill = "Landcover") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

p_treemap_bilberry <- ggplot(
  treemap_data %>% filter(species_short == "Bilberry"),
  aes(area = n, fill = Landcover, label = label)
) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 10) +
  labs(title = "Ecosystem composition — Bilberry", fill = "Landcover") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_treemap_ibex)
print(p_treemap_bilberry)

ggsave(file.path(figures_path, "fig_ecosystem_treemap_ibex.png"),
       p_treemap_ibex, width = 8, height = 6, dpi = 300)
ggsave(file.path(figures_path, "fig_ecosystem_treemap_bilberry.png"),
       p_treemap_bilberry, width = 8, height = 6, dpi = 300)

# =============================================================================
# FIGURE 4 — HEATMAP: Landcover x Landforms par espèce
# =============================================================================

heatmap_data <- matrix_full_eco %>%
  filter(!is.na(Landcover), !is.na(Landforms)) %>%
  group_by(species_short, Landcover, Landforms) %>%
  summarise(n = n(), .groups = "drop")

p_heatmap <- ggplot(heatmap_data,
  aes(x = Landforms, y = Landcover, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), size = 3, color = "white") +
  scale_fill_viridis_c(name = "Count", option = "plasma") +
  facet_wrap(~ species_short) +
  labs(
    title    = "Heatmap: Landcover × Landforms by species — Valais",
    subtitle = "Number of occurrences per ecosystem combination",
    x = "Landforms", y = "Landcover"
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p_heatmap)
ggsave(file.path(figures_path, "fig_ecosystem_heatmap.png"),
       p_heatmap, width = 12, height = 7, dpi = 300)

# =============================================================================
# FIGURE 5 — BAR PLOT: Landcover and Landforms
# =============================================================================

p_landcover <- ggplot(
  matrix_full_eco %>% filter(!is.na(Landcover)),
  aes(x = reorder(Landcover, -table(Landcover)[Landcover]),
      fill = species_short)
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(title = "Observations by landcover type — Valais",
       x = "Landcover", y = "Number of observations", fill = "Species") +
  theme_classic(base_size = 12) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(p_landcover)
ggsave(file.path(figures_path, "fig_ecosystem_landcover.png"),
       p_landcover, width = 10, height = 6, dpi = 300)

p_landforms <- ggplot(
  matrix_full_eco %>% filter(!is.na(Landforms)),
  aes(x = Landforms, fill = species_short)
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(title = "Observations by landform type — Valais",
       x = "Landforms", y = "Number of observations", fill = "Species") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_landforms)
ggsave(file.path(figures_path, "fig_ecosystem_landforms.png"),
       p_landforms, width = 9, height = 6, dpi = 300)

# =============================================================================
# STATISTICAL TEST — Chi-squared: ecosystem distribution by species
# =============================================================================

cat("\n=== CHI-SQUARED TEST: Landcover distribution by species ===\n")
landcover_table <- table(
  species  = matrix_full_eco$species_short,
  landcover = matrix_full_eco$Landcover
)
print(landcover_table)
chi_landcover <- chisq.test(landcover_table)
print(chi_landcover)

cat("\n=== CHI-SQUARED TEST: Climate category distribution by species ===\n")
climate_table <- table(
  species  = matrix_full_eco$species_short,
  climate  = matrix_full_eco$Climate_Re
)
chi_climate <- chisq.test(climate_table)
print(chi_climate)

cat("\n=== FISHER EXACT TEST: Landforms distribution by species ===\n")
landforms_table <- table(
  species   = matrix_full_eco$species_short,
  landforms = matrix_full_eco$Landforms
)
print(landforms_table)
fisher_landforms <- fisher.test(landforms_table, simulate.p.value = TRUE)
print(fisher_landforms)

# =============================================================================
# UPDATE MATRIX
# =============================================================================

matrix_full_updated <- read.csv(matrix_path)

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

matrix_full_updated <- matrix_full_updated %>%
  left_join(new_cols_eco, by = c("longitude", "latitude", "species_short"))

write.csv(matrix_full_updated, matrix_path, row.names = FALSE)
cat("Matrix updated —", ncol(matrix_full_updated), "columns,",
    nrow(matrix_full_updated), "rows\n")

