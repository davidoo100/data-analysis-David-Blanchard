###############################################################################
# ANALYSIS 4 — FINAL SUMMARY PANEL
# Question: What are the key environmental drivers of the co-distribution
#           of Capra ibex and Vaccinium myrtillus in the Valais?
# This panel synthesizes all analyses into a single publication-ready figure
# with at least 3 different types of graphics as required by the project.
###############################################################################

# =============================================================================
# JUSTIFICATION
# =============================================================================
# Ce panneau de synthèse rassemble les résultats clés des analyses précédentes
# (ACP, comparaison environnementale, Random Forest) en une seule figure
# cohérente. Il répond directement à la question écologique centrale du projet :
# quelles variables environnementales structurent la co-distribution de
# Capra ibex et Vaccinium myrtillus dans le Valais, et comment le changement
# climatique pourrait-il affecter cette relation ?
# Le panneau combine 5 types de graphiques différents : carte, boxplot,
# diagramme de densité, barplot d'importance et graphique climatique.
# =============================================================================

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(sf)
library(geodata)
library(randomForest)
library(scales)

sf_use_s2(FALSE)

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_full <- read.csv("data/matrix_full.csv")

cat("Matrix loaded —", nrow(matrix_full), "rows,", ncol(matrix_full), "cols\n")

figures_path <- "data/figures"
dir.create(figures_path, showWarnings = FALSE)

# Variables environnementales
env_vars <- c("elevation", "slope", "tmax_mean_c",
              "prec_mean_annual", "NDVI_mean", "NDVI_range")
env_vars <- env_vars[env_vars %in% names(matrix_full)]

var_labels <- c(
  elevation        = "Elevation (m)",
  slope            = "Slope (°)",
  tmax_mean_c      = "Mean Tmax (°C)",
  prec_mean_annual = "Precipitation (mm)",
  NDVI_mean        = "Mean NDVI",
  NDVI_range       = "NDVI Range"
)

data_clean <- matrix_full %>%
  dplyr::select(species_short, longitude, latitude, all_of(env_vars)) %>%
  filter(complete.cases(.)) %>%
  mutate(species_short = as.factor(species_short))

# =============================================================================
# 3) LOAD VALAIS POLYGON
# =============================================================================

swiss_cantons    <- gadm(country = "CHE", level = 1, path = tempdir())
swiss_cantons_sf <- st_as_sf(swiss_cantons)
valais           <- swiss_cantons_sf[swiss_cantons_sf$NAME_1 == "Valais", ]

# =============================================================================
# PANEL COMPONENT A — MAP: Species occurrences in Valais
# =============================================================================

p_map <- ggplot() +
  geom_sf(data = valais, fill = "grey93", color = "grey60", linewidth = 0.5) +
  geom_point(
    data  = data_clean,
    aes(x = longitude, y = latitude, color = species_short),
    size  = 0.8, alpha = 0.6
  ) +
  scale_color_manual(
    values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77"),
    name   = "Species",
    labels = c("Ibex" = "Capra ibex", "Bilberry" = "V. myrtillus")
  ) +
  coord_sf(xlim = c(6.77, 8.48), ylim = c(45.86, 46.66)) +
  labs(
    title = "A. Species occurrences",
    x = NULL, y = NULL
  ) +
 theme_classic(base_size = 10) +
theme(
  legend.position = "bottom",
  legend.text     = element_text(size = 8, face = "italic"),
  plot.title      = element_text(face = "bold", size = 11, color = "#1F4E79"),
  axis.text       = element_text(size = 7)
)

# =============================================================================
# PANEL COMPONENT B — BOXPLOT: Elevation comparison
# =============================================================================

wilcox_elev <- wilcox.test(elevation ~ species_short, data = data_clean)
sig_elev    <- ifelse(wilcox_elev$p.value < 0.001, "***",
               ifelse(wilcox_elev$p.value < 0.01, "**", "*"))

p_boxplot <- ggplot(data_clean,
  aes(x = species_short, y = elevation,
      fill = species_short, color = species_short)) +
  geom_boxplot(alpha = 0.4, outlier.size = 0.4, outlier.alpha = 0.2) +
  geom_jitter(width = 0.1, size = 0.3, alpha = 0.15) +
  annotate("text", x = 1.5,
           y = max(data_clean$elevation, na.rm = TRUE) * 1.02,
           label = paste("Wilcoxon:", sig_elev),
           size = 3.5, color = "#333333", fontface = "italic") +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_x_discrete(labels = c("Ibex" = "C. ibex", "Bilberry" = "V. myrtillus")) +
  labs(title = "B. Elevation", x = NULL, y = "Elevation (m a.s.l.)") +
  theme_classic(base_size = 10) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 11, color = "#1F4E79"))

# =============================================================================
# PANEL COMPONENT C — DENSITY: NDVI distribution
# =============================================================================

p_ndvi <- ggplot(data_clean,
  aes(x = NDVI_mean, fill = species_short, color = species_short)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title = "C. Mean NDVI distribution",
    x     = "Mean NDVI",
    y     = "Density"
  ) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 11, color = "#1F4E79"))

# =============================================================================
# PANEL COMPONENT D — BARPLOT: Random Forest variable importance
# =============================================================================

set.seed(42)
rf_model <- randomForest(
  species_short ~ .,
  data       = data_clean %>% dplyr::select(-longitude, -latitude),
  ntree      = 500,
  importance = TRUE
)

importance_df <- as.data.frame(importance(rf_model)) %>%
  tibble::rownames_to_column("variable") %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  mutate(label = var_labels[variable])

p_importance <- ggplot(importance_df,
  aes(x = reorder(label, MeanDecreaseAccuracy),
      y = MeanDecreaseAccuracy,
      fill = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = round(MeanDecreaseAccuracy, 1)),
            hjust = -0.2, size = 2.8, color = "#333333") +
  scale_fill_gradient(low = "#AED6F1", high = "#1F4E79") +
  coord_flip() +
  labs(
    title = "D. Variable importance (RF)",
    x     = NULL,
    y     = "Mean Decrease Accuracy"
  ) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 11, color = "#1F4E79")) +
  expand_limits(y = max(importance_df$MeanDecreaseAccuracy) * 1.2)

# =============================================================================
# PANEL COMPONENT E — CLIMATE CHANGE: Delta temperature by scenario
# =============================================================================

# Vérifier si les colonnes de changement climatique sont disponibles
if (all(c("delta_ssp126", "delta_ssp585") %in% names(matrix_full))) {

  delta_df <- matrix_full %>%
    dplyr::select(species_short, delta_ssp126, delta_ssp585) %>%
    filter(complete.cases(.)) %>%
    pivot_longer(cols = c(delta_ssp126, delta_ssp585),
                 names_to  = "scenario",
                 values_to = "delta_temp") %>%
    mutate(scenario = ifelse(scenario == "delta_ssp126",
                             "SSP126\n(optimistic)",
                             "SSP585\n(pessimistic)"))

  p_climate <- ggplot(delta_df,
    aes(x = scenario, y = delta_temp,
        fill = species_short, color = species_short)) +
    geom_boxplot(alpha = 0.4, outlier.size = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
    scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
    labs(
      title = "E. Projected warming by 2050",
      x     = "Climate scenario",
      y     = "ΔT July (°C)"
    ) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 11, color = "#1F4E79"))

} else {
  # Figure de remplacement si colonnes absentes
  p_climate <- ggplot(data_clean,
    aes(x = tmax_mean_c, fill = species_short, color = species_short)) +
    geom_density(alpha = 0.4, linewidth = 1) +
    scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
    scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
    labs(title = "E. Mean Tmax distribution",
         x = "Mean Tmax (°C)", y = "Density") +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 11, color = "#1F4E79"))
}

# =============================================================================
# ASSEMBLE FINAL PANEL
# =============================================================================
# Layout:
# [ A: Map (large)        ] [ B: Boxplot ] [ C: NDVI density ]
# [ D: RF importance      ] [ E: Climate change              ]

panel_top    <- p_map | p_boxplot | p_ndvi
panel_bottom <- p_importance | p_climate

final_panel <- panel_top / panel_bottom +
  plot_layout(heights = c(1.2, 1)) +
  plot_annotation(
    title   = "Environmental drivers of Capra ibex and Vaccinium myrtillus co-distribution in Valais",
    subtitle = paste(
      "Question: Which environmental variables structure the co-distribution of C. ibex and V. myrtillus,",
      "and how will climate change affect this relationship?"
    ),
    caption = paste(
      "Data: GBIF + iNaturalist (2000-2025) | DEM: AWS Terrain Tiles (z=9) |",
      "Climate: CHELSA (2015-2020, SSP126/SSP585) | NDVI: MODIS MOD13Q1 (2025)",
      "| Random Forest: 500 trees, Accuracy = 88.2%"
    ),
    theme = theme(
      plot.title    = element_text(size = 14, face = "bold",
                                   color = "#1F4E79", hjust = 0.5),
      plot.subtitle = element_text(size = 9,  color = "#555555",
                                   hjust = 0.5, face = "italic"),
      plot.caption  = element_text(size = 7.5, color = "#888888", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

print(final_panel)
ggsave(file.path(figures_path, "fig_summary_panel.png"),
       final_panel, width = 18, height = 13, dpi = 300)

cat("\n=== SUMMARY PANEL COMPLETE ===\n")
cat("Output: fig_summary_panel.png\n")
