###############################################################################
# ANALYSIS 2 — ENVIRONMENTAL COMPARISON
# Question: Which environmental variables differ most significantly between
#           Capra ibex and Vaccinium myrtillus in the Valais?
# Methods: Boxplots, density plots, correlation analysis, radial plot
###############################################################################

# =============================================================================
# QUESTION ÉCOLOGIQUE & JUSTIFICATION
# =============================================================================
# Après avoir visualisé la séparation des niches dans l'espace des composantes
# principales (ACP), cette analyse compare directement chaque variable
# environnementale entre les deux espèces. L'objectif est d'identifier quelles
# variables discriminent le mieux Capra ibex et Vaccinium myrtillus, et de
# quantifier les différences à l'aide de tests statistiques non-paramétriques
# (Wilcoxon). Un graphique radial (radar chart) permet de synthétiser
# visuellement les profils environnementaux moyens des deux espèces.
# =============================================================================

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(scales)


install.packages(c("fmsb"), dependencies = TRUE)
library(fmsb)

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_full <- read.csv("data/matrix_full.csv")

cat("Matrix loaded —", nrow(matrix_full), "rows,", ncol(matrix_full), "cols\n")

figures_path <- "data/figures"
dir.create(figures_path, showWarnings = FALSE)

# Variables environnementales à comparer
env_vars <- c("elevation", "slope", "tmax_mean_c",
              "prec_mean_annual", "NDVI_mean", "NDVI_range")

# Garder uniquement les colonnes disponibles
env_vars <- env_vars[env_vars %in% names(matrix_full)]

# Données complètes uniquement
data_clean <- matrix_full %>%
  dplyr::select(species_short, all_of(env_vars)) %>%
  filter(complete.cases(.))

cat("Rows used:", nrow(data_clean), "\n")

# =============================================================================
# 3) WILCOXON TESTS — toutes les variables
# =============================================================================

cat("\n=== WILCOXON TESTS: Ibex vs Bilberry ===\n")

wilcox_results <- lapply(env_vars, function(var) {
  test <- wilcox.test(
    as.formula(paste(var, "~ species_short")),
    data = data_clean
  )
  data.frame(
    variable = var,
    W        = round(test$statistic, 0),
    p_value  = round(test$p.value, 4),
    sig      = ifelse(test$p.value < 0.001, "***",
               ifelse(test$p.value < 0.01,  "**",
               ifelse(test$p.value < 0.05,  "*", "ns")))
  )
})

wilcox_df <- bind_rows(wilcox_results)
print(wilcox_df)

# =============================================================================
# 4) DESCRIPTIVE STATISTICS
# =============================================================================

stats_df <- data_clean %>%
  group_by(species_short) %>%
  summarise(across(all_of(env_vars), list(
    mean   = ~round(mean(., na.rm = TRUE), 2),
    median = ~round(median(., na.rm = TRUE), 2),
    sd     = ~round(sd(., na.rm = TRUE), 2)
  )))

cat("\n=== DESCRIPTIVE STATISTICS ===\n")
print(stats_df)

# =============================================================================
# FIGURE 1 — Boxplots for all variables
# =============================================================================

# Labels plus lisibles pour les variables
var_labels <- c(
  elevation        = "Elevation (m)",
  slope            = "Slope (°)",
  tmax_mean_c      = "Mean Tmax (°C)",
  prec_mean_annual = "Precipitation (mm)",
  NDVI_mean        = "Mean NDVI",
  NDVI_range       = "NDVI Range"
)

# Créer un boxplot par variable
boxplots <- lapply(env_vars, function(var) {

  # Récupérer la significativité
  sig_label <- wilcox_df$sig[wilcox_df$variable == var]
  y_max      <- max(data_clean[[var]], na.rm = TRUE)
  y_range    <- diff(range(data_clean[[var]], na.rm = TRUE))

  ggplot(data_clean,
    aes_string(x = "species_short", y = var,
               fill = "species_short", color = "species_short")) +
    geom_boxplot(alpha = 0.4, outlier.size = 0.5, outlier.alpha = 0.3) +
    geom_jitter(width = 0.15, size = 0.4, alpha = 0.2) +
    annotate("text",
             x = 1.5, y = y_max + y_range * 0.05,
             label = sig_label, size = 5, fontface = "bold", color = "#333333") +
    scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
    scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
    labs(x = NULL, y = var_labels[var]) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text.x     = element_text(size = 9))
})

# Assembler tous les boxplots
p_boxplots <- wrap_plots(boxplots, ncol = 3) +
  plot_annotation(
    title    = "Environmental variable comparison — Ibex vs Bilberry",
    subtitle = "Wilcoxon test significance: *** p<0.001 | ** p<0.01 | * p<0.05 | ns",
    theme    = theme(
      plot.title    = element_text(size = 14, face = "bold", color = "#1F4E79"),
      plot.subtitle = element_text(size = 10, color = "#666666")
    )
  )

print(p_boxplots)
ggsave(file.path(figures_path, "fig_comparison_boxplots.png"),
       p_boxplots, width = 12, height = 8, dpi = 300)

# =============================================================================
# FIGURE 2 — Correlation heatmap between environmental variables
# =============================================================================

# Matrice de corrélation
cor_matrix <- cor(data_clean %>% dplyr::select(all_of(env_vars)),
                  use = "complete.obs", method = "pearson")

cor_df <- as.data.frame(cor_matrix) %>%
  tibble::rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

p_cor <- ggplot(cor_df, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(correlation, 2)),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_gradient2(
    low      = "#2166AC",
    mid      = "white",
    high     = "#D7191C",
    midpoint = 0,
    limits   = c(-1, 1),
    name     = "Pearson r"
  ) +
  scale_x_discrete(labels = var_labels) +
  scale_y_discrete(labels = var_labels) +
  labs(
    title    = "Correlation matrix of environmental variables",
    subtitle = "Pearson correlation coefficient",
    x = NULL, y = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_cor)
ggsave(file.path(figures_path, "fig_comparison_correlation.png"),
       p_cor, width = 8, height = 7, dpi = 300)

# =============================================================================
# FIGURE 3 — Radial plot (radar chart) — profils environnementaux moyens
# =============================================================================
# Normaliser les variables entre 0 et 1 pour les comparer sur le même axe

normalize <- function(x) (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))

radar_data <- data_clean %>%
  mutate(across(all_of(env_vars), normalize)) %>%
  group_by(species_short) %>%
  summarise(across(all_of(env_vars), mean, na.rm = TRUE))

# Format requis par fmsb: max row, min row, data rows
radar_matrix <- rbind(
  rep(1, length(env_vars)),  # max
  rep(0, length(env_vars)),  # min
  radar_data %>% dplyr::select(-species_short)
)
colnames(radar_matrix) <- names(var_labels)[names(var_labels) %in% env_vars]

# Plot radar
png(file.path(figures_path, "fig_comparison_radar.png"),
    width = 800, height = 700, res = 150)

par(mar = c(2, 2, 3, 2))
radarchart(
  as.data.frame(radar_matrix),
  axistype  = 1,
  pcol      = c("#D95F02", "#1B9E77"),
  pfcol     = c(adjustcolor("#D95F02", 0.25), adjustcolor("#1B9E77", 0.25)),
  plwd      = 2.5,
  cglcol    = "grey80",
  cglty     = 1,
  axislabcol = "grey40",
  vlcex     = 0.85,
  title     = "Environmental profiles — Ibex vs Bilberry (normalized)",
  cex.main  = 1.1
)
legend("topright",
       legend = c("Capra ibex", "Vaccinium myrtillus"),
       col    = c("#D95F02", "#1B9E77"),
       lwd = 2, bty = "n", cex = 0.9)

dev.off()
cat("Radar chart saved\n")

# =============================================================================
# FIGURE 4 — PANEL FINAL avec résumé
# =============================================================================

# Résumé texte
summary_text <- paste(
  "Résultats clés de la comparaison environnementale",
  "",
  paste("Tests de Wilcoxon —",
        paste(apply(wilcox_df, 1, function(r)
          paste0(var_labels[r["variable"]], ": ", r["sig"])), collapse = " | ")),
  "",
  "Élévation & Pente : Le bouquetin occupe des altitudes significativement plus élevées",
  "   et des terrains plus escarpés que la myrtille, cohérent avec son habitat rocheux.",
  "",
  "Température & Précipitation : Capra ibex est associé à des zones plus froides et plus",
  "   sèches, correspondant aux flancs sud-exposés continentaux du Valais.",
  "",
  "NDVI : La myrtille présente un NDVI moyen plus élevé, reflétant sa présence dans des",
  "   zones de végétation dense, tandis que le bouquetin fréquente aussi des zones rocheuses.",
  "",
  "Corrélations : Élévation et tmax sont fortement négativement corrélées (gradient altitudinal).",
  "   NDVI_mean et prec_mean_annual sont positivement corrélées (humidité → végétation).",
  sep = "\n"
)

p_text <- ggplot() +
  annotate("text",
           x = 0, y = 1,
           label     = summary_text,
           hjust     = 0, vjust = 1,
           size      = 3.5,
           family    = "mono",
           color     = "#333333",
           lineheight = 1.4) +
  xlim(0, 1) + ylim(0, 1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F5F8FC",
                                   color = "#CCCCCC", linewidth = 0.8),
    plot.margin     = ggplot2::margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  )

# Panel: boxplots + correlation + text
panel <- (p_boxplots | p_cor) /
  p_text +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
    title    = "Comparaison environnementale — Capra ibex vs Vaccinium myrtillus",
    subtitle = "Canton du Valais | Tests de Wilcoxon | Corrélation de Pearson | Radar chart",
    caption  = "*** p<0.001 | ** p<0.01 | * p<0.05 | ns = non significatif",
    theme    = theme(
      plot.title    = element_text(size = 16, face = "bold", color = "#1F4E79"),
      plot.subtitle = element_text(size = 11, color = "#444444"),
      plot.caption  = element_text(size = 9,  color = "#888888", hjust = 0)
    )
  )

print(panel)
ggsave(file.path(figures_path, "fig_comparison_panel.png"),
       panel, width = 18, height = 14, dpi = 300)

cat("Panel saved to fig_comparison_panel.png\n")