###############################################################################
# ANALYSIS 1 — PCA (Principal Component Analysis)
# Question: How do Capra ibex and Vaccinium myrtillus separate
#           in multidimensional environmental space?
# Variables: elevation, slope, aspect, tmax, precipitation,
#            NDVI_mean, NDVI_range
###############################################################################

# =============================================================================
# QUESTION ÉCOLOGIQUE & JUSTIFICATION DE LA PCA
# =============================================================================
# Ce fichier est pour analyser les conditions environnementales qui joue sur la co-distribution
# de Capra ibex et Vaccinium myrtillus dans le canton du Valais, en Suisse.
# La question centrale est la suivante :
# Ces deux espèces occupent-elles desvniches environnementales distinctes, et si oui, quelles variablesvdiscriminent le mieux leurs distributions ?
#
# Pour répondre à cette question, une Analyse en Composantes Principales (ACP)
# j'ai réalisée sur six variables environnementales extraites à chaque point
# d'occurrence : l'élévation, la pente, la température maximale annuelle
# moyenne (tmax), la précipitation annuelle, le NDVI moyen estival et
# l'amplitude saisonnière du NDVI. J'ai choisis ces variables car elles
# capturent les principaux axes de variation environnementale dans les
# écosystèmes alpins — le gradient altitudinal/thermique, la rugosité du
# terrain, le régime hydrique et la productivité de la végétation.
#
# L'ACP est particulièrement adaptée ici pour trois raisons : 
# 1) les variables sont corrélées entre elles (ex. élévation et température sont
# fortement négativement corrélées), ce qui rend une réduction de
# dimensionnalité nécessaire 
# (2) elle permet de visualiser simultanément les deux espèces dans le même espace environnemental
# (3) les loadings révèlent quelles variables structurent les principaux axes de variation,
# apportant un éclairage mécaniste sur la différenciation de niche.
# =============================================================================

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(dplyr)
library(ggplot2)
install.packages(c("ggfortify", "factoextra"))  # if not already installed
library(ggfortify)   # for autoplot PCA
library(factoextra) # for beautiful PCA plots
install.packages("patchwork")
library(patchwork)  # for combining multiple ggplots

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_full <- read.csv("data/matrix_full.csv")


# Dossier figures
figures_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/figures"
dir.create(figures_path, showWarnings = FALSE)

# =============================================================================
# 3) PREPARE DATA FOR PCA
# =============================================================================
# Select numeric environmental variables only
# Remove rows with NA in any of the selected variables

pca_vars <- c("elevation", "slope", "tmax_mean_c",
              "prec_mean_annual", "NDVI_mean", "NDVI_range")

# Check which variables are available
available_vars <- pca_vars[pca_vars %in% names(matrix_full)]
missing_vars   <- pca_vars[!pca_vars %in% names(matrix_full)]

if (length(missing_vars) > 0) {
  cat("Warning — missing variables:", missing_vars, "\n")
  cat("PCA will run with:", available_vars, "\n")
}

# Keep only complete cases
pca_data <- matrix_full %>%
  dplyr::select(species_short, all_of(available_vars)) %>%
  filter(complete.cases(.))

cat("Rows used for PCA:", nrow(pca_data), "\n")
cat("Variables used:", available_vars, "\n")

# =============================================================================
# 4) RUN PCA
# =============================================================================
# scale. = TRUE is essential — variables have different units (m, °C, mm...)
# Without scaling, elevation (0-4500m) would dominate over NDVI (0-1)

pca_result <- prcomp(
  pca_data %>% dplyr::select(-species_short),
  scale.  = TRUE,
  center  = TRUE
)

# Summary of variance explained
cat("\n=== PCA SUMMARY ===\n")
print(summary(pca_result))

# Variance explained by each component
var_explained <- round(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 1)
cat("\nVariance explained per PC:\n")
for (i in seq_along(var_explained)) {
  cat(sprintf("  PC%d: %.1f%%\n", i, var_explained[i]))
}

# =============================================================================
# FIGURE 1 — Scree plot (variance explained per component)
# =============================================================================

scree_df <- data.frame(
  PC       = paste0("PC", 1:length(var_explained)),
  variance = var_explained
)

p_scree <- ggplot(scree_df, aes(x = PC, y = variance)) +
  geom_bar(stat = "identity", fill = "#2E75B6", alpha = 0.8) +
  geom_line(aes(group = 1), color = "grey30", linewidth = 0.8) +
  geom_point(size = 3, color = "grey30") +
  geom_text(aes(label = paste0(variance, "%")),
            vjust = -0.5, size = 3.5) +
  labs(
    title    = "PCA — Scree plot",
    subtitle = "Variance explained by each principal component",
    x        = "Principal Component",
    y        = "Variance explained (%)"
  ) +
  theme_classic(base_size = 12)

print(p_scree)
ggsave(file.path(figures_path, "fig_pca_scree.png"),
       p_scree, width = 8, height = 5, dpi = 300)

# =============================================================================
# FIGURE 2 — Biplot PC1 vs PC2 (scores + loadings)
# =============================================================================

# Extract scores (individual points)
scores_df <- data.frame(
  pca_result$x,
  species_short = pca_data$species_short
)

# Extract loadings (variable arrows)
loadings_df <- data.frame(
  variable = rownames(pca_result$rotation),
  PC1      = pca_result$rotation[, 1],
  PC2      = pca_result$rotation[, 2]
)

# Scale arrows for visibility
arrow_scale <- max(abs(scores_df[, c("PC1", "PC2")])) * 0.6

p_biplot <- ggplot() +
  # Individual points
  geom_point(
    data  = scores_df,
    aes(x = PC1, y = PC2, color = species_short),
    size  = 1.5, alpha = 0.5
  ) +
  # Ellipses de confiance 95%
  stat_ellipse(
    data  = scores_df,
    aes(x = PC1, y = PC2, color = species_short, fill = species_short),
    geom  = "polygon", alpha = 0.1, level = 0.95
  ) +
  # Variable arrows
  geom_segment(
    data = loadings_df,
    aes(x = 0, y = 0,
        xend = PC1 * arrow_scale,
        yend = PC2 * arrow_scale),
    arrow     = arrow(length = unit(0.2, "cm")),
    color     = "grey30", linewidth = 0.8
  ) +
  # Variable labels
  geom_text(
    data  = loadings_df,
    aes(x = PC1 * arrow_scale * 1.1,
        y = PC2 * arrow_scale * 1.1,
        label = variable),
    size  = 3.5, color = "grey20", fontface = "bold"
  ) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  labs(
    title    = "Environmental niche of Capra ibex and Vaccinium myrtillus",
    subtitle = sprintf("PC1: %.1f%% | PC2: %.1f%% of variance explained",
                       var_explained[1], var_explained[2]),
    x        = sprintf("PC1 (%.1f%%)", var_explained[1]),
    y        = sprintf("PC2 (%.1f%%)", var_explained[2]),
    color    = "Species", fill = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_biplot)
ggsave(file.path(figures_path, "fig_pca_biplot.png"),
       p_biplot, width = 10, height = 8, dpi = 300)

# =============================================================================
# FIGURE 3 — Variable contributions to PC1 and PC2
# =============================================================================

contrib_df <- data.frame(
  variable = rownames(pca_result$rotation),
  PC1      = abs(pca_result$rotation[, 1]),
  PC2      = abs(pca_result$rotation[, 2])
) %>%
  tidyr::pivot_longer(cols = c(PC1, PC2),
                      names_to  = "component",
                      values_to = "contribution")

p_contrib <- ggplot(contrib_df,
  aes(x = reorder(variable, contribution),
      y = contribution, fill = component)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("PC1" = "#2E75B6", "PC2" = "#ED7D31")) +
  coord_flip() +
  labs(
    title    = "Variable contributions to PC1 and PC2",
    subtitle = "Absolute loadings — higher = more influential",
    x        = "Variable",
    y        = "Absolute loading",
    fill     = "Component"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

print(p_contrib)
ggsave(file.path(figures_path, "fig_pca_contributions.png"),
       p_contrib, width = 9, height = 6, dpi = 300)

# =============================================================================
# INTERPRETATION (as comments)
# =============================================================================
# PC1 typically captures the elevation/temperature gradient:
# high elevation = low temperature = high NDVI_range (seasonal variation)
# PC2 typically captures moisture/precipitation gradient
#
# If ibex and bilberry ellipses overlap strongly on the biplot:
# -> their environmental niches are largely similar (confirmed by BC = 0.943)
#
# If they separate along PC1:
# -> ibex occupies higher/colder environments than bilberry
# -> consistent with logistic regression results (elevation p=0.004)
 
cat("\n=== INTERPRETATION ===\n")
cat("PC1 explains", var_explained[1], "% of variance\n")
cat("PC2 explains", var_explained[2], "% of variance\n")
cat("Combined PC1+PC2:", var_explained[1] + var_explained[2], "%\n")
 
# Test if PC1 scores differ significantly between species
wilcox_pc1 <- wilcox.test(PC1 ~ species_short, data = scores_df)
wilcox_pc2 <- wilcox.test(PC2 ~ species_short, data = scores_df)
 
cat("\nWilcoxon test PC1 scores (Ibex vs Bilberry):",
    ifelse(wilcox_pc1$p.value < 0.05,
           paste("SIGNIFICANT (p =", round(wilcox_pc1$p.value, 4), ")"),
           paste("not significant (p =", round(wilcox_pc1$p.value, 4), ")")), "\n")
 
cat("Wilcoxon test PC2 scores (Ibex vs Bilberry):",
    ifelse(wilcox_pc2$p.value < 0.05,
           paste("SIGNIFICANT (p =", round(wilcox_pc2$p.value, 4), ")"),
           paste("not significant (p =", round(wilcox_pc2$p.value, 4), ")")), "\n")
 
# =============================================================================
# FIGURE 4 — PANEL: 3 figures + résumé des résultats
# =============================================================================
 
library(patchwork)
library(grid)
library(gridExtra)
 
# Texte du résumé
summary_text <- paste(
  "Résultats clés de l'ACP",
  "",
  "A. Scree plot : PC1 explique 54.5% de la variance et PC2 17.4%, soit 71.9% au total.",
  "   L'essentiel de la variation environnementale est capturée en deux dimensions.",
  "",
  "B. Biplot : Les ellipses de confiance à 95% se chevauchent fortement, confirmant que",
  "   Capra ibex et Vaccinium myrtillus occupent des niches environnementales largement",
  "   similaires (BC = 0.943). L'ellipse ibex est plus large, indiquant une plus grande",
  "   plasticité environnementale. La pente pointe fortement vers PC2 négatif, révélant",
  "   que le bouquetin fréquente des zones plus escarpées que la myrtille.",
  "",
  "C. Contributions : PC1 est dominé par l'élévation et tmax (gradient altitudinal/",
  "   thermique). PC2 est dominé par la pente (loading = 0.85), variable la plus",
  "   discriminante entre les deux espèces sur le deuxième axe.",
  sep = "\n"
)
 
# Créer le bloc de texte comme une figure ggplot
p_summary <- ggplot() +
  annotate("text",
           x = 0, y = 1,
           label    = summary_text,
           hjust    = 0, vjust = 1,
           size     = 3.8,
           family   = "mono",
           color    = "#333333",
           lineheight = 1.4) +
  xlim(0, 1) + ylim(0, 1) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "#F5F8FC", color = "#CCCCCC", linewidth = 0.8),
    plot.margin      = ggplot2::margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  )
 
# Assembler les 3 figures + résumé avec patchwork
panel <- (p_scree | p_biplot | p_contrib) /
  p_summary +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
    title    = "Analyse en Composantes Principales (ACP)",
    subtitle = "Capra ibex vs Vaccinium myrtillus — Canton du Valais",
    caption  = "Variables : élévation, pente, tmax, précipitation annuelle, NDVI moyen, amplitude NDVI",
    theme    = theme(
      plot.title    = element_text(size = 16, face = "bold", color = "#1F4E79"),
      plot.subtitle = element_text(size = 12, color = "#444444"),
      plot.caption  = element_text(size = 9,  color = "#888888", hjust = 0)
    )
  )
 
print(panel)
ggsave(file.path(figures_path, "fig_pca_panel.png"),
       panel, width = 16, height = 12, dpi = 300)
 
cat("Panel saved to fig_pca_panel.png\n")
