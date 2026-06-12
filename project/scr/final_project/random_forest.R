###############################################################################
# ANALYSIS 3 — RANDOM FOREST
# Question: Which environmental variables best discriminate
#           Capra ibex from Vaccinium myrtillus?
# Method: Random Forest classification + feature importance plot
###############################################################################

# =============================================================================
# QUESTION ÉCOLOGIQUE & JUSTIFICATION
# =============================================================================
# L'ACP et la comparaison environnementale ont montré que plusieurs variables
# diffèrent significativement entre les deux espèces. Cependant, ces analyses
# univariées ne tiennent pas compte des interactions entre variables.
# Le Random Forest est une méthode d'apprentissage automatique non-paramétrique
# qui modélise la relation entre toutes les variables simultanément pour prédire
# l'appartenance à une espèce. L'importance des variables (feature importance)
# révèle quelles variables sont les plus informatives pour discriminer les deux
# espèces, en tenant compte des corrélations et interactions entre elles.
# Contrairement à la régression logistique, le Random Forest ne fait aucune
# hypothèse sur la distribution des données et gère bien la multicolinéarité.
# =============================================================================

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
library(randomForest)
install.packages("caret")  
library(caret)         

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_full <- read.csv("data/matrix_full.csv")

cat("Matrix loaded —", nrow(matrix_full), "rows,", ncol(matrix_full), "cols\n")

figures_path <- "data/figures"
dir.create(figures_path, showWarnings = FALSE)

# =============================================================================
# 3) PREPARE DATA
# =============================================================================

env_vars <- c("elevation", "slope", "tmax_mean_c",
              "prec_mean_annual", "NDVI_mean", "NDVI_range")

env_vars <- env_vars[env_vars %in% names(matrix_full)]

rf_data <- matrix_full %>%
  dplyr::select(species_short, all_of(env_vars)) %>%
  filter(complete.cases(.)) %>%
  mutate(species_short = as.factor(species_short))

cat("Rows used for Random Forest:", nrow(rf_data), "\n")
cat("Class balance:\n")
print(table(rf_data$species_short))

# =============================================================================
# 4) TRAIN/TEST SPLIT (70/30)
# =============================================================================

set.seed(42)
train_idx   <- createDataPartition(rf_data$species_short, p = 0.7, list = FALSE)
train_data  <- rf_data[train_idx, ]
test_data   <- rf_data[-train_idx, ]

cat("\nTraining set:", nrow(train_data), "rows\n")
cat("Test set:    ", nrow(test_data),  "rows\n")

# =============================================================================
# 5) TRAIN RANDOM FOREST
# =============================================================================

set.seed(42)
rf_model <- randomForest(
  species_short ~ .,
  data       = train_data,
  ntree      = 500,      # 500 trees
  mtry       = 2,        # variables tried at each split
  importance = TRUE,     # compute variable importance
  keep.forest = TRUE
)

cat("\n=== RANDOM FOREST MODEL ===\n")
print(rf_model)

# =============================================================================
# 6) MODEL EVALUATION ON TEST SET
# =============================================================================

predictions  <- predict(rf_model, test_data)
conf_matrix  <- confusionMatrix(predictions, test_data$species_short)

cat("\n=== CONFUSION MATRIX ===\n")
print(conf_matrix)

accuracy    <- round(conf_matrix$overall["Accuracy"] * 100, 1)
kappa       <- round(conf_matrix$overall["Kappa"], 3)

cat(sprintf("\nAccuracy: %.1f%%\n", accuracy))
cat(sprintf("Kappa:    %.3f\n", kappa))

# =============================================================================
# 7) VARIABLE IMPORTANCE
# =============================================================================

importance_df <- as.data.frame(importance(rf_model)) %>%
  tibble::rownames_to_column("variable") %>%
  arrange(desc(MeanDecreaseAccuracy))

cat("\n=== VARIABLE IMPORTANCE ===\n")
print(importance_df)

# Labels lisibles
var_labels <- c(
  elevation        = "Elevation (m)",
  slope            = "Slope (°)",
  tmax_mean_c      = "Mean Tmax (°C)",
  prec_mean_annual = "Precipitation (mm)",
  NDVI_mean        = "Mean NDVI",
  NDVI_range       = "NDVI Range"
)

importance_df$label <- var_labels[importance_df$variable]

# =============================================================================
# FIGURE 1 — Feature importance: MeanDecreaseAccuracy
# =============================================================================

p_importance_acc <- ggplot(
  importance_df,
  aes(x = reorder(label, MeanDecreaseAccuracy),
      y = MeanDecreaseAccuracy,
      fill = MeanDecreaseAccuracy)
) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = round(MeanDecreaseAccuracy, 1)),
            hjust = -0.2, size = 3.5, color = "#333333") +
  scale_fill_gradient(low = "#AED6F1", high = "#1F4E79") +
  coord_flip() +
  labs(
    title    = "Variable importance — Mean Decrease in Accuracy",
    subtitle = "Higher = more important for classification",
    x        = NULL,
    y        = "Mean Decrease Accuracy",
    fill     = "Importance"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none") +
  expand_limits(y = max(importance_df$MeanDecreaseAccuracy) * 1.15)

print(p_importance_acc)
ggsave(file.path(figures_path, "fig_rf_importance_accuracy.png"),
       p_importance_acc, width = 9, height = 6, dpi = 300)

# =============================================================================
# FIGURE 2 — Feature importance: MeanDecreaseGini
# =============================================================================

p_importance_gini <- ggplot(
  importance_df %>% arrange(desc(MeanDecreaseGini)),
  aes(x = reorder(label, MeanDecreaseGini),
      y = MeanDecreaseGini,
      fill = MeanDecreaseGini)
) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = round(MeanDecreaseGini, 1)),
            hjust = -0.2, size = 3.5, color = "#333333") +
  scale_fill_gradient(low = "#A9DFBF", high = "#1E8449") +
  coord_flip() +
  labs(
    title    = "Variable importance — Mean Decrease in Gini",
    subtitle = "Node purity: higher = better split between species",
    x        = NULL,
    y        = "Mean Decrease Gini",
    fill     = "Importance"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none") +
  expand_limits(y = max(importance_df$MeanDecreaseGini) * 1.15)

print(p_importance_gini)
ggsave(file.path(figures_path, "fig_rf_importance_gini.png"),
       p_importance_gini, width = 9, height = 6, dpi = 300)

# =============================================================================
# FIGURE 3 — Confusion matrix visualization
# =============================================================================

cm_df <- as.data.frame(conf_matrix$table) %>%
  rename(Predicted = Prediction, Actual = Reference)

p_cm <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Freq), size = 8, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#AED6F1", high = "#1F4E79", name = "Count") +
  labs(
    title    = "Confusion Matrix — Random Forest",
    subtitle = sprintf("Accuracy: %.1f%% | Kappa: %.3f", accuracy, kappa),
    x        = "Actual species",
    y        = "Predicted species"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "right")

print(p_cm)
ggsave(file.path(figures_path, "fig_rf_confusion_matrix.png"),
       p_cm, width = 7, height = 6, dpi = 300)

# =============================================================================
# FIGURE 4 — Distribution des top 2 variables par espèce
# =============================================================================

top_vars <- importance_df$variable[1:2]

pd_plots <- lapply(top_vars, function(var) {
  ggplot(rf_data, aes_string(x = var, fill = "species_short",
                              color = "species_short")) +
    geom_density(alpha = 0.4, linewidth = 1) +
    scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
    scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
    labs(
      title    = paste("Distribution —", var_labels[var]),
      subtitle = "Top discriminating variable (Random Forest)",
      x        = var_labels[var],
      y        = "Density",
      fill     = "Species", color = "Species"
    ) +
    theme_classic(base_size = 11) +
    theme(legend.position = "bottom")
})

p_partial <- wrap_plots(pd_plots, ncol = 2) +
  plot_annotation(title = "Distribution of top 2 discriminating variables")

print(p_partial)
ggsave(file.path(figures_path, "fig_rf_top_variables.png"),
       p_partial, width = 12, height = 5, dpi = 300)

# =============================================================================
# FIGURE 5 — PANEL FINAL avec résumé
# =============================================================================

top1 <- importance_df$label[1]
top2 <- importance_df$label[2]
top3 <- importance_df$label[3]

summary_text <- paste(
  "Résultats clés du Random Forest",
  "",
  sprintf("Performance du modèle : Accuracy = %.1f%% | Kappa = %.3f", accuracy, kappa),
  sprintf("Le modèle discrimine correctement les deux espèces dans %.1f%% des cas.", accuracy),
  "",
  sprintf("Variables les plus discriminantes (MeanDecreaseAccuracy) :"),
  sprintf("   1. %s  —  variable la plus informative pour séparer ibex et myrtille", top1),
  sprintf("   2. %s", top2),
  sprintf("   3. %s", top3),
  "",
  "Interprétation : Contrairement à la régression logistique univariée, le Random Forest",
  "   tient compte des interactions entre variables. Ce résultat confirme que la niche",
  "   environnementale de Capra ibex est avant tout structurée par le gradient altitudinal/",
  "   thermique et la rugosité du terrain, indépendamment de la présence de Vaccinium myrtillus.",
  sep = "\n"
)

p_text <- ggplot() +
  annotate("text",
           x = 0, y = 1,
           label      = summary_text,
           hjust      = 0, vjust = 1,
           size       = 3.5,
           family     = "mono",
           color      = "#333333",
           lineheight = 1.4) +
  xlim(0, 1) + ylim(0, 1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F5F8FC",
                                   color = "#CCCCCC", linewidth = 0.8),
    plot.margin     = ggplot2::margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  )

panel <- (p_importance_acc | p_importance_gini | p_cm) /
  p_text +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
    title    = "Random Forest — Discrimination de Capra ibex et Vaccinium myrtillus",
    subtitle = sprintf("500 arbres | Accuracy: %.1f%% | Variables: %s",
                       accuracy, paste(env_vars, collapse = ", ")),
    caption  = "Importance mesurée par MeanDecreaseAccuracy et MeanDecreaseGini",
    theme    = theme(
      plot.title    = element_text(size = 16, face = "bold", color = "#1F4E79"),
      plot.subtitle = element_text(size = 11, color = "#444444"),
      plot.caption  = element_text(size = 9,  color = "#888888", hjust = 0)
    )
  )

print(panel)
ggsave(file.path(figures_path, "fig_rf_panel.png"),
       panel, width = 18, height = 12, dpi = 300)

cat("Panel saved to fig_rf_panel.png\n")
cat("\n=== RANDOM FOREST COMPLETE ===\n")


