###############################################################################
# MY PROJECT — Applied Biological Data Analysis — June 2026
# Author: David Blanchard
# Université de Neuchâtel
#
# ECOLOGICAL QUESTION:
# Which environmental variables best explain the co-distribution of
# Capra ibex and Vaccinium myrtillus in the Valais, and how will
# climate change affect this plant-herbivore relationship?
###############################################################################

# =============================================================================
# SET WORKING DIRECTORY
# =============================================================================
# All paths in the sourced scripts are relative to this directory

setwd("/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project")
# =============================================================================
# STEP 1 — Load the final environmental matrix + ecological question
# =============================================================================
# Loads matrix_full.csv, checks variables, prints summary statistics
# matrix_full is made available for all subsequent scripts

cat("\n========================================\n")
cat("STEP 1 — Loading matrix & question\n")
cat("========================================\n")
source("scr/final_project/read_matrix.R")

# =============================================================================
# STEP 2 — PCA: Environmental niche comparison
# =============================================================================
# Performs PCA on 6 environmental variables
# Produces: scree plot, biplot with 95% ellipses, variable contributions
# Key result: PC1 (54.5%) = altitudinal/thermal gradient | PC2 (17.4%) = slope

cat("\n========================================\n")
cat("STEP 2 — PCA analysis\n")
cat("========================================\n")
source("scr/final_project/analysis_PCA.R")

# =============================================================================
# STEP 3 — Environmental comparison: variable-by-variable analysis
# =============================================================================
# Wilcoxon tests for all variables, correlation heatmap, radar chart
# Key result: all 6 variables differ significantly between species

cat("\n========================================\n")
cat("STEP 3 — Environmental comparison\n")
cat("========================================\n")
source("scr/final_project/environmental_comparison.R")

# =============================================================================
# STEP 4 — Random Forest: discriminating variable analysis
# =============================================================================
# Trains a Random Forest (500 trees) to classify species from environment
# Produces: feature importance plots, confusion matrix
# Key result: Accuracy = 88.2% | Top variables: NDVI mean, slope, elevation

cat("\n========================================\n")
cat("STEP 4 — Random Forest\n")
cat("========================================\n")
source("scr/final_project/random_forest.R")


# =============================================================================
# STEP 5 — Final summary panel
# =============================================================================
# Synthesizes all results in a single publication-ready panel figure
# Includes: map, boxplot, density, RF importance, climate change

cat("\n========================================\n")
cat("STEP 6 — Summary panel\n")
cat("========================================\n")
source("scr/final_project/summary_panel.R")

# =============================================================================
# END
# =============================================================================

cat("\n========================================\n")
cat("PROJECT COMPLETE\n")
cat("All figures saved in: data/figures/\n")
cat("========================================\n")
cat("\nKey outputs:\n")
cat("  Static figures (.png):\n")
cat("    - fig_pca_panel.png\n")
cat("    - fig_comparison_panel.png\n")
cat("    - fig_rf_panel.png\n")
cat("    - fig_summary_panel.png\n")
cat("    - fig_ndvi_dynamics.png\n")
cat("  Interactive figures (.html):\n")
cat("    - fig_interactive_3d.html\n")
cat("    - fig_interactive_scatter.html\n")
cat("    - fig_interactive_boxplot.html\n")
cat("    - fig_interactive_climate.html\n")
