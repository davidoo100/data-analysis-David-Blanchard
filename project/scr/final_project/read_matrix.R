###############################################################################
# STEP 1 — READ MATRIX & ECOLOGICAL QUESTION
# Applied Biological Data Analysis — June 2026
# Author: David Blanchard
###############################################################################

# =============================================================================
# ECOLOGICAL QUESTION
# =============================================================================
# This project investigates the environmental drivers of the co-distribution
# of the Alpine ibex (Capra ibex) and the common bilberry (Vaccinium myrtillus)
# in the canton of Valais, Switzerland.
#
# Central question:
# "Which environmental variables best explain the spatial co-distribution
#  of Capra ibex and Vaccinium myrtillus in the Valais, and how will
#  projected climate change affect this plant-herbivore relationship?"
#
# Sub-questions:
# 1. Do the two species occupy distinct environmental niches, and if so,
#    along which axes (elevation, temperature, NDVI, precipitation, slope)?
# 2. Which variables best discriminate the two species (Random Forest)?
# 3. How much warming is projected at their occurrence sites by 2050
#    under optimistic (SSP126) and pessimistic (SSP585) scenarios?
#
# Biological context:
# Capra ibex is a specialist alpine ungulate whose diet includes V. myrtillus
# during summer. The bilberry is projected to lose up to 47% of its European
# climatic niche by 2080 (Puchałka et al. 2023), while ibex are already
# shifting upward by ~135m/decade in the Swiss Alps (Büntgen et al. 2017).
# This raises the possibility of a future trophic mismatch between consumer
# and food plant, motivating the present spatial analysis.
# =============================================================================

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(dplyr)
library(ggplot2)
library(knitr)

# =============================================================================
# 2) SET WORKING DIRECTORY
# =============================================================================

setwd("/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project")

# =============================================================================
# 3) LOAD MATRIX
# =============================================================================

matrix_full <- read.csv("data/matrix_full.csv")

cat("=== MATRIX LOADED ===\n")
cat("Rows:   ", nrow(matrix_full), "\n")
cat("Columns:", ncol(matrix_full), "\n")
cat("\nSpecies distribution:\n")
print(table(matrix_full$species_short))

# =============================================================================
# 4) CHECK AVAILABLE VARIABLES
# =============================================================================

cat("\n=== AVAILABLE COLUMNS ===\n")
print(names(matrix_full))

# Check for key environmental variables
key_vars <- c("elevation", "slope", "tmax_mean_c", "prec_mean_annual",
              "NDVI_mean", "NDVI_range", "delta_ssp126", "delta_ssp585",
              "Landcover", "Climate_Re")

cat("\n=== KEY VARIABLES CHECK ===\n")
for (var in key_vars) {
  status <- ifelse(var %in% names(matrix_full), "✓ present", "✗ missing")
  cat(sprintf("  %-25s %s\n", var, status))
}