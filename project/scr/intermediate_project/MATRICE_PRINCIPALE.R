###############################################################################
# MATRIX INITIALIZATION — Capra ibex & Vaccinium myrtillus
# Region: Canton of Valais, Switzerland
# This script creates the base matrix with occurrence data.
# All other scripts will add columns to this matrix.
###############################################################################

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(rgbif)
library(rinat)
library(sf)
library(geodata)
library(dplyr)

sf_use_s2(FALSE)

# =============================================================================
# 2) DOWNLOAD OCCURRENCES — Capra ibex & Vaccinium myrtillus (Valais)
# =============================================================================

species_list <- c("Capra ibex", "Vaccinium myrtillus")
date_start   <- as.Date("2000-01-01")
date_end     <- as.Date("2025-12-31")

# Polygone exact du canton du Valais
swiss_cantons    <- gadm(country = "CHE", level = 1, path = tempdir())
swiss_cantons_sf <- st_as_sf(swiss_cantons)
valais           <- swiss_cantons_sf[swiss_cantons_sf$NAME_1 == "Valais", ]

all_species_data <- list()

for (sp in species_list) {

  cat("\n--- Processing:", sp, "---\n")

  # GBIF
  gbif_raw <- occ_data(
    scientificName = sp,
    hasCoordinate  = TRUE,
    country        = "CH",
    limit          = 10000
  )
  gbif_occ  <- gbif_raw$data
  data_gbif <- data.frame(
    species   = gbif_occ$species,
    latitude  = gbif_occ$decimalLatitude,
    longitude = gbif_occ$decimalLongitude,
    date_obs  = as.Date(gbif_occ$eventDate),
    source    = "GBIF"
  )

  # iNaturalist
  inat_raw <- tryCatch({
    get_inat_obs(query = sp, place_id = "switzerland", maxresults = 1000)
  }, error = function(e) { cat("  iNaturalist unavailable\n"); NULL })

  if (!is.null(inat_raw) && nrow(inat_raw) > 0) {
    data_inat <- data.frame(
      species   = inat_raw$scientific_name,
      latitude  = as.numeric(inat_raw$latitude),
      longitude = as.numeric(inat_raw$longitude),
      date_obs  = as.Date(inat_raw$observed_on),
      source    = "iNaturalist"
    )
  } else {
    data_inat <- data.frame(
      species   = character(0), latitude  = numeric(0),
      longitude = numeric(0),   date_obs  = as.Date(character(0)),
      source    = character(0)
    )
  }

  # Filtrage temporel + spatial strict sur le polygone Valais
  combined <- bind_rows(data_gbif, data_inat) %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(date_obs),
           date_obs >= date_start, date_obs <= date_end)

  if (nrow(combined) > 0) {
    combined_sf <- st_as_sf(combined, coords = c("longitude", "latitude"), crs = 4326)
    in_valais   <- st_intersects(combined_sf, valais, sparse = FALSE)[, 1]
    combined    <- combined[in_valais, ]
  }

  cat("  Records after filtering:", nrow(combined), "\n")
  all_species_data[[sp]] <- combined
}

# =============================================================================
# 3) CREATE BASE MATRIX
# =============================================================================

matrix_full <- bind_rows(all_species_data) %>%
  mutate(species_short = ifelse(species == "Capra ibex", "Ibex", "Bilberry")) %>%
  dplyr::select(species, species_short, longitude, latitude, date_obs, source)

# Vérification
cat("\n=== BASE MATRIX CREATED ===\n")
cat("Rows:   ", nrow(matrix_full), "\n")
cat("Columns:", ncol(matrix_full), "\n")
cat("Species:\n")
print(table(matrix_full$species_short))
head(matrix_full)

# =============================================================================
# 4) SAVE MATRIX
# =============================================================================

matrix_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/matrix_full.csv"

dir.create(dirname(matrix_path), showWarnings = FALSE, recursive = TRUE)
write.csv(matrix_full, matrix_path, row.names = FALSE)

