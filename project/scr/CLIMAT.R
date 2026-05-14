###############################################################################
# CLIMATE DATA EXTRACTION WITH CHELSA — Capra ibex & Vaccinium myrtillus
# Region: Canton of Valais, Switzerland
# Reads occurrences directly from matrix_full.csv
###############################################################################

# =============================================================================
# 1) PACKAGES
# =============================================================================

library(Rchelsa)
library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/matrix_full.csv"

occ_valais <- read.csv(matrix_path)

cat("Matrix loaded —", nrow(occ_valais), "rows,", ncol(occ_valais), "cols\n") #I added this line to check the data loading
table(occ_valais$species_short)

# =============================================================================
# 3) SEPARATE SPECIES + DEDUPLICATE COORDINATES
# =============================================================================

ibex_df     <- occ_valais %>% filter(species_short == "Ibex")
bilberry_df <- occ_valais %>% filter(species_short == "Bilberry")

coords_ibex <- ibex_df %>%
  dplyr::select(longitude, latitude) %>%
  distinct()

coords_bilberry <- bilberry_df %>%
  dplyr::select(longitude, latitude) %>%
  distinct()

cat("Ibex unique coordinates:    ", nrow(coords_ibex), "\n")
cat("Bilberry unique coordinates:", nrow(coords_bilberry), "\n")

# =============================================================================
# 4) EXTRACT MONTHLY Tmax — MOYENNe 2015-2020 pour Ibex
# =============================================================================
# On extrait chaque année séparément puis on fait la moyenne
# pour éviter le biais d'une année atypique (ex: canicule 2018)

years <- 2015:2020

extract_mean_var <- function(coords, var, years) {
  all_years <- lapply(years, function(yr) {
    cat("  Extracting", var, "for year", yr, "...\n")
    r <- getChelsa(
      var       = var,
      coords    = coords,
      startdate = as.Date(paste0(yr, "-01-01")),
      enddate   = as.Date(paste0(yr+1, "-01-01")),
      dataset   = "chelsa-monthly"
    )
    colMeans(r %>% dplyr::select(-time) %>% as.matrix(), na.rm = TRUE)
  })
  # Moyenne sur toutes les années
  Reduce("+", all_years) / length(all_years)
}

cat("\nExtracting Tmax for Ibex (2015-2020)...\n")
tmax_ibex_mean <- extract_mean_var(coords_ibex, "tasmax", years) - 273.15
tmax_ibex_df   <- data.frame(longitude   = coords_ibex$longitude,latitude    = coords_ibex$latitude,tmax_mean_c = as.numeric(tmax_ibex_mean))

cat("\nExtracting Tmax for Bilberry (2015-2020)...\n")
tmax_bilberry_mean <- extract_mean_var(coords_bilberry, "tasmax", years) - 273.15
tmax_bilberry_df   <- data.frame(longitude   = coords_bilberry$longitude,latitude    = coords_bilberry$latitude,tmax_mean_c = as.numeric(tmax_bilberry_mean))

# =============================================================================
# 5) EXTRACT MONTHLY PRECIPITATION — MOYENNE 2015-2020 pour Bluberry
# =============================================================================

cat("\nExtracting Precipitation for Ibex (2015-2020)...\n")
prec_ibex_mean <- extract_mean_var(coords_ibex, "pr", years)
prec_ibex_df   <- data.frame(longitude        = coords_ibex$longitude,latitude         = coords_ibex$latitude,prec_mean_annual = as.numeric(prec_ibex_mean))

cat("\nExtracting Precipitation for Bilberry (2015-2020)...\n")
prec_bilberry_mean <- extract_mean_var(coords_bilberry, "pr", years)
prec_bilberry_df   <- data.frame(longitude        = coords_bilberry$longitude,latitude         = coords_bilberry$latitude,prec_mean_annual = as.numeric(prec_bilberry_mean))

# =============================================================================
# 6) JOIN CLIMATE TO OCCURRENCES
# =============================================================================

ibex_climate_df <- ibex_df %>%
  left_join(tmax_ibex_df, by = c("longitude", "latitude")) %>%
  left_join(prec_ibex_df, by = c("longitude", "latitude"))

bilberry_climate_df <- bilberry_df %>%
  left_join(tmax_bilberry_df, by = c("longitude", "latitude")) %>%
  left_join(prec_bilberry_df, by = c("longitude", "latitude"))

all_climate_df <- bind_rows(ibex_climate_df, bilberry_climate_df)

# =============================================================================
# 7) PLOT — Tmax by species (moyenne 2015-2020)
# =============================================================================

figures_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/figures"
dir.create(figures_path, showWarnings = FALSE)

p_tmax <- ggplot(all_climate_df,
                 aes(x = tmax_mean_c, fill = species_short, color = species_short)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  theme_classic() +
  labs(title    = "Annual mean Tmax (CHELSA 2015-2020) by species — Valais",
       subtitle = "Mean over 6 years to avoid single-year bias",
       x = "Annual mean Tmax (°C)", y = "Density",
       fill = "Species", color = "Species")

print(p_tmax)
ggsave(file.path(figures_path, "fig_climate_tmax.png"),
       p_tmax, width = 9, height = 6, dpi = 300)

# =============================================================================
# 8) PLOT — Precipitation by species (moyenne 2015-2020)
# =============================================================================

p_prec <- ggplot(all_climate_df,
                 aes(x = prec_mean_annual, fill = species_short, color = species_short)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  theme_classic() +
  labs(title    = "Annual mean precipitation (CHELSA 2015-2020) by species — Valais",
       subtitle = "Mean over 6 years to avoid single-year bias",
       x = "Annual mean precipitation (mm)", y = "Density",
       fill = "Species", color = "Species")

print(p_prec)
ggsave(file.path(figures_path, "fig_climate_precip.png"),
       p_prec, width = 9, height = 6, dpi = 300)

# =============================================================================
# 9) CURRENT VS FUTURE CLIMATE — SSP126 ET SSP585
# =============================================================================

# ---- 9A) Température actuelle juillet (climatologie 1981-2010) ----
cat("\nExtracting current July temperature...\n")

tas_cur_ibex     <- getChelsa(var = "tas", coords = coords_ibex,
                               date = c(7, 1981, 2010),
                               dataset = "chelsa-climatologies")
tas_cur_bilberry <- getChelsa(var = "tas", coords = coords_bilberry,
                               date = c(7, 1981, 2010),
                               dataset = "chelsa-climatologies")

tas_cur_ibex_df <- data.frame(
  longitude          = coords_ibex$longitude,
  latitude           = coords_ibex$latitude,
  tas_current_july_c = tas_cur_ibex %>% dplyr::select(-time) %>%
    unlist() %>% as.numeric() - 273.15)

tas_cur_bilberry_df <- data.frame(
  longitude          = coords_bilberry$longitude,
  latitude           = coords_bilberry$latitude,
  tas_current_july_c = tas_cur_bilberry %>% dplyr::select(-time) %>%
    unlist() %>% as.numeric() - 273.15)

# ---- 9B) Température future juillet 2050 — SSP126 ----
cat("\nExtracting future July temperature SSP126...\n")

tas_fut_ssp126_ibex     <- getChelsa(var = "tas", coords = coords_ibex,
                                      date    = as.Date("2050-07-01"),
                                      dataset = "chelsa-climatologies",
                                      ssp     = "ssp126",
                                      forcing = "MPI-ESM1-2-HR")
tas_fut_ssp126_bilberry <- getChelsa(var = "tas", coords = coords_bilberry,
                                      date    = as.Date("2050-07-01"),
                                      dataset = "chelsa-climatologies",
                                      ssp     = "ssp126",
                                      forcing = "MPI-ESM1-2-HR")

tas_fut_ssp126_ibex_df <- data.frame(
  longitude                  = coords_ibex$longitude,
  latitude                   = coords_ibex$latitude,
  tas_future_ssp126_july_c   = tas_fut_ssp126_ibex %>% dplyr::select(-time) %>%
    unlist() %>% as.numeric() - 273.15)

tas_fut_ssp126_bilberry_df <- data.frame(
  longitude                  = coords_bilberry$longitude,
  latitude                   = coords_bilberry$latitude,
  tas_future_ssp126_july_c   = tas_fut_ssp126_bilberry %>% dplyr::select(-time) %>%
    unlist() %>% as.numeric() - 273.15)

# ---- 9C) Température future juillet 2050 — SSP585 ----
cat("\nExtracting future July temperature SSP585...\n")

tas_fut_ssp585_ibex     <- getChelsa(var = "tas", coords = coords_ibex,
                                      date    = as.Date("2050-07-01"),
                                      dataset = "chelsa-climatologies",
                                      ssp     = "ssp585",
                                      forcing = "MPI-ESM1-2-HR")
tas_fut_ssp585_bilberry <- getChelsa(var = "tas", coords = coords_bilberry,
                                      date    = as.Date("2050-07-01"),
                                      dataset = "chelsa-climatologies",
                                      ssp     = "ssp585",
                                      forcing = "MPI-ESM1-2-HR")

tas_fut_ssp585_ibex_df <- data.frame(
  longitude                  = coords_ibex$longitude,
  latitude                   = coords_ibex$latitude,
  tas_future_ssp585_july_c   = tas_fut_ssp585_ibex %>% dplyr::select(-time) %>%
    unlist() %>% as.numeric() - 273.15)

tas_fut_ssp585_bilberry_df <- data.frame(
  longitude                  = coords_bilberry$longitude,
  latitude                   = coords_bilberry$latitude,
  tas_future_ssp585_july_c   = tas_fut_ssp585_bilberry %>% dplyr::select(-time) %>%
    unlist() %>% as.numeric() - 273.15)

# ---- 9D) Joindre tout et calculer les deltas ----
ibex_future_df <- ibex_climate_df %>%
  left_join(tas_cur_ibex_df,       by = c("longitude", "latitude")) %>%
  left_join(tas_fut_ssp126_ibex_df, by = c("longitude", "latitude")) %>%
  left_join(tas_fut_ssp585_ibex_df, by = c("longitude", "latitude")) %>%
  mutate(
    delta_ssp126 = tas_future_ssp126_july_c - tas_current_july_c,
    delta_ssp585 = tas_future_ssp585_july_c - tas_current_july_c
  )

bilberry_future_df <- bilberry_climate_df %>%
  left_join(tas_cur_bilberry_df,       by = c("longitude", "latitude")) %>%
  left_join(tas_fut_ssp126_bilberry_df, by = c("longitude", "latitude")) %>%
  left_join(tas_fut_ssp585_bilberry_df, by = c("longitude", "latitude")) %>%
  mutate(
    delta_ssp126 = tas_future_ssp126_july_c - tas_current_july_c,
    delta_ssp585 = tas_future_ssp585_july_c - tas_current_july_c
  )

all_future_df <- bind_rows(ibex_future_df, bilberry_future_df)

# =============================================================================
# 10) PLOT — Current vs future temperature SSP126 et SSP585
# =============================================================================

p_scatter_ssp126 <- ggplot(all_future_df,
  aes(x = tas_current_july_c, y = tas_future_ssp126_july_c,
      color = species_short)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  theme_classic() +
  labs(title    = "Current vs future July temperature — SSP126 (2050)",
       subtitle = "Optimistic scenario — points above dashed line = warming",
       x = "Current July temperature (°C, 1981-2010)",
       y = "Future July temperature (°C, 2050)",
       color = "Species")

print(p_scatter_ssp126)
ggsave(file.path(figures_path, "fig_climate_future_ssp126.png"),
       p_scatter_ssp126, width = 9, height = 6, dpi = 300)

p_scatter_ssp585 <- ggplot(all_future_df,
  aes(x = tas_current_july_c, y = tas_future_ssp585_july_c,
      color = species_short)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  theme_classic() +
  labs(title    = "Current vs future July temperature — SSP585 (2050)",
       subtitle = "Pessimistic scenario — points above dashed line = warming",
       x = "Current July temperature (°C, 1981-2010)",
       y = "Future July temperature (°C, 2050)",
       color = "Species")

print(p_scatter_ssp585)
ggsave(file.path(figures_path, "fig_climate_future_ssp585.png"),
       p_scatter_ssp585, width = 9, height = 6, dpi = 300)

# =============================================================================
# 11) PLOT — Delta température par espèce (SSP126 vs SSP585)
# =============================================================================
# Cette figure montre de combien de degrés les zones d'occurrence
# vont se réchauffer d'ici 2050 selon les deux scénarios

delta_long <- all_future_df %>%
  dplyr::select(species_short, delta_ssp126, delta_ssp585) %>%
  pivot_longer(
    cols      = c(delta_ssp126, delta_ssp585),
    names_to  = "scenario",
    values_to = "delta_temp"
  ) %>%
  mutate(scenario = ifelse(scenario == "delta_ssp126",
                           "SSP126 (optimistic)",
                           "SSP585 (pessimistic)"))

p_delta <- ggplot(
  delta_long %>% filter(!is.na(delta_temp)),
  aes(x = delta_temp, fill = species_short, color = species_short)
) +
  geom_density(alpha = 0.4, linewidth = 1) +
  facet_wrap(~ scenario, ncol = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Projected temperature change by 2050 at species occurrence sites",
    subtitle = "Delta = Future (2050) - Current (1981-2010) July temperature",
    x        = "Temperature change (°C)",
    y        = "Density",
    fill     = "Species", color = "Species"
  ) +
  theme(legend.position = "bottom")

print(p_delta)
ggsave(file.path(figures_path, "fig_climate_delta_temperature.png"),
       p_delta, width = 9, height = 8, dpi = 300)

# =============================================================================
# 12) UPDATE MATRIX
# =============================================================================

matrix_full <- read.csv(matrix_path)

new_cols_chelsa <- all_climate_df %>%
  dplyr::group_by(longitude, latitude, species_short) %>%
  dplyr::summarise(
    tmax_mean_c      = mean(tmax_mean_c,      na.rm = TRUE),
    prec_mean_annual = mean(prec_mean_annual,  na.rm = TRUE),
    .groups = "drop")

new_cols_future <- all_future_df %>%
  dplyr::group_by(longitude, latitude, species_short) %>%
  dplyr::summarise(
    tas_current_july_c     = mean(tas_current_july_c,     na.rm = TRUE),
    tas_future_ssp126_july = mean(tas_future_ssp126_july_c, na.rm = TRUE),
    tas_future_ssp585_july = mean(tas_future_ssp585_july_c, na.rm = TRUE),
    delta_ssp126           = mean(delta_ssp126,            na.rm = TRUE),
    delta_ssp585           = mean(delta_ssp585,            na.rm = TRUE),
    .groups = "drop")

matrix_full <- matrix_full %>%
  left_join(new_cols_chelsa, by = c("longitude", "latitude", "species_short")) %>%
  left_join(new_cols_future, by = c("longitude", "latitude", "species_short"))

write.csv(matrix_full, matrix_path, row.names = FALSE)
cat("Matrix updated —", ncol(matrix_full), "columns,", nrow(matrix_full), "rows\n")
