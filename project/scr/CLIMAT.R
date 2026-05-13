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

# =============================================================================
# 2) LOAD MATRIX
# =============================================================================

matrix_path <- "/Users/davvidoo/Desktop/master_cours/semestre 2/vs code/data-analysis-David-Blanchard/project/data/matrix_full.csv"

occ_valais <- read.csv(matrix_path)

cat("Matrix loaded —", nrow(occ_valais), "rows,", ncol(occ_valais), "cols\n")
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
# 4) EXTRACT MONTHLY Tmax 2018
# =============================================================================

tmax_ibex_r    <- getChelsa(var = "tasmax", coords = coords_ibex,
                             startdate = as.Date("2018-01-01"),
                             enddate   = as.Date("2019-01-01"),
                             dataset   = "chelsa-monthly")
tmax_ibex_mean <- colMeans(tmax_ibex_r %>% dplyr::select(-time) %>% as.matrix(),
                            na.rm = TRUE) - 273.15

tmax_ibex_df <- data.frame(longitude   = coords_ibex$longitude,
                            latitude    = coords_ibex$latitude,
                            tmax_mean_c = as.numeric(tmax_ibex_mean))

tmax_bilberry_r    <- getChelsa(var = "tasmax", coords = coords_bilberry,
                                 startdate = as.Date("2018-01-01"),
                                 enddate   = as.Date("2019-01-01"),
                                 dataset   = "chelsa-monthly")
tmax_bilberry_mean <- colMeans(tmax_bilberry_r %>% dplyr::select(-time) %>% as.matrix(),
                                na.rm = TRUE) - 273.15

tmax_bilberry_df <- data.frame(longitude   = coords_bilberry$longitude,
                                latitude    = coords_bilberry$latitude,
                                tmax_mean_c = as.numeric(tmax_bilberry_mean))

# =============================================================================
# 5) EXTRACT MONTHLY PRECIPITATION 2018
# =============================================================================

prec_ibex_r    <- getChelsa(var = "pr", coords = coords_ibex,
                             startdate = as.Date("2018-01-01"),
                             enddate   = as.Date("2019-01-01"),
                             dataset   = "chelsa-monthly")
prec_ibex_mean <- colMeans(prec_ibex_r %>% dplyr::select(-time) %>% as.matrix(), na.rm = TRUE)
prec_ibex_df   <- data.frame(longitude        = coords_ibex$longitude,
                              latitude         = coords_ibex$latitude,
                              prec_mean_annual = as.numeric(prec_ibex_mean))

prec_bilberry_r    <- getChelsa(var = "pr", coords = coords_bilberry,
                                 startdate = as.Date("2018-01-01"),
                                 enddate   = as.Date("2019-01-01"),
                                 dataset   = "chelsa-monthly")
prec_bilberry_mean <- colMeans(prec_bilberry_r %>% dplyr::select(-time) %>% as.matrix(), na.rm = TRUE)
prec_bilberry_df   <- data.frame(longitude        = coords_bilberry$longitude,
                                  latitude         = coords_bilberry$latitude,
                                  prec_mean_annual = as.numeric(prec_bilberry_mean))

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
# 7) PLOT — Tmax by species
# =============================================================================

Tmax = ggplot(all_climate_df, aes(x = tmax_mean_c, fill = species_short, color = species_short)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  theme_classic() +
  labs(title = "Annual mean Tmax (CHELSA 2018) by species — Valais",
       x = "Annual mean Tmax (°C)", y = "Density",
       fill = "Species", color = "Species")
print(Tmax)
# =============================================================================
# 8) PLOT — Precipitation by species
# =============================================================================

Precipitation = ggplot(all_climate_df, aes(x = prec_mean_annual, fill = species_short, color = species_short)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values  = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  theme_classic() +
  labs(title = "Annual mean precipitation (CHELSA 2018) by species — Valais",
       x = "Annual mean precipitation (mm)", y = "Density",
       fill = "Species", color = "Species")
print(Precipitation)

# =============================================================================
# 9) CURRENT VS FUTURE CLIMATE (July, SSP126, 2050)
# =============================================================================

tas_cur_ibex     <- getChelsa(var = "tas", coords = coords_ibex,
                               date = c(7, 1981, 2010),
                               dataset = "chelsa-climatologies")
tas_cur_bilberry <- getChelsa(var = "tas", coords = coords_bilberry,
                               date = c(7, 1981, 2010),
                               dataset = "chelsa-climatologies")

tas_cur_ibex_df <- data.frame(
  longitude          = coords_ibex$longitude,
  latitude           = coords_ibex$latitude,
  tas_current_july_c = tas_cur_ibex %>% dplyr::select(-time) %>% unlist() %>% as.numeric() - 273.15)

tas_cur_bilberry_df <- data.frame(
  longitude          = coords_bilberry$longitude,
  latitude           = coords_bilberry$latitude,
  tas_current_july_c = tas_cur_bilberry %>% dplyr::select(-time) %>% unlist() %>% as.numeric() - 273.15)

tas_fut_ibex     <- getChelsa(var = "tas", coords = coords_ibex,
                               date    = as.Date("2050-07-01"),
                               dataset = "chelsa-climatologies",
                               ssp     = "ssp126", forcing = "MPI-ESM1-2-HR")
tas_fut_bilberry <- getChelsa(var = "tas", coords = coords_bilberry,
                               date    = as.Date("2050-07-01"),
                               dataset = "chelsa-climatologies",
                               ssp     = "ssp126", forcing = "MPI-ESM1-2-HR")

tas_fut_ibex_df <- data.frame(
  longitude              = coords_ibex$longitude,
  latitude               = coords_ibex$latitude,
  tas_future_july_2050_c = tas_fut_ibex %>% dplyr::select(-time) %>% unlist() %>% as.numeric() - 273.15)

tas_fut_bilberry_df <- data.frame(
  longitude              = coords_bilberry$longitude,
  latitude               = coords_bilberry$latitude,
  tas_future_july_2050_c = tas_fut_bilberry %>% dplyr::select(-time) %>% unlist() %>% as.numeric() - 273.15)

ibex_future_df <- ibex_climate_df %>%
  left_join(tas_cur_ibex_df, by = c("longitude", "latitude"),
            relationship = "many-to-many") %>%
  left_join(tas_fut_ibex_df, by = c("longitude", "latitude"),
            relationship = "many-to-many") %>%
  mutate(delta_tas_july_c = tas_future_july_2050_c - tas_current_july_c)

bilberry_future_df <- bilberry_climate_df %>%
  left_join(tas_cur_bilberry_df, by = c("longitude", "latitude"),
            relationship = "many-to-many") %>%
  left_join(tas_fut_bilberry_df, by = c("longitude", "latitude"),
            relationship = "many-to-many") %>%
  mutate(delta_tas_july_c = tas_future_july_2050_c - tas_current_july_c)

all_future_df <- bind_rows(ibex_future_df, bilberry_future_df)

# =============================================================================
# 10) PLOT — Current vs future temperature
# =============================================================================

delta_temp = ggplot(all_future_df,
       aes(x = tas_current_july_c, y = tas_future_july_2050_c, color = species_short)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Ibex" = "#D95F02", "Bilberry" = "#1B9E77")) +
  theme_classic() +
  labs(title    = "Current vs future July temperature (SSP126, 2050) — Valais",
       subtitle = "Points above the dashed line = warming projected",
       x = "Current July temperature (°C, 1981-2010)",
       y = "Future July temperature (°C, 2050)",
       color = "Species")

print(delta_temp)

# =============================================================================
# 11) UPDATE MATRIX
# =============================================================================

matrix_full <- read.csv(matrix_path)

# Garder uniquement une valeur climatique par coord unique
# en faisant la moyenne si plusieurs valeurs existent
new_cols_chelsa <- all_climate_df %>%
  group_by(longitude, latitude, species_short) %>%
  summarise(
    tmax_mean_c      = mean(tmax_mean_c,      na.rm = TRUE),
    prec_mean_annual = mean(prec_mean_annual,  na.rm = TRUE),
    .groups = "drop"
  )

new_cols_future <- all_future_df %>%
  group_by(longitude, latitude, species_short) %>%
  summarise(
    tas_current_july_c     = mean(tas_current_july_c,     na.rm = TRUE),
    tas_future_july_2050_c = mean(tas_future_july_2050_c, na.rm = TRUE),
    delta_tas_july_c       = mean(delta_tas_july_c,       na.rm = TRUE),
    .groups = "drop"
  )

matrix_full <- matrix_full %>%
  left_join(new_cols_chelsa, by = c("longitude", "latitude", "species_short")) %>%
  left_join(new_cols_future, by = c("longitude", "latitude", "species_short"))

write.csv(matrix_full, matrix_path, row.names = FALSE)
cat("Matrix updated —", ncol(matrix_full), "columns,", nrow(matrix_full), "rows\n")
