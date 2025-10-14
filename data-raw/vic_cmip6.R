## code to prepare `vic_cmip6` dataset goes here

library(terra)
library(readr)
library(dplyr)
library(stringr)
library(sf)

# download data
piggyback::pb_download(
  "cost.tif", dest = tempdir(),
  repo="jeffreyhanson/robust.prioritizr.data", tag="v1.0.0"
)
# import data
cost  <- rast(file.path(tempdir(), "cost.tif"))

piggyback::pb_download(
  "pa.tif", dest = tempdir(),
  repo="jeffreyhanson/robust.prioritizr.data", tag="v1.0.0"
)
pa <- rast(file.path(tempdir(), "pa.tif"))

piggyback::pb_download(
  "species.tif", dest = tempdir(),
  repo="jeffreyhanson/robust.prioritizr.data", tag="v1.0.0"
)
species <- rast(file.path(tempdir(), "species.tif"))

study_area <- cost
values(study_area)[!is.na(values(study_area))] <- 1
names(study_area) <- "study_area"
study_area <- as.polygons(study_area, na.rm = TRUE)

piggyback::pb_download(
  "species.csv", dest = tempdir(),
  repo="jeffreyhanson/robust.prioritizr.data", tag="v1.0.0"
)
species_details = read_csv(file.path(tempdir(), "species.csv"))

species_details <- species_details %>%
  mutate(scenario = str_extract(proj, "ssp[0-9]{3,4}|historic_baseline")) %>%
  mutate(timestep = as.numeric(str_extract(proj, "[0-9]{4}")))

# Data checks
if (!(length(unique(species_details$name)) == length(unique(names(species))))) {
  stop("Number of unique species-scenario-year combinations don't match.")
}

if (sum(is.na(match(names(species), species_details$name)) > 0)) {
  stop("Not all species are uniquely identified in the raster layer names.")
}

threated_species <- c("Philoria_frosti",
                      "Petrogale_penicillata",
                      "Liopholis_guthega",
                      "Tympanocryptis_lineata",
                      "Lichenostomus_melanops",
                      "Gymnobelideus_leadbeateri",
                      "Stipiturus_mallee",
                      "Burramys_parvus",
                      "Pseudophryne_pengilleyi",
                      "Neophema_chrysogaster",
                      "Pedionomus_torquatus",
                      "Anthochaera_phrygia",
                      "Pseudomys_fumeus",
                      "Miniopterus_schreibersii",
                      "Pseudophryne_corroboree",
                      "Litoria_spenceri",
                      "Mixophyes_balbus",
                      "Sarcophilus_harrisii")

# threated_species <- unique(species_details$species)

species_details_subset <- species_details %>%
  filter(species %in% threated_species) %>%
  arrange(id)

length(unique(species_details_subset$species))

species_subset <- species[[species_details_subset$id]]

# Reset ID for species detail
species_details_subset$id <- 1:nrow(species_details_subset)

# Find the total area with species presence
global_sums = global(species_subset, 'sum', na.rm = T)
species_details_subset$sum = unname(unlist(global_sums))


# Write outputs to extdata -----
write_raster_w_params <- function(raster, path) {
  writeRaster(
    raster, path,
    NAflag = 2, overwrite = TRUE, datatype = "INT1U",
    gdal = c("COMPRESS=ZSTD", "NBITS=2", "TILED=YES", "ZSTD_LEVEL=9")
  )
}

data_path <- "inst/extdata"

writeRaster(cost, file.path(data_path, "vic_cost.tif"), overwrite = TRUE)

write_raster_w_params(pa, file.path(data_path, "vic_pa.tif"))

write_raster_w_params(species_subset, file.path(data_path, "vic_species.tif"))

writeVector(
  study_area, file.path(data_path, "vic_study_area.shp"),
  overwrite = TRUE
)

vic_cmip6 <- species_details_subset

write.csv(
  vic_cmip6, file.path(data_path, "vic_cmip5.csv"),
  row.names = FALSE , quote = TRUE
)
