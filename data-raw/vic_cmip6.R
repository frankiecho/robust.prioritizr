## code to prepare `vic_cmip6` dataset goes here

library(terra)
library(readr)
library(dplyr)
library(stringr)
library(sf)

data_path <- "C:/Github/robust.prioritizr.data/data/final"

study_area <- vect(file.path(data_path, "../raw/gadm/gadm41_AUS_shp/gadm41_AUS_1.shp"))
study_area <- study_area[study_area$GID_1=="AUS.10_1",]

species = rast(file.path(data_path, "species.tif"))

study_area <- study_area %>%
  project("EPSG:3577")

species_details = read_csv(file.path(data_path, "species.csv"))

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
species_details_subset$sum = global_sums

cost = wrap(rast(file.path(data_path, "cost.tif")))

pa = wrap(rast(file.path(data_path, "pa.tif")))

vic_cmip6 <- list(
  species = wrap(species_subset),
  cost = cost,
  pa = pa,
  species_details = species_details_subset,
  study_area = wrap(study_area)
)

usethis::use_data(vic_cmip6, overwrite = TRUE)
