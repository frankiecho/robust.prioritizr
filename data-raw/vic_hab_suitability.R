## code to prepare `vic_hab_suitability` dataset goes here

library(terra)
library(readr)

data_path <- "C:/Github/robust.prioritizr.data/data/final"

vic_hab_suitability <- list(
  species = wrap(rast(file.path(data_path, "species.tif"))),
  cost = wrap(rast(file.path(data_path, "cost.tif"))),
  pa = wrap(rast(file.path(data_path, "pa.tif"))),
  species_details = read_csv(file.path(data_path, "species.csv"))
)

usethis::use_data(vic_hab_suitability, overwrite = TRUE)
