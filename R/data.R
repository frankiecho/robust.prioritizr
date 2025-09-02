#' Species Presence/ Absence data in Victoria, Australia
#'
#' Contains a table showing information of layers retrieved from `get_vic_species()` function of the CMIP6 projections of flora and fauna occurrence in Victoria derived from MaxEnt modelling by Archibald et al. (2024).
#'
#'
#' Cost data is based on the Human Footprint Index 2013.
#'
#' Protected area data are from the Collaborative Australian Protected Areas Database (2024),
#' DCCEEW, Australian Government.
#'
#' Details of can be found in [robust.prioritizr.data](https://github.com/jeffreyhanson/robust.prioritizr.data)
#'
#' @format ## `vic_cmip6`
#' An R object with
#' \describe{
#'   \item{species}{Wrapped 5km terra SpatRaster object of species presence/ absence under different CIMP6 climate projections}
#'   \item{cost}{Wrapped 5km terra SpatRaster object of conservation cost proxied by the Human Footprint Index}
#'   \item{pa}{Wrapped 5km terra SpatRaster object of protected areas (1 = Protected Area, 0 = Not Protected Area)}
#'   \item{species_details}{Tibble object of the details of the species presence/ absence data, including (a) name: name
#'   of the species layer, (b) species: species name, (c) class: species class, proj: climate projection, auc: Training AUC metric for model fit,
#'   boyce: Spearman correlation (the Boyce index value), threshold: MaxEnt threshold.}
#' }
#'
#'
#' @references
#' Archibald, C., Summers, D., Graham, E. and Bryan, B. (2024). Habitat suitability maps
#' for Australian flora and fauna under CMIP6 climate scenarios. GigaScience, Volume
#' 13, 2024, giae002. <https://doi.org/10.1093/gigascience/giae002>
#'

#'
"vic_cmip6"

#' Species Occurrence climate projections in Victoria, Australia
#'
#' Load a terra raster object of CMIP6 projections of flora and fauna occurrence in Victoria derived from MaxEnt modelling by Carla Archibald et al. (2024). It was cropped to the state of Victoria, converted to
#' a presence/ absence metric based on a threshold, and subsetted to only contain
#' species predictions with adequate model fit (i.e., AUC threshold > 0.7, Boyce threshold > 0.5
#' and Minimum Area Threshold > 1.0). It was subsetted to contain 17 species of interest.
#'
#' @examples
#' vic_species <- get_vic_species()
#'
#' @references
#' Archibald, C., Summers, D., Graham, E. and Bryan, B. (2024). Habitat suitability maps
#' for Australian flora and fauna under CMIP6 climate scenarios. GigaScience, Volume
#' 13, 2024, giae002. <https://doi.org/10.1093/gigascience/giae002>
#'
#' @export
get_vic_species <- function() {
  x <- terra::rast(
    system.file("extdata", "vic_species.tif", package = "robust.prioritizr")
  )
  return(x)
}

#' Conservation cost proxy of Victoria, Australia
#'
#' Load a terra raster of a proxy of conservation cost, from
#' the Human Footprint Index by Brooke Williams et al. (2020), cropped to
#' the state of Victoria, Australia.
#'
#' @references
#' Williams, B. A., Venter, O., ..., Watson, J. E. M. (2020). Change in Terrestrial Human Footprint Drives Continued Loss of Intact Ecosystems. One Earth, 3, 3, 371-382.
#'
#' @export
get_vic_cost <- function() {
  x <- terra::rast(
    system.file("extdata", "vic_cost.tif", package = "robust.prioritizr")
  )
  return(x)
}

#' Study area polygon of Victoria, Australia
#'
#' Load an R sf object of the state of Victoria, Australia, defining the study area
#' of the Victoria case study.
#'
#' @export
get_vic_study_area <- function() {
  x <- sf::st_read(
    system.file("extdata", "vic_study_area.shp", package = "robust.prioritizr"),
    quiet = T
  )
  return(x)
}

#' Existing protected areas of Victoria, Australia
#'
#' Load a raster indicating whether the cell is covered by a protected area (1) or not(0).
#'
#' @references
#' DCCEEW (2024), Collaborative Australian Protected Areas Database (CAPAD).
#' <https://www.dcceew.gov.au/environment/land/nrs/science/capad>
#'
#' @export
get_vic_pa <- function() {
  x <- terra::rast(
    system.file("extdata", "vic_pa.tif", package = "robust.prioritizr")
  )
  return(x)
}
