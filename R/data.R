#' Species Presence/ Absence data in Victoria, Australia
#'
#' Contains a table showing information of layers retrieved from `get_vic_species()` function of the CMIP6 projections of flora and fauna occurrence in Victoria derived from MaxEnt modelling by Archibald et al. (2024).
#'
#' See the `vic-cons-planning.Rmd` vignette for a demonstration of its usage.
#'
#' Details of can be found in [robust.prioritizr.data](https://github.com/jeffreyhanson/robust.prioritizr.data)
#'
#' @format ## `vic_cmip6`
#' An tibble with
#' \describe{
#'   \item{id}{ID of the layer}
#'   \item{name}{Full name of the layer}
#'   \item{species}{Name of the species}
#'   \item{class}{Class of the species}
#'   \item{proj}{Climate projection and timestep}
#'   \item{auc}{AUC metric from the model fitting process}
#'   \item{boyce}{Boyce metric from the model fitting process}
#'   \item{threshold}{Threshold}
#'   \item{scenario}{Climate scenario, can be: "historic_baseline" (historic baseline), ssp126 (SSP1-RCP2.6), ssp245 (SSP2-RCP4.5), ssp370 (SSP3-RCP7.0), or ssp585 (SSP5-RCP8.5)}
#'   \item{timestep}{Timestep (year)}
#'   \sum{sum}{Sum of the total number of cells in Victoria with historic/ projected presence under the climate scenario in that timestep}
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
