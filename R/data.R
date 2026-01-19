#' Conservation planning dataset for Victoria, Australia
#'
#' This is a conservation planning dataset for Victoria, Australia
#' that can be used to generate robust prioritizations.
#' This dataset was derived from Archibald *et al.* (2024),
#' Department of Climate Change, Energy, the Environment and Water (2024),
#' Global Administrative Areas (2024), and Williams *et al.* (2020).
#' For an example of using this dataset, please refer to the
#' the *Example using Victoria, Australia* vignette
#' (\code{vignette("vic-cons-planning", package = "robust.prioritizr")}).
#'
#' @details
#' Briefly, this dataset contains
#' `r terra::global(get_vic_cost(), "notNA")[[1]]` planning units and
#' `r length(unique(get_vic_species_metadata()$species))` terrestrial vertebrate
#' species For each species, the dataset contains the present-day spatial
#' distribution for the species as well as projections for the species' future
#' spatial distribution at
#' `r length(unique(get_vic_species_metadata()$timestep)) - 1`
#' time periods based on
#' `r length(unique(get_vic_species_metadata()$scenario)) - 1`
#' combinations of climate models and scenarios.
#' To account for existing conservation efforts, the dataset also
#' contains the locations of existing protected areas.
#'
#' The following functions are provided to import the dataset:
#'
#' \describe{
#'
#' \item{`get_vic_study_area()`}{
#' A [sf::st_sf()] object containing the spatial boundary of the Victoria,
#' Australia (derived from Global Administrative Areas 2024).
#' }
#'
#' \item{`get_vic_cost()`}{
#' A [terra::rast()] object containing the opportunity costs associated
#' with protected area establishment (derived from Williams *et al.* 2020).
#' This object contains a single layer, and grid cells denote planning units.
#' Cells contain positive continuous values, such that greater values denote
#' greater opportunity costs.
#' }
#'
#' \item{`get_vic_pa()`}{
#' A [terra::rast()] object containing the locations of existing protected areas
#' (derived from DCCEEW 2024).
#' This object contains a single layer, and grid cells denote planning units.
#' Cells contain binary values indicating if existing protected areas cover, at
#' least, 50% of the grid cell or not.
#' }
#'
#' \item{`get_vic_species()`}{
#' A [terra::rast()] object containing the present-day and potential future
#' spatial distributions of terrestrial vertebrate species
#' (derived from Archibald *et al.* 2024).
#' This object contains a `r terra::nlyr(get_vic_species())` layers,
#' where each layer corresponds to the predicted spatial distribution of a
#' particular species at a particular point in time based on a particular
#' climate scenario. For a given layer, grid cells denote planning units.
#' Cells contain binary values indicating if the species is predicted to be
#' present or absent within the cell.
#' }
#'
#' \item{`get_vic_species_metadata()`}{
#' A [tibble::tibble()] data frame containing information on the species'
#' spatial distribution data (i.e., `get_vic_species()`).
#' Here, each row of the `dadta.frame`
#' corresponds to each layer of species' spatial distribution data,
#' and columns describe different aspects of the layers.
#' This object contains columns with the following values.
#'
#' \describe{
#'
#' \item{id}{
#' `integer` index values for the layers. E.g., the fifth layer is associated
#' with an `id` value of 5.
#' }
#'
#' \item{id}{
#' `character` names for the layers. These values correspond to
#' `names(get_vic_species())`.
#' }
#'
#' \item{species}{
#' `character` scientific names of the species associated with the layers.
#' }
#'
#' \item{class}{
#' `character` taxonomic classes of the species associated with the layers.
#' }
#'
#' \item{proj}{
#' `character` names of the climate projections and timesteps associated with
#' the layers.
#' Layers that represent the species' present-day distributions are denoted
#' with a value of `"historic_baseline_1990"`.
#' Also, layers that represent the species' potential future distributions
#' denoted with values of `"GCM-Ensembles_ssp126_2030"`, `"GCM-
#' Ensembles_ssp126_2050"`, `"GCM-Ensembles_ssp126_2070"`, `"GCM-
#' Ensembles_ssp126_2090"`, `"GCM-Ensembles_ssp245_2030"`,
#' `"GCM- Ensembles_ssp245_2050"`, `"GCM-Ensembles_ssp245_2070"`,
#' `"GCM-Ensembles_ssp245_2090"`, `"GCM-Ensembles_ssp370_2030"`,
#' `"GCM-Ensembles_ssp370_2050"`, `"GCM-Ensembles_ssp370_2070"`,
#' `"GCM-Ensembles_ssp370_2090"`, `"GCM-Ensembles_ssp585_2030"`,
#' `"GCM-Ensembles_ssp585_2050"`, `"GCM-Ensembles_ssp585_2070"`, and
#' `"GCM-Ensembles_ssp585_2090"`.
#' Note that these values provide the same information as the
#' `timestep` and `scenario` columns, and are provided to help
#' with subsetting the data.
#' }
#'
#' \item{timestep}{
#' `numeric` year of the datasets used to generate the layers.
#' Layers that represent the
#' species' present-day distributions are denoted with a year of `1990`, and
#' layers that represent the species' potential future distributions
#' denoted with years of `2030`, `2050`, `2070`, and `2090`.
#' }
#'
#' \item{scenario}{
#' `character` names of the climate scenarios used to generate the layers.
#' Layers that represent the
#' species' present day distributions are denoted with `"historic_baseline"`.
#' Also, layers that represent species' future distributions are
#' associated with a particular Shared Socioeconomic Pathway (SSP) and
#' Representative Concentration Pathways (RCP), such as
#' (`"ssp126"`) SSP 1 and RCP 2.6, (`"ssp245"`) SSP 2 and RCP 4.5,
#' (`"ssp370"`) SSP 3 and RCP 7.0, and (`"ssp585"`) SSP 5 and RCP 8.5.
#' }
#'
#' \item{sum}{
#' `numeric` number of planning units where the species are predicted to be
#' present within each of the layers.
#' }
#'
#' }
#'
#' }
#'
#' }
#'
#' @seealso
#' The code used to prepare this dataset are available online
#' (<https://github.com/jeffreyhanson/robust.prioritizr.data>),
#'
#' @docType data
#'
#' @name data
#'
#' @aliases get_vic_study_area
#' @aliases get_vic_cost
#' @aliases get_vic_pa
#' @aliases get_vic_species
#' @aliases get_vic_species_metadata
#'
#' @format \describe{
#'   \item{`get_vic_study_area()`}{[sf::st_sf()] object.}
#'   \item{`get_vic_cost()`}{[terra::rast()] object}
#'   \item{`get_vic_species()`}{[terra::rast()] object}
#'   \item{`get_vic_pa()`}{[terra::rast()] object}
#'   \item{`vic_species_metadata()`}{[tibble::tibble()] object}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Archibald CL, Summers DM, Graham EM, Bryan B (2024) Habitat suitability maps
#' for Australian flora and fauna under CMIP6 climate scenarios.
#' *GigaScience*, 13:giae002.
#'
#' DCCEEW (2024), Collaborative Australian Protected Areas Database (CAPAD).
#'
#' Global Administrative Areas (2024). Database of Global Administrative Areas.
#' Version 4.1. Available at https://gadm.org (accessed on 15 August 2025).
#'
#' Williams BA, Venter O, Allan JR, Atkinson SC, Rehbein JA, Ward M,
#' Di Marco M, Grantham HS, Ervin J, Goetz SJ, Hansen AJ, Jantz P, Pillay R,
#' Rodríguez-Buriticá S, Supples C, Virnig ALS, Watson JEM (2020)
#' Change in terrestrial human footprint drives continued loss of intact
#' ecosystems. *One Earth*, 3:371--382.
#'
#' @examples
#' # load spatial R packages
#' library(sf)
#' library(terra)
#'
#' # load data
#' vic_study_area <- get_vic_study_area()
#' vic_cost <- get_vic_cost()
#' vic_species <- get_vic_species()
#' vic_pa <- get_vic_pa()
#'  vic_species_metadata <- get_vic_species_metadata()
#'
#' # preview data
#' print(vic_species_metadata)
#' print(vic_study_area)
#' print(vic_cost)
#' print(vic_species)
#' print(vic_pa)
#'
#' # visualize data
#' plot(vic_study_area, main = "vic_study_area")
#' plot(vic_cost, main = "vic_cost")
#' plot(vic_species, main = "vic_species")
#' plot(vic_pa, main = "vic_pa")
NULL

#' @rdname data
#' @export
get_vic_study_area <- function() {
  sf::read_sf(
    system.file("extdata", "vic_study_area.shp", package = "robust.prioritizr")
  )
}

#' @rdname data
#' @export
get_vic_cost <- function() {
  terra::rast(
    system.file("extdata", "vic_cost.tif", package = "robust.prioritizr")
  )
}

#' @rdname data
#' @export
get_vic_pa <- function() {
  terra::rast(
    system.file("extdata", "vic_pa.tif", package = "robust.prioritizr")
  )
}

#' @rdname data
#' @export
get_vic_species <- function() {
  terra::rast(
    system.file("extdata", "vic_species.tif", package = "robust.prioritizr")
  )
}

#' @rdname data
#' @export
get_vic_species_metadata <- function() {
  # import data
  x <- tibble::as_tibble(utils::read.csv(
    system.file("extdata", "vic_cmip6.csv", package = "robust.prioritizr"),
    header = TRUE
  ))
  # define relevant columns
  col_names <- c(
    "id", "name", "species", "class", "proj", "timestep", "scenario", "sum"
  )
  # return data with relevant columns
  x[, col_names, drop = FALSE]
}
