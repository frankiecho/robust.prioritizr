#' @title robust.prioritizr: Robust Systematic Conservation Prioritization in R
#'
#' @description
#' The \pkg{robust.prioritizr} R package provides tools for building and solving
#' robust systematic conservation prioritization problems. It extends the
#' \pkg{prioritizr} package to account for uncertainty in the input data.
#' This is particularly useful when working with data that is subject to
#' change, such as species distribution models under climate change scenarios.
#'
#' @details
#' This package contains several vignettes that are designed to
#' showcase its functionality. To view them, please use the code
#' `vignette("name", package = "robust.prioritizr")` where `"name"` is the
#' name of the desired vignette (e.g., `"robust.prioritizr"`).
#'
#' \describe{
#'
#' \item{robust.prioritizr}{
#' Brief introduction to the package.
#' }
#'
#' \item{climate-sdm}{
#' Example using simulated data from a species distribution model.
#' }
#'
#' \item{vic-cons-planning}{
#' Example using Victoria, Australia.
#' }
#'
#' }
#'
#' @seealso
#' Useful links:
#' * Package website (<https://frankiecho.github.io/robust.prioritizr/>)
#' * Source code repository (<https://github.com/frankiecho/robust.prioritizr>)
#' * Report bugs (<https://github.com/frankiecho/robust.prioritizr/issues>)
#'
#' @author
#'  Authors:
#' * Frankie Cho \email{frankie.cho@monash.edu} ([ORCID](https://orcid.org/0000-0003-1369-4980))
#' * Jeffrey O Hanson \email{jeffrey.hanson@uqconnect.edu.au} ([ORCID](https://orcid.org/0000-0002-4716-6134))
#'
#' @srrstats {G1.0} Primary references (Charnes & Cooper 1959; Rockafellar &
#'   Uryasev 2000) are cited in DESCRIPTION and all main function documentation.
#' @srrstats {G1.1} Package description documents that this is the first
#'   implementation within R of chance-constrained programming and CVaR-based
#'   robust optimization for systematic conservation prioritization.
#' @srrstats {G1.2} A life cycle statement is provided in CONTRIBUTING.md, and
#'   a lifecycle badge is present in the README.
#' @srrstats {G1.4} All exported functions are documented with roxygen2.
#' @srrstats {G1.4a} All internal functions include a @noRd tag.
#' @srrstats {SP1.0} This package operates on two-dimensional geographic
#'   (curvilinear) spatial data, consistent with inputs accepted by prioritizr
#'   (SpatRaster, sf objects). It is applicable to geographic domains only.
#' @srrstats {SP1.1} The package is applicable to two-dimensional spatial data
#'   only, consistent with the input requirements of terra and prioritizr.
#' @srrstats {SP2.1} The package does not use sp; it relies on terra and sf
#'   via prioritizr.
#' @srrstats {SP2.4, SP2.4a} PROJ6+ and WKT2 compliance is inherited from
#'   terra and sf, which are required dependencies via prioritizr.
#' @srrstats {SP2.2, SP2.2a, SP2.2b} Compatibility with established spatial
#'   workflows (terra, sf) is demonstrated throughout the vignettes, and tests
#'   use SpatRaster inputs consistent with these packages.
#' @srrstats {G2.0, G2.0a} Input validation for vector lengths is implemented
#'   in main function documentation and enforced via assertthat assertions.
#' @srrstats {G2.1, G2.1a} Type checking for all inputs is documented in
#'   function-specific roxygen tags and enforced via assertthat.
#' @srrstats {G2.13} Missing data checks are performed via assertthat in core
#'   constraint and objective functions before optimization.
#' @srrstats {G5.0, G5.1} Test suite uses standard spatial data (sim data from
#'   prioritizr, Victoria study area) with known properties, and all test data
#'   are examples accessible within the package and vignettes.
#' @srrstats {G5.2, G5.2a, G5.2b} Appropriate error and warning behavior is
#'   explicitly tested; all messages produced by stop(), warning(), and message()
#'   are unique and triggered by explicit test conditions.
#' @name robust.prioritizr
#' @docType package
#' @aliases robust.prioritizr-package
"_PACKAGE"

# avoid CRAN check NOTES due to R6 classes
# see: https://github.com/r-lib/R6/issues/230
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("self"))
}

# define imports
#' @importFrom Rcpp evalCpp
NULL

#' @useDynLib robust.prioritizr, .registration = TRUE
NULL
