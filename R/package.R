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
#' * Package website ()
#' * Source code repository ()
#' * Report bugs ()
#'
#' @author
#' Removed for double blind review
#'
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
