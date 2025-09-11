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
#' The main functions provided by this package are:
#'
#' - [add_robust_min_set_objective()]
#' - [add_robust_min_shortfall_objective()]
#' - [add_constant_robust_constraints()]
#' - [add_variable_robust_constraints()]
#'
#' For more information on using this package, see the vignettes:
#' `vignette("robust.prioritizr", package = "robust.prioritizr")`
#'
#' @name robust.prioritizr
#' @docType package
#' @aliases robust.prioritizr-package
"_PACKAGE"

# avoid CRAN check NOTES due to R6 classes
# see: https://github.com/r-lib/R6/issues/230
if (getRversion() >= "2.15.1")  utils::globalVariables(c("self"))

# define imports
#' @importFrom Rcpp evalCpp
NULL

#' @useDynLib robust.prioritizr, .registration = TRUE
NULL
