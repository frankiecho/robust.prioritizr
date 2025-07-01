#' robustprioritizr: Spatial conservation planning under uncertainty
#'
#' TODO.
#'
#' @details
#' TODO.
#'
#' @section Citation:
#' TODO.
#'
#' @seealso
#' TODO.
#'
#' @author
#' TODO.
#'
#' @references
#' TODO.
#'
#' @name robustprioritizr
#' @docType package
#' @aliases robustprioritizr-package
"_PACKAGE"

# avoid CRAN check NOTES due to R6 classes
# see: https://github.com/r-lib/R6/issues/230
if (getRversion() >= "2.15.1")  utils::globalVariables(c("self"))

# define imports
#' @importFrom Rcpp evalCpp
NULL

#' @useDynLib robustprioritizr, .registration = TRUE
NULL
