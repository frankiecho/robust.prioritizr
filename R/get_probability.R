#' @include internal.R
NULL

#' Get probability of holding the constraint
#'
#' Get the probability of holding the constraint from a [prioritizr::problem()] object.
#'
#' @param x [prioritizr::problem()] object.
#'
#'
#' @details
#' This function is used internally by the robust objective functions
#' to extract the probability of holding constraints from a [prioritizr::problem()]
#' based on the robust constraints.
#'
#' @seealso
#' The [add_robust_constraints()] function is used to specify feature
#' groupings.
#'
#' @return An invisible `logical` value indicating success.
#'
#' @noRd
get_probability <- function(x) {
  assert_required(x)
  assert(
    is_conservation_problem(x)
  )
  assert(has_robust_constraints(x))
  feature_groupings <- get_feature_groupings(x)

  # If it is a scalar, then return the same number with the length of the feature groupings
  i <- which(vapply(x$constraints, inherits, logical(1), "RobustConstraint"))
  probability <- x$constraints[[i]]$get_data("probability")

  # TODO: additional assertions to check the validity

  if (length(probability) == 1) {
    return(rep(probability, length(feature_groupings)))
  }
}
