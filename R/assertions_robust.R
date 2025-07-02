#' @include internal.R
NULL

#' Has robust constraints
#'
#' Check if a [prioritizr::problem()] has robust constraints.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @return An invisible `logical` value indicating success.
#'
# @noRd
has_robust_constraints <- function(x) {
  assert(is_conservation_problem(x), .internal = TRUE)
  i <- which(vapply(x$constraints, inherits, logical(1), "RobustConstraint"))
  identical(length(i), 1L)
}

assertthat::on_failure(has_robust_constraints) <- function(call, env) {
  i <- which(
    vapply(env$x$constraints, inherits, logical(1), "RobustConstraint")
  )
  msg <- "Unknown."
  if (identical(length(i), 0L)) {
    msg <- c(
      "!" = "{.arg x} must have robust constraints.",
      "i" = "Use {.fn add_robust_constraints} to specify robust constraints."
    )
  } else {
    msg <- c("!" = "{.arg x} must not have multiple robust constraints.")
  }
  msg
}
