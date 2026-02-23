#' @include internal.R
NULL

#' Has robust constraints?
#'
#' Check if a [prioritizr::problem()] has robust constraints.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @return An invisible `logical` value indicating success.
#'
#' @examples
#' # Get planning unit data
#' pu <- get_sim_pu_raster()
#'
#' # Get feature data
#' features <- get_sim_features()
#'
#' # Define the feature groups,
#' # Here, we will assign the first 2 features to the group A, and
#' # the remaining features to the group B
#' groups <- c(rep("A", 2), rep("B", nlyr(features) - 2))
#'
#' # Build problem
#' p <-
#'   problem(pu, features) |>
#'   add_robust_min_set_objective() |>
#'   add_constant_robust_constraints(groups = groups, conf_level = 0.9) |>
#'   add_relative_targets(0.1) |>
#'   add_binary_decisions() |>
#'   add_default_solver(verbose = FALSE)
#' 
#' robust_constraints <- has_robust_constraints(p)
#' print(robust_constraints)
#' 
has_robust_constraints <- function(x) {
  assert(is_conservation_problem(x), .internal = TRUE)
  i <- which(vapply(x$constraints, inherits, logical(1), "RobustConstraint"))
  identical(length(i), 1L)
}

assertthat::on_failure(has_robust_constraints) <- function(call, env) {
  x  <- eval(call$x, envir = env)
  i <- which(vapply(x$constraints, inherits, logical(1), "RobustConstraint"))
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
