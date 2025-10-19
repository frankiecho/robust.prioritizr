#' @include internal.R
NULL

#' Add variable robust constraints
#'
#' Add robust constraints to a conservation problem to allow
#' the solution's level of robustness to uncertainty to differ across
#' feature groups.
#' For example, this function may be especially
#' useful when it is important to ensure that a prioritization is
#' highly robust to uncertainty in the spatial distribution of threatened
#' species, and only moderately robust to uncertainty in the spatial
#' distribution of widespread species.
#'
#' @inheritParams add_robust_min_set_objective
#'
#' @param data [tibble::tibble()] data frame containing information on the
#' feature groups and confidence level associated with each group.
#'  Defaults to 1, corresponding to a maximally robust solution.
#  See the Data format section for further information on this parameter.
#'
#' @inherit add_constant_robust_constraints details
#'
#' @section Data format:
#' The `data` argument must be a [tibble::tibble()] data frame that has
#' information on the feature groups and their confidence levels.
#' Here, each row corresponds to a different feature group and
#' columns contain information about the groups.
#' In particular, `data` must have the following columns.
#' \describe{
#' \item{features}{
#' A `list` column with the names of the features that belong to each group.
#' In particular, if a particular set of features should belong to the same
#' group, then they should be stored in the same element of this column.
#' }
#' \item{conf_level}{
#' A `numeric` column with values that describe the confidence level
#' associated with each feature group (ranging between 0 and 1).
#' See the Details section for information on `conf_level` values.
#' }
#' }
#'
#' @inheritSection add_constant_robust_constraints Data requirements
#'
#'
#' @inherit add_constant_robust_constraints return seealso
#'
#' @family constraints
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(prioritizr)
#' library(terra)
#' library(tibble)
#'
#' # Get planning unit data
#' pu <- get_sim_pu_raster()
#'
#' # Get feature data
#' features <- get_sim_features()
#'
#' # Define the feature group data
#' # Here, we will assign the first 2 features to the group A, and the
#' # remaining features to the group B
#' groups <- c(rep("A", 2), rep("B", nlyr(features) - 2))
#'
#' # Next, we will use this information to create a data frame containing
#' # the feature groups and specifying a confidence level of 0.95 for group A,
#' # and a confidence level of 0.5 for group B
#' constraint_data <- tibble(
#'   features = split(names(features), groups),
#'   conf_level = c(0.95, 0.5)
#' )
#'
#' # Display constraint data
#' print(constraint_data)
#'
#' # Build problem
#' p <-
#'   problem(pu, features) |>
#'   add_robust_min_set_objective() |>
#'   add_variable_robust_constraints(data = constraint_data) |>
#'   add_relative_targets(0.1) |>
#'   add_binary_decisions() |>
#'   add_default_solver(verbose = FALSE)
#'
#' # Solve the problem
#' soln <- solve(p)
#'
#' # Plot the solution
#' plot(soln)
#' }
#' @name add_variable_robust_constraints
NULL

#' @rdname add_variable_robust_constraints
#' @export
add_variable_robust_constraints <- function(x, data) {
  # assert arguments are valid
  assert_required(x)
  assert_required(data)
  assert(
    is_conservation_problem(x),
    is.data.frame(data),
    assertthat::has_name(data, "features"),
    assertthat::has_name(data, "conf_level"),
    is.list(data$features),
    is.numeric(data$conf_level),
    all_finite(data$conf_level),
    all(data$conf_level >= 0),
    all(data$conf_level <= 1)
  )

  # additional validation for feature groupings
  assert(
    all(vapply(data$features, is.character, logical(1))),
    msg = c(
      "!" = paste(
        "Each element in {.arg data$features} must contain a",
        "{.cls character} vector."
      )
    )
  )
  assert(
    all(
      unlist(data$features, recursive = TRUE, use.names = FALSE) %in%
      prioritizr::feature_names(x)
    ),
    msg = c(
      "!" = paste(
        "{.arg data$features} must contain only the names of features in",
        "{.arg x}."
      ),
      "i" = paste(
        "To see the features names in {.arg x},",
        "use {.code feature_names(x)}."
      )
    )
  )
  assert(
    all(lengths(data$features) >= 2),
    msg = c(
      "!" = paste(
        "Each group in {.arg data$features} must have at least 2",
        "feature names."
      )
    )
  )



  # add constraints
  x$add_constraint(
    R6::R6Class(
      "RobustConstraint",
      inherit = Constraint,
      public = list(
        name = ifelse(
          identical(length(unique(data$conf_level)), 1L),
          "constant robust constraints",
          "variable robust constraints"
        ),
        data = list(data = data),
        apply = function(x, y) {
          # note that these constraints are just used as a dummy place holder
          # to store the feature grouping information, and so the $apply()
          # method does not actually do anything
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # return success
          invisible(TRUE)
        }
      )
    )$new()
  )
}
