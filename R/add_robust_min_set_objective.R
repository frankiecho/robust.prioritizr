#' @include internal.R
NULL

#' Add robust minimum set objective
#'
#' TODO.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @details
#' TODO.
#'
#' @section Mathematical formulation:
#' TODO.
#'
#' @references
#' TODO.
#'
#' @seealso
#' See [robust_objectives] for an overview of all functions for adding
#' robust objectives.
#'
#' @family robust_objectives
#'
#' @return An updated [prioritizr::problem()] object with the objective added
#' to it.
#'
#' @examples
#' \dontrun{
#' TODO.
#' }
#'
#' @name add_robust_min_set_objective
NULL

#' @rdname add_robust_min_set_objective
#' @export
add_robust_min_set_objective <- function(x) {
  # assert argument is valid
  assert_required(x)
  assert(is_conservation_problem(x))
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "RobustMinimumSetObjective",
      inherit = prioritizr::Objective,
      public = list(
        name = "robust minimum set objective",
        calculate = function(x) {
          # assert argument is valid
          assert(is_conservation_problem(x), .internal = TRUE)
          # assert that robust constraints are specified correctly
          assert(has_robust_constraints(x))
          # return success
          invisible(TRUE)
        },
        apply = function(x, y) {
          # assert valid arguments
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # get feature groupings
          feature_groupings <- get_feature_groupings(y)
          # apply the objective
          invisible(
            rcpp_apply_robust_min_set_objective(
              x$ptr,
              y$feature_targets(),
              y$planning_unit_costs(),
              feature_groupings
            )
          )
        }
      )
    )$new()
  )
}
