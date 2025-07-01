#' @include internal.R
NULL

#' Add robust minimum shortfall objective
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
#' @name add_robust_min_shortfall_objective
NULL

#' @rdname add_robust_min_shortfall_objective
#' @export
add_robust_min_shortfall_objective <- function(x, budget) {
  # assert arguments are valid
  assert_required(x)
  assert_required(budget)
  assert(
    is_conservation_problem(x),
    is.numeric(budget),
    all_finite(budget),
    all_positive(budget),
    is_budget_length(x, budget)
  )
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "RobustMinimumShortfallObjective",
      inherit = Objective,
      public = list(
        name = "robust minimum shortfall objective",
        data = list(budget = budget),
        default_weights = function(x) {
          assert(
            inherits(x, "ConservationProblem"),
            .internal = TRUE
          )
          w <- 1 / x$feature_targets()$value
          w[!is.finite(w)] <- 0 # replace 1/0 = Inf, with zeros
          w
        },
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
          assertthat::assert_that(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem")
          )
          # get feature groupings
          feature_groupings <- get_feature_groupings(y)
          # apply objective
          invisible(
            rcpp_apply_min_shortfall_objective(
              x$ptr,
              y$feature_targets(),
              y$planning_unit_costs(),
              self$get_data("budget"),
              feature_groupings
            )
          )
        }
      )
    )$new()
  )
}
