#' @include internal.R
NULL

#' Add robust minimum shortfall objective
#'
#' TODO.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @param budget `numeric` value specifying the maximum expenditure of
#'   the prioritization. For problems with multiple zones, the argument
#'   to `budget` can be (i) a single `numeric` value to specify a single budget
#'   for the entire solution or (ii) a `numeric` vector to specify
#'   a separate budget for each management zone.
#'
#' @details
#' TODO.
#'
#' @section Mathematical formulation:
#' This objective can be expressed mathematically for a set of planning units
#' (\eqn{I}{I} indexed by \eqn{i}{i}), a set of features (\eqn{J}{J} indexed
#' by \eqn{j}{j}), and a set of realizations (\eqn{K}{K} indexed by \eqn{k}{k}) as:
#'
#' \deqn{\mathit{Minimize} \space  \sum_{j = 1}^{J} w_j \frac{y_j}{t_j} \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ijk} + v_{jk} \geq t_j \forall j \in J, k \in K \\
#' \sum_{i = 1}^{I} x_i c_i \leq B \\
#' y_j \geq v_{jk} \forall k \in K}{
#' Minimize sum_j^J wj * (yj / tj) subject to
#' sum_i^I (xi * rij) + yj >= tj for all j in J &
#' sum_i^I (xi * ci) <= B
#' }
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ijk}}{rijk} is the amount of feature \eqn{j}{j} in planning
#' unit \eqn{i}{i} in realization \eqn{k}{k}, \eqn{t_j}{tj} is the representation target for feature
#' \eqn{j}{j}, \eqn{y_j}{yj} denotes the robust representation shortfall for
#' the target \eqn{t_j}{tj} for feature \eqn{j}{j} across all realizations \eqn{k}{k},
#' \eqn{v_{jk}}{vjk} is the shortfall for feature \eqn{j}{j} under realization \eqn{k}{k}, and \eqn{w_j}{wj} is the
#' weight for feature \eqn{j}{j} (defaults to 1 for all features; see
#' [add_feature_weights()] to specify weights). Additionally,
#' \eqn{B}{B} is the budget allocated for the solution, \eqn{c_i}{ci} is the
#' cost of planning unit \eqn{i}{i}. Note that \eqn{y_j}{yj} is a continuous
#' variable bounded between zero and infinity, and denotes the shortfall
#' for target \eqn{j}{j}.
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
          d <- get_feature_group_data(y)
          # determine if probability constraints are needed
          is_prob_needed <- any(d$confidence_level != 1)
          # TODO: additional checks to see whether or not probability constraints are really needed
          # apply the objective

          invisible(
            rcpp_apply_robust_min_shortfall_objective(
              x$ptr,
              y$feature_targets(),
              y$planning_unit_costs(),
              self$get_data("budget"),
              d$ids
            )
          )
          if (isTRUE(is_prob_needed)) {
            invisible(
              rcpp_apply_robust_probability_constraints(
                x$ptr,
                y$feature_targets(),
                d$ids,
                d$confidence_level
              )
            )
          }
        }
      )
    )$new()
  )
}
