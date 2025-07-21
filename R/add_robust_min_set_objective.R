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
#' This objective can be expressed
#' mathematically for a set of planning units (\eqn{I}{I} indexed by
#' \eqn{i}{i}), a set of features (\eqn{J}{J} indexed by \eqn{j}{j}), and
#' a set of realizations (\eqn{K} indexed by \eqn{k}) as:
#'
#' \deqn{\mathit{Minimize} \space \sum_{i = 1}^{I} x_i c_i \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i r_{ijk} \geq \max_k (T_{jk}) \space \forall \space j \in J, \space k \in K}{
#' Minimize sum_i^I (xi * ci) subject to sum_i^I (xi * rij) >= Tj for all
#' j in J}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i},
#' \eqn{r_{ijk}}{rijk} is the amount of feature \eqn{j}{j} in planning unit
#' \eqn{i}{i} under realization \eqn{k}, and \eqn{T_{jk}}{Tj} is the target for feature \eqn{j}{j}
#' under realization \eqn{k}{k}. The
#' first term is the objective function and the second is the set of
#' constraints. In words this says find the set of planning units that meets
#' all the representation targets across all realizations while minimizing the overall cost.
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
          # get feature grouping data
          d <- get_feature_group_data(y)
          # determine if probability constraints are needed
          is_prob_needed <- any(d$confidence_level != 1)
          # TODO: additional checks to see whether or not probability constraints are really needed
          # apply objective
          invisible(
            rcpp_apply_robust_min_set_objective(
              x$ptr,
              y$feature_targets(),
              y$planning_unit_costs(),
              d$ids
            )
          )
          if (isTRUE(is_prob_needed)) {
            invisible(
              rcpp_apply_robust_probability_constraints(
                x$ptr,
                y$feature_targets(),
                feature_groupings,
                d$confidence_level
              )
            )
          }
        }
      )
    )$new()
  )
}
