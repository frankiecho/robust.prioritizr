#' Add robust minimum shortfall objective
#'
#' Solves a minimum shortfall objective that minimises a robust shortfall objective across all features. For each feature, the shortfall objective is quantified based on the distribution of shortfall across all realizations of data. For a fully robust solution, the shortfall metric used for optimization is the maximum shortfall across all realizations of the data. For a partially robust solution (`conf_level` < 1), the solution guarantees that the probability that the shortfall is greater than the shortfall metric used for optimisation is less than or equals to 1 - `conf_level`.
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
#' by \eqn{j}{j}), and a set of realizations (\eqn{K}{K} indexed
#' by \eqn{k}{k}) as:
#'
#' \deqn{\mathit{Minimize} \space  \sum_{j = 1}^{J} w_j y_j \\
#' \mathit{subject \space to} \\
#' \Pr_k\{\sum_{i = 1}^{I} x_i r_{ijk} + T_{j} y_j \geq T_j\} > \alpha \qquad \forall j \in J  \\
#' \sum_{i = 1}^{I} x_i c_i \leq B \\
#' y_j \geq v_{jk} \qquad \forall k \in K}{'
#' Minimize sum_j^J wj * yj subject to
#' sum_i^I (xi * rij) + tj * yj >= tj for all j in J &
#' sum_i^I (xi * ci) <= B
#' }
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{r_{ijk}}{rijk} is the amount of feature \eqn{j}{j} in planning
#' unit \eqn{i}{i} in realization \eqn{k}{k}, \eqn{t_j}{tj} is the maximum representation target for feature
#' \eqn{j}{j} across all realizations \eqn{k}{k}, i.e., \eqn{T_j = \max_k(t_{jk})},
#' \eqn{y_j}{yj} denotes the robust representation shortfall for
#' the target \eqn{t_j}{tj} for feature \eqn{j}{j} across all realizations \eqn{k}{k},
#' \eqn{v_{jk}}{vjk} is the shortfall for feature \eqn{j}{j} under realization \eqn{k}{k}, and \eqn{w_j}{wj} is the
#' weight for feature \eqn{j}{j} (defaults to 1 for all features; see
#' [add_feature_weights()] to specify weights). \eqn{\alpha}{\alpha} is the
#' specified `conf_level` confidence level for the uncertain constraint
#' (as specified in `add_*_robust_constraints`), and ensures that the proportion of
#' constraints for each feature group \eqn{k}{k} that are held is higher than the
#' specified confidence level. Additionally,
#' \eqn{B}{B} is the budget allocated for the solution, \eqn{c_i}{ci} is the
#' cost of planning unit \eqn{i}{i}. Note that \eqn{y_j}{yj} is a continuous
#' variable bounded between zero and one, and denotes the shortfall
#' for target \eqn{j}{j} as a proportion of the total target.
#'
#' If `conf_level = 1` (default), then the probabilistic constraint is simply represented
#' as:
#' \deqn{
#' \sum_{i = 1}^{I} x_i r_{ijk} + T_{j} y_j \geq T_j  \quad \forall \space j \in J, \space k \in K
#' }
#'
#' If `conf_level < 1`, the probabilistic constraint is parameterised using a Chance Constraint Programming
#' approach by reformulating the constraint into the following series of constraints:
#' \deqn{
#' \sum_{i = 1}^{I} x_i r_{ijk} + T_j y_j + T_j m_{jk} \geq T_{j} \quad \forall \space j \in J, \space k \in K \\
#' \sum_{k = 1}^{K_j} m_{jk} / K_j \leq 1 - \alpha \quad \forall \space j \in J\\
#' m \in \{0, 1\}
#' }
#'
#' where \eqn{m}{m} is a binary discrete variable indicating whether the constraint has been held or not, and
#' \eqn{K_j}{K_j} is the number of realizations for the feature \eqn{j}{j}. The auxiliary variable
#' \eqn{m}{m} is essentially a count of the number of times the probabilistic constraint
#' has been violated (if \eqn{m_{jk}=1}{m_{jk}=1}), and the problem ensures that the
#' total amount of times these are violated don't exceed \eqn{1-\alpha}{1-\alpha}.
#'
#' The "Conditional Value-at-Risk" method found in `add_robust_min_set_objective` has not yet
#' been implemented in this objective function.
#'
#' @references
#' Charnes, A., & Cooper, W. W. (1959). Chance-Constrained Programming. Management Science, 6(1), 73-79.
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
# Get planning unit data
#' pu <- get_sim_pu_raster()
#'
#' # Get feature data
#' features <- replicate(2, get_sim_features())
#' features <- rast(features)
#' names(features) <- paste0("feature_", rep(1:5, 2),
#' "_scenario_", rep(1:2, each = 5))
#' relative_budget <- as.numeric(global(pu, 'sum', na.rm = T)) * 0.1
#'
#' # Get the groups
#' groups <- rep(paste0("feature_", 1:5), 2)
#'
#' # Set up prioritizr problem
#' p <- problem(pu, features) %>%
#'   add_constant_robust_constraints(groups = groups) %>%
#'   add_robust_min_shortfall_objective(budget = relative_budget) %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver()
#'
#' # Solve the problem
#' soln <- solve(p)
#'
#' # Plot the solution
#' plot(soln)
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
          is_prob_needed <- any(d$conf_level != 1)
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
                d$conf_level
              )
            )
          }
        }
      )
    )$new()
  )
}
