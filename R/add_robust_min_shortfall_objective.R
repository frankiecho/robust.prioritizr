#' Add robust minimum shortfall objective
#'
#' Add an objective to a conservation planning problem that minimizes the
#' representation shortfalls for each feature group in a manner that is robust
#' to uncertainty, whilst ensuring that the total cost of the solution
#' does not exceed a budget.
#'
#' @inheritParams add_robust_min_set_objective
#'
#' @param budget `numeric` value specifying the maximum expenditure of
#' the prioritization. For problems with multiple zones, the argument
#' to `budget` can be (i) a single `numeric` value to specify a single budget
#' for the entire solution or (ii) a `numeric` vector to specify
#' a separate budget for each management zone.
#'
#' @details
#' The robust minimum shortfall objective seeks to find the set of planning
#' units that minimizes the representation shortfall for each feature group,
#' subject to a budget.
#' In particular, a target shortfall reflects difference
#' between the target for a feature and the amount held by a candidate solution,
#' expressed as a proportion of the target.
#' These target shortfalls are then calculated for each of the features
#' associated with a feature group, and a representation shortfall is used
#' describe how well all the features associated with a particular feature
#' group are represented by a candidate solution.
#' Thus this objective aims to get as close as possible to
#' reducing the representation shortfalls shortfalls to zero,
#' by getting as close as possible to reaching all of the targets
#' for the features associated with each of the feature groups.
#' Since the probabilistic calculations that underpin this objective are
#' non-linear, the chance constraint programming method
#' (Charnes and Cooper 1959) is used to formulate the optimization problem as a
#' mixed integer linear programming problem. With this method, the
#' confidence level parameter (i.e.,
#' specified per `conf_level` with [add_constant_robust_constraints()] or
#' [add_variable_robust_constraints()]) describes the quantile
#' of the target shortfalls associated with the feature group
#' that should be minimized during optimization. For example,
#' if `conf_level = 1` for a feature group, then the 100th quantile is used and
#' this means that -- after calculating the target shortfalls for each
#' feature associated with the feature group -- the largest target
#' shortfall for the associated features is used to calculate the
#' representation shortfall for the feature group.
#' Additionally, if `conf_level = 0.5` for a feature group, then the
#' 50th quantile is used and this means that the median target shortfall
#' for the features associated with the group is used to represent the
#' representation shortfall for the feature group.
#'
#' @section Mathematical formulation:
#' This objective can be expressed
#' mathematically for a set of planning units (\eqn{I}{I} indexed by
#' \eqn{i}{i}), a set of feature groups (\eqn{J}{J} indexed by \eqn{j}{j}), and
#' a set of features associated with each feature group
#' (\eqn{K} indexed by \eqn{k}). Let \eqn{c_i}{ci} denote the cost of
#' planning unit \eqn{i}{i}, \eqn{r_{ijk}}{rijk} the amount of feature
#' \eqn{k}{k} associated with planning unit \eqn{i}{i} for feature group
#' \eqn{j}{j}, \eqn{T_j}{Tj} the target for each feature group
#' \eqn{j}{j}, \eqn{w_j}{wj} the weight for each feature group
#' \eqn{j}{j}, and \eqn{\alpha}{a} the confidence level for uncertainty
#' (specified per `conf_level` with [add_constant_robust_constraints()] or
#' [add_variable_robust_constraints()]).
#' Additionally, to describe the decision variables,
#' let \eqn{x_i}{xi} denote the status of the planning unit \eqn{i}{i}
#' (e.g., specifying whether
#' planning unit \eqn{i}{i} has been selected or not with binary values),
#' \eqn{v_{jk}}{vjk} the target shortfall for each feature \eqn{k}{k}
#' associated with each feature group \eqn{j}{j},
#' and \eqn{y_j}{yj} the representation shortfall for
#' for each feature group \eqn{j}{j}.
#' Given this terminology, the robust minimum shortfall formulation of the
#' reserve selection problem is formulated as follows.
#'
#' \deqn{
#' \mathit{Minimize} \space \sum_{j = 1}^{J} w_j \times y_j \\
#' \mathit{subject \space to} \\
#' \sum_{i = 1}^{I} x_i c_i \leq B \\
#' \Pr_ k\{\sum_{i = 1}^{I} ( x_i \times r_{ijk} ) +
#'   ( T_{j} \times v_{jk} ) \geq T_j \} \geq \alpha
#'   \quad \forall j \in J \\
#' y_j \geq v_{jk} \quad \forall j \in J, k \in K \\
#' 0 \leq y_j \leq 1 \quad \forall j \in J
#' }{
#' Minimize sum_j^J wj * yj
#' subject to sum_i^I (xi * ci) <= B
#' Pr_k \{ sum_i^I (xi * rij) + (tj * vjk) >= tj \} >= a, for all j in J
#' y_j >= vjk, for all j in J, k \in K
#' 0 <= y_j <= 1, for all j in J
#' }
#'
#' Here, the objective function (first equation) is to minimize the
#' weighted sum of the representation shortfalls for each feature group.
#' The budget constraints (second equation) ensure that the
#' solution does not exceed the budget.
#' The probabilistic constraints (third equation) specify that
#' only some of the target shortfall variables (i.e., \eqn{v_{jk}}{vjk})
#' associated with each feature group are used to calculate the representation
#' shortfall for each feature group, and the subset of target shortfall
#' variables that are used is based on the confidence level
#' (i.e., \eqn{\alpha}{a}).
#' For example, if \eqn{\alpha=1}{a=1}, then all of the target shortfall
#' variables associated with each feature group must be used for the
#' calculations.
#' Alternatively, if \eqn{\alpha=0.5}{a=0.5}, then only enough of the target
#' shortfall variables are required for the calculations to achieve a 50%
#' chance of correctly calculating the target shortfall variables for a
#' given feature group.
#' The representation shortfall constraints (fourth equation) ensure
#' that the representation shortfall variable for each feature group must
#' be greater than or equal to the target shortfall variables of the
#' features associated with the feature group.
#' In combination with the other constraints, this means that
#' the representation shortfall variable for a given feature group
#' is calculated as the largest value of a subset of the
#' target shortfall variables for the features associated with the feature
#' group, and this particular subset is based on the confidence level.
#' Thus if \eqn{\alpha}{a} is closer to a value of 1, then the representation
#' shortfall variable for each feature group is calculated with
#' a greater degree of certainty and, in turn, the optimization process
#' seeks a solution that is more robust to uncertainty.
#' Since the probabilistic constraints are non-linear, an approximation method
#' is used to linearize them so that the optimization problem can be solved
#' with mixed integer programming exact algorithm solvers.
#'
#' The chance constraint programming method is used to linearize the
#' probabilistic constraints (Charnes and Cooper 1959). To describe this
#' method, let \eqn{m_{jk}}{mjk} denote a binary
#' auxiliary variable for each feature \eqn{k}{k} associated with
#' feature group \eqn{j}{j}. Also \eqn{K_j}{Kj} denote a
#' pre-computed value describing the number of
#' features associated with each feature group \eqn{j}{j}.
#' Given this terminology, the method involves replacing the probabilistic
#' constraints with the following linear constraints.
#'
#' \deqn{
#' \sum_{i = 1}^{I} x_i r_{ijk} + T_j y_j + T_j m_{jk} \geq T_{j} \quad
#'   \forall \space j \in J, \space k \in K \\
#' \sum_{k = 1}^{K_j} \frac{m_{jk}}{K_j} \leq 1 - \alpha \quad
#'   \forall \space j \in J\\
#' m_{jk} \in \{0, 1\}
#' }{
#' sum_i^I xi rijk + Tj yj + Tj mjk >= Tj, for all j in J, k in K
#' sum_k^Kj mjk / Kj <= 1 - a, for all j in J
#' mjk in \{0, 1\}
#' }
#'
#' Here, the solution calculates the representation shortfall variable
#' for a given feature group based on a particular subset of the
#' target shortfalls for the associated features. Specifically, this
#' subset based on a particular number of the smallest target shortfall
#' variables based on \eqn{\alpha}{a}. For example, if a feature group is
#' associated with 30 features and \eqn{\alpha=0.3}{a=0.3}, then the
#' representation shortfall for the feature group is calculated by identifying
#' which 10 of these 30 features have the smallest target shortfall variables,
#' and then calculating the maximum value of these 10 target shortfall
#' variables. As such, the chance constraint programming method provides
#' an intuitive approximation of the probabilistic constraints.
#'
#' @references
#' Charnes A & Cooper WW (1959) Chance-constrained programming.
#' *Management Science*, 6(1), 73--79.
#'
#' @inherit add_robust_min_set_objective seealso return
#'
#' @family objectives
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(prioritizr)
#' library(terra)
#'
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
#' # Build problem with budget calculated as 30% total cost
#' p <-
#'   problem(pu, features) %>%
#'   add_robust_min_shortfall_objective(
#'     budget = terra::global(pu, "sum", na.rm = TRUE)[[1]] * 0.3
#'   ) %>%
#'   add_constant_robust_constraints(groups = groups, conf_level = 0.4) %>%
#'   add_binary_decisions() %>%
#'   add_relative_targets(0.3) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # Solve the problem
#' soln <- solve(p)
#'
#' # Plot the solution
#' plot(soln)
#' }
#'
#' @name add_robust_min_shortfall_objective
#' @export
add_robust_min_shortfall_objective <- function(x, budget) {
  # assert arguments are valid
  assert_required(x)
  assert_required(budget)
  assert(
    is_conservation_problem(x),
    is.numeric(budget),
    all_finite(budget),
    all(budget >= 0),
    is_budget_length(x, budget)
  )
  # add objective to problem
  x$add_objective(
    R6::R6Class(
      "RobustMinimumShortfallObjective",
      inherit = prioritizr::Objective,
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
