#' Add robust minimum set objective
#'
#' Set the objective of a conservation planning problem to minimize the cost of the solution while ensuring that all targets are met robustly across all realizations of data.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @details
#' The robust minimum set objective seeks to find the set of planning units at a minimum cost such that the targets are met across all realizations of data.
#'
#' @section Mathematical formulation:
#' This objective can be expressed
#' mathematically for a set of planning units (\eqn{I}{I} indexed by
#' \eqn{i}{i}), a set of features (\eqn{J}{J} indexed by \eqn{j}{j}), and
#' a set of realizations (\eqn{K} indexed by \eqn{k}) as:
#'
#' \deqn{\mathit{Minimize} \space \sum_{i = 1}^{I} x_i c_i \\
#' \mathit{subject \space to} \\
#' \Pr_k \{ \sum_{i = 1}^{I} x_i r_{ijk} \geq T_{j} \} \geq \alpha \space \forall \space j \in J}{Minimize sum_i^I (xi * ci) subject to sum_i^I (xi * rij) >= Tj for all
#' j in J}
#'
#' Here, \eqn{x_i}{xi} is the [decisions] variable (e.g.,
#' specifying whether planning unit \eqn{i}{i} has been selected (1) or not
#' (0)), \eqn{c_i}{ci} is the cost of planning unit \eqn{i}{i},
#' \eqn{r_{ijk}}{rijk} is the amount of feature \eqn{j}{j} in planning unit
#' \eqn{i}{i} under realization \eqn{k}, and \eqn{T_{j}}{Tj} is the target for feature \eqn{j}{j}
#' under realization \eqn{k}{k}. The probabilistic constraint ensures that the proportion of constraints that
#' are held are greater than the confidence level `conf_level` specified in `add_*_robust_constraints`.
#' In other words, if \eqn{\alpha=1}{\alpha=1}, then the constraint is held across all of \eqn{k}{k}. The
#' first term is the objective function and the second is the set of
#' constraints. In words this says find the set of planning units that meets
#' all the representation targets across all realizations while minimizing the overall cost.
#'
#' If \eqn{\alpha}{\alpha} is 1, then the probabilistic constraint simplifies to
#' the following constraint:
#' \deqn{
#' \sum_{i = 1}^{I} x_i r_{ijk} \geq T_{j} \quad \forall \space j \in J, \space k \in K
#' }{}
#'
#' If \eqn{\alpha}{\alpha} is less than 1, the probabilistic constraint is formulated using either a Chance Constraint
#' Programming approach (`method = "Chance"`), or a Conditional Value-at-Risk
#' `method = 'CondValueAtRisk'` approach.
#'
#' For the Chance Constraint approach, the probabilistic constraint is parameterised using a "big-M" formulation,
#' which replaces the constraint as follows:
#' \deqn{
#' \sum_{i = 1}^{I} x_i r_{ijk} + T_j m_{jk} \geq T_{j} \quad \forall \space j \in J, \space k \in K \\
#' \sum_{k = 1}^{K_j} m_{jk} / K_j \leq 1 - \alpha \quad \forall \space j \in J\\
#' m \in \{0, 1\}
#' }
#'
#' where \eqn{m}{m} is a binary discrete variables indicating whether the constraint has been held or not, and
#' \eqn{K_j}{K_j} is the number of realizations for the feature \eqn{j}{j}. This formulation effectively
#' uses the variable \eqn{m}{m} to allow for the constraint to be violated, and
#' counts the number of times the constraint has been violated. The proportion of constraint violations that
#' count is then constrained to be less than \eqn{1 - \alpha}{1 - \alpha}. This
#' allows for a more intuitive interpretation of the confidence level parameter, but
#' adds \eqn{J \times K} number of binary variables to the problem can significantly increase solve times.
#' See Charnes and Cooper (1959) for more details about this original formulation.
#'
#' In contrast, the Conditional Value-at-Risk constraint is a tighter formulation than the Chance Constraint,
#' as in, the proportion of constraints that are held will usually be much higher than \eqn{\alpha}{\alpha}.
#' It avoids having to add discrete binary variables to the problem and can sometimes yield
#' lower solve times. It replaces the probabilistic constraint with the constraints below:
#' \deqn{
#' \sum_{i = 1}^{I} x_i r_{ijk} - \eta_j + s_{jk} \geq 0 \quad \forall \space j \in J, \space k \in K \\
#' \eta_j - \frac{1}{(1-\alpha) K_j} \sum_{k=1}^{K_j} s_{jk} \geq T_j \quad  \forall \space j \in J \\
#' s_{jk} \geq 0 \quad \forall \space j \in J, \space k \in K \\
#' \eta_j \in \mathbb{R}  \quad  \forall \space j \in J
#' }
#' where \eqn{\eta}{\eta} and \eqn{s}{s} are auxiliary variables introduced to represent the "tail" of the
#' distribution of the uncertain quantity \eqn{\sum_{i = 1}^{I} x_i r_{ijk}}. In other words, it ensures
#' that the average of the values of \eqn{\sum_{i = 1}^{I} x_i r_{ijk}} that fall below the \eqn{(1-\alpha)}{1-\alpha}
#' quantile of the distribution is greater than \eqn{T_j}{T_j}. Using this constraint preserves the
#' convexity of the problem (if other constraints were convex) and adds \eqn{J\times K+J}{J\times K+J} number of continuous
#' variables to the problem. Though some users find it complicates the interpretation of
#' the `conf_level` parameter. See Rockafellar and Uryasev (2000) for more details about this formulation.
#'
#' @references
#' Charnes, A., & Cooper, W. W. (1959). Chance-Constrained Programming. Management Science, 6(1), 73-79.
#'
#' Rockafellar, R. T., & Uryasev, S. (2000). Optimization of Conditional Value-at-Risk. Journal of Risk, 2(3), 21-42.
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
#' # load packages
#' library(prioritizr)
#' library(terra)
#'
#' # create dummy data
#' # planning units
#' pu <- rast(matrix(1, 10, 10))
#'
#' # 2 features with 3 scenarios each
#' features <- c(
#'   rast(matrix(rnorm(100, 1, 1), 10, 10)),
#'   rast(matrix(rnorm(100, 2, 1), 10, 10)),
#'   rast(matrix(rnorm(100, 3, 1), 10, 10)),
#'   rast(matrix(rnorm(100, 4, 1), 10, 10)),
#'   rast(matrix(rnorm(100, 5, 1), 10, 10)),
#'   rast(matrix(rnorm(100, 6, 1), 10, 10))
#' )
#' names(features) <- paste0("feature_", rep(1:2, each = 3), "_scenario_", 1:3)
#'
#' # define groups for robust constraints
#' # each feature has 3 scenarios
#' groups <- rep(paste0("feature_", 1:2), each = 3)
#'
#' # create problem with robust minimum set objective
#' p <- problem(pu, features) %>%
#'   add_robust_min_set_objective() %>%
#'   add_absolute_targets(2) %>%
#'   add_constant_robust_constraints(groups = groups)
#'
#' # print problem
#' print(p)
#' }
#'
#' @name add_robust_min_set_objective
NULL

#' @rdname add_robust_min_set_objective
#' @export
add_robust_min_set_objective <- function(x, method = "Chance") {
  # assert argument is valid
  assert_required(x)
  assert(is_conservation_problem(x))
  assert(assertthat::noNA(method))
  assert(assertthat::is.string(method))
  allowed_methods <- c("CondValueAtRisk", "Chance")
  assert(
    isTRUE(method %in% c("CondValueAtRisk", "Chance")),
    msg = "`method` must be either \"CondValueAtRisk\" or \"Chance\"."
  )
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
          is_prob_needed <- any(d$conf_level < 1)
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
            if (identical(method, "CondValueAtRisk")) {
              invisible(
                rcpp_apply_robust_cvar_constraints(
                  x$ptr,
                  y$feature_targets(),
                  d$ids,
                  d$conf_level
                )
              )
            } else {
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
        }
      )
    )$new()
  )
}
