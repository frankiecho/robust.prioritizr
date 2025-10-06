#' Add robust minimum set objective
#'
#' Add an objective to a conservation planning problem that minimizes the cost
#' of the solution while ensuring that the targets for each feature group are
#' met in a manner that is robust to uncertainty.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @param method `character` value with the name of the probabilistic
#' constraint formulation method. Available options include the (`"chance"`)
#' chance constraint programming method (Charnes and Cooper 1959) or (`"cvar"`)
#' or the conditional value-at-risk method (Rockafellar and Uryasev 2000),
#' Defaults to `"chance"`. See the Details section for further information
#' on these methods.
#'
#' @details
#' The robust minimum set objective seeks to find the set of planning units at
#' a minimum cost such that the targets are in a robust manner for each
#' feature group. Since the probabilistic calculations that underpin this
#' objective are non-linear, two methods are provided for formulating
#' the optimization problem as a mixed integer linear programming problem.
#' These methods are the chance constraint programming method
#' (`method = "chance"`) and conditional value-at-risk method
#' (`method = "cvar"`). In particular, the chance constraint programming
#' method is associated a more intuitive interpretation for the
#' confidence level parameter (i.e.,
#' specified per `conf_level` with [add_constant_robust_constraints()] or
#' [add_variable_robust_constraints()]). Whereas, the
#' conditional value-at-risk constraint method may yield faster solve times.
#' This is because the conditional value-at-risk constraint method
#' is capable of preserving the convexity of an optimization problem,
#' and uses continuous (instead of binary) auxiliary variables.
#' As such, the chance constraint programming method may be
#' more useful for facilitating stakeholder involvement, and the
#' conditional value-at-risk constraint method may be more useful for
#' large-scale planning exercises.
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
#' \eqn{j}{j}, and \eqn{\alpha}{a} the confidence level for uncertainty
#' (specified per `conf_level` with [add_constant_robust_constraints()] or
#' [add_variable_robust_constraints()]).
#' Additionally, to describe the decision variables,
#' let \eqn{x_i}{xi} denote the status of the planning unit \eqn{i}{i}
#' (e.g., specifying whether
#' planning unit \eqn{i}{i} has been selected or not with binary values).
#' Given this terminology, the robust minimum set formulation of the reserve
#' selection problem is formulated as follows.
#'
#' \deqn{
#' \mathit{Minimize} \space \sum_{i = 1}^{I} x_i c_i \\
#' \mathit{subject \space to} \\
#' \Pr_k \{ \sum_{i = 1}^{I} x_i r_{ijk} \geq T_j \}
#' \geq \alpha \quad \forall \space j \in J
#' }{
#' Minimize sum_i^I (xi * ci)
#' subject to Pr_k \{ sum_i^I  (xi * rij) >= Tj \} >= a, for all j in J
#' }
#'
#' Here, the objective function (first equation) is to minimize the total
#' cost of the solution. The probabilistic constraints (second equation)
#' specify that the solution must achieve a particular probability threshold
#' (based on \eqn{\alpha}{a}) for meeting the targets of the features
#' associated with each feature group.
#' For example, if \eqn{\alpha=1}{a=1}, then each and every target associated
#' with each feature group must be met.
#' Alternatively, if \eqn{\alpha=0.5}{a=0.5}, then the
#' solution must have a 50% chance of meeting the targets associated
#' with each feature group.
#' Since the probabilistic constraints are non-linear, approximation methods
#' are used to linearize them so that the optimization problem can be solved
#' with mixed integer programming exact algorithm solvers.
#'
#' The chance constraint programming method uses a "big-M" formulation
#' to linearize the probabilistic constraints (Charnes and Cooper 1959).
#' To describe this method, let \eqn{m_{jk}}{mjk} denote a binary
#' auxiliary variable for each feature \eqn{k}{k} associated with each
#' feature group \eqn{j}{j}. Also, let \eqn{K_j}{Kj} denote a
#' pre-computed value describing the number of
#' features associated with each feature group \eqn{j}{j}.
#' Given this terminology, the method involves replacing the probabilistic
#' constraints with the following linear constraints.
#'
#' \deqn{
#' \sum_{i = 1}^{I} x_i r_{ijk} + T_j m_{jk} \geq T_{j} \quad
#'   \forall \space j \in J, \space k \in K \\
#' \sum_{k = 1}^{K_j} \frac{m_{jk}}{K_j} \leq 1 - \alpha \quad
#'   \forall \space j \in J \\
#' m_{jk} \in \{0, 1\} \quad
#'   \forall \space j \in J, \space k \in K
#' }{
#' sum_i^I xi rijk + Tj mjk >= Tj, for all j in J, k in K
#' sum_{k = 1}^{Kj} mjk / Kj <= 1 - a, for all j in J
#' mjk in \{0, 1\}, for all j in J, k in K
#' }
#'
#' Here, the solution is allowed to fail to meet the targets
#' for the features, and the auxiliary variable \eqn{m_{jk}}{mjk} is
#' used to calculate the proportion of features that do not have their targets
#' met for each feature group. For a given feature group, the
#' proportion of features that do not have their target met is
#' constrained to be less than \eqn{1 - \alpha}{1 - a}.
#' This method allows for an intuitive interpretation of the confidence level
#' parameter. Yet this method also adds \eqn{J \times K}{J * K} binary
#' variables to the problem and, as such, may present long solve times.
#'
#' The conditional value-at-risk constraint method presents a tighter
#' formulation than the chance constraint programming method
#' (Rockafellar and Uryasev 2000).
#' As such, this method is able to better approximate the non-linear
#' probabilistic constraints and, in turn, tends to yield
#' solutions that are more robust to uncertainty than the chance
#' constraint programming method.
#' To describe this method, let \eqn{\eta_j}{ej} denote
#' a continuous auxiliary variable for each feature group \eqn{j}{j},
#' and \eqn{s_{jk}}{sjk} a continuous auxiliary variable for each feature
#' \eqn{k}{k} associated with each feature group \eqn{j}{j}.
#' Given this terminology, the method involves replacing the probabilistic
#' constraints with the following linear constraints.
#'
#' \deqn{
#' \sum_{i = 1}^{I} x_i r_{ijk} - \eta_j + s_{jk} \geq 0 \quad
#'   \forall \space j \in J, \space k \in K \\
#' \eta_j - \frac{1}{(1 - \alpha) \times K_j} \sum_{k=1}^{K_j} s_{jk} \geq T_j
#'   \quad \forall \space j \in J \\
#' s_{jk} \geq 0 \quad \forall \space j \in J, \space k \in K \\
#' \eta_j \in \mathbb{R}  \quad  \forall \space j \in J
#' }{
#' \sum_i^I xi rijk - ej + sjk >= 0, for all j in J, k in K
#' ej - (1 / ((1 - a) * K_j)) sum_k^Kj sjk >= Tj for all j in J
#' sjk >= 0, for all j in J, k in K
#' ej in R, forall j in J
#' }
#'
#' Here, the continuous auxiliary variables are used to represent the "tail" of
#' the distribution of the uncertain quantity
#' (i.e., \eqn{\sum_{i=1}^{I} x_i r_{ijk}}{sum_i^I xi rijk}).
#' In other words, it ensures
#' that the average of amount of each feature held by the solution for
#' a particular feature group that falls below a particular
#' quantile (i.e., \eqn{(1 - \alpha)}{1 - a}) is greater than the
#' target for the feature group (i.e., \eqn{T_j}{Tj}).
#' Although this method does not provide an easily intuitive interpretation of
#' the confidence level parameter, it only adds \eqn{J \times K + J}{J * K + J}
#' continuous variables to the problem.
#'
#' @references
#' Charnes A & Cooper WW (1959) Chance-constrained programming.
#' *Management Science*, 6(1), 73--79.
#'
#' Rockafellar RT & Uryasev S (2000) Optimization of conditional value-at-risk.
#' *Journal of Risk*, 2(3), 21--42.
#'
#' @seealso
#' See [robust_objectives] for an overview of all functions for adding
#' robust objectives.
#'
#' @family objectives
#'
#' @return
#' An updated [prioritizr::problem()] object with the objective added
#' to it.
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
#' # Build problem with chance constraint programming method
#' p <-
#'   problem(pu, features) |>
#'   add_robust_min_set_objective(method = "cvar") |>
#'   add_constant_robust_constraints(groups = groups, conf_level = 0.9) |>
#'   add_binary_decisions() |>
#'   add_relative_targets(0.1) |>
#'   add_default_solver(verbose = FALSE)
#'
#' # Solve the problem
#' soln <- solve(p)
#'
#' # Plot the solution
#' plot(soln)
#' }
#'
#' @name add_robust_min_set_objective
#' @export
add_robust_min_set_objective <- function(x, method = "chance") {
  # assert argument is valid
  assert_required(x)
  assert_required(method)
  assert(
    is_conservation_problem(x),
    assertthat::noNA(method),
    assertthat::is.string(method)
  )
  method <- tolower(method) # for reverse compatibility
  assert(
    isTRUE(method %in% c("cvar", "chance")),
    msg = "{.arg method} must be either {.val cvar} or {.val chance}."
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
          # apply objective
          invisible(
            rcpp_apply_robust_min_set_objective(
              x$ptr,
              y$feature_targets(),
              y$planning_unit_costs(),
              d$ids
            )
          )
          # apply constraints
          if (isTRUE(any(d$conf_level < 1))) {
            if (identical(method, "cvar")) {
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
