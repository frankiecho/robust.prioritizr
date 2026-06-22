#' Add robust minimum set objective
#'
#' Add an objective to a conservation planning problem that minimizes the cost
#' of the solution while ensuring that the solution is robust to uncertainty for
#' each feature group.
#'
#' @param x A `ConservationProblem` object (i.e., [prioritizr::problem()]).
#'
#' @param method `character` value with the name of the probabilistic
#' constraint formulation method. Available options include the (`"chance"`)
#' chance constraint programming method (Charnes and Cooper 1959) or (`"cvar"`)
#' or the conditional value-at-risk method (Rockafellar and Uryasev 2000),
#' Defaults to `"chance"`. See the Details section for further information
#' on these methods.
#'
#' @srrstats {G2.0, G2.0a} Function validates input types and lengths via
#'   assertthat assertions and roxygen parameter documentation.
#' @srrstats {G2.1, G2.1a} Type checking for method parameter is enforced via
#'   validation routines; x must be a ConservationProblem object.
#' @srrstats {G2.3, G2.3a} The method parameter is restricted to valid options
#'   ('chance', 'cvar') via explicit validation.
#'
#' @details
#' The robust minimum set objective seeks to find the set of planning units at
#' a minimum cost such that the solution meets the targets in a robust manner
#' for each feature group. Two methods are provided for formulating
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
#' preserves the convexity of an optimization problem,
#' and uses continuous (instead of binary) auxiliary variables.
#' Also note that the conditional value-at-risk constraint method may
#' produce an infeasible solution for problems that are feasible with
#' the chance constraint with the same `conf_level`. In such cases the chance
#' chance constraint programming method should be used instead.
#' As such, the chance constraint programming method may be
#' more useful for facilitating stakeholder involvement for small-scale
#' planning exercises, and the
#' conditional value-at-risk constraint method may be more useful for
#' large-scale applications.
#'
#' @section Mathematical formulation:
#' This objective can be expressed
#' mathematically for a set of planning units (\eqn{I}{I} indexed by
#' \eqn{i}{i}), a set of feature groups (\eqn{J}{J} indexed by \eqn{j}{j}), and
#' a set of features associated with each feature group
#' (\eqn{K} indexed by \eqn{k}). Let \eqn{c_i}{ci} denote the cost of
#' planning unit \eqn{i}{i}, \eqn{R_{ijk}}{Rijk} the amount of feature
#' \eqn{k}{k} associated with planning unit \eqn{i}{i} for feature group
#' \eqn{j}{j}, \eqn{T_{jk}}{Tjk} the target for each feature \eqn{k} in each
#' feature group \eqn{j},
#' and \eqn{\alpha}{a} the confidence level for uncertainty
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
#' \Pr_k \{ \sum_{i = 1}^{I} x_i R_{ijk} \geq T_j \}
#' \geq \alpha \quad \forall \space j \in J
#' }{
#' Minimize sum_i^I (xi * ci)
#' subject to Pr_k \{ sum_i^I  (xi * Rijk) >= Tj \} >= a, for all j in J
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
#' Approximation methods are used to linearize them so that the optimization
#' problem can be solved with mixed integer programming exact algorithm solvers.
#'
#' The chance constraint programming method uses a "big-M" formulation
#' to linearize the probabilistic constraints (Charnes and Cooper 1959).
#' To describe this method, let \eqn{M_{jk}}{Mjk} denote a binary
#' auxiliary variable for each feature \eqn{k}{k} associated with each
#' feature group \eqn{j}{j}. Also, let \eqn{K_j}{Kj} denote a
#' pre-computed value describing the number of
#' features associated with each feature group \eqn{j}{j}.
#' Given this terminology, the method involves replacing the probabilistic
#' constraints with the following linear constraints.
#'
#' \deqn{
#' \sum_{i = 1}^{I} (x_i \times R_{ijk}) + (T_{jk} \times M_{jk})
#'   \geq T_{jk} \quad \forall \space j \in J, \space k \in K \\
#' \sum_{k = 1}^{K_j} \frac{M_{jk}}{K_j} \leq 1 - \alpha \quad
#'   \forall \space j \in J \\
#' M_{jk} \in \{0, 1\} \quad
#'   \forall \space j \in J, \space k \in K
#' }{
#' sum_i^I xi Rijk + Tjk Mjk >= Tjk, for all j in J, k in K
#' sum_{k = 1}^{Kj} Mjk / Kj <= 1 - a, for all j in J
#' Mjk in \{0, 1\}, for all j in J, k in K
#' }
#'
#' Here, the solution is allowed to fail to meet the targets
#' for the features, and the auxiliary variable \eqn{M_{jk}}{Mjk} is
#' used to calculate the proportion of features that do not have their targets
#' met for each feature group. For a given feature group, the
#' proportion of features that do not have their target met is
#' constrained to be less than \eqn{1 - \alpha}{1 - a}.
#' This method allows for an intuitive interpretation of the confidence level
#' parameter. Yet this method also adds \eqn{J \times K}{J * K} binary
#' variables to the problem and, as such, may present long solve times for
#' problems with many other decision variables and constraints.
#'
#' The conditional value-at-risk constraint method presents a tighter
#' formulation than the chance constraint programming method
#' (Rockafellar and Uryasev 2000).
#' As such, this method is able to better approximate the
#' probabilistic constraints and, in turn, could potentially yield
#' solutions that are more robust to uncertainty and less cost-efficient
#' than the chance constraint programming method.
#' To describe this method, let \eqn{\eta_j}{ej} denote
#' a continuous auxiliary variable for each feature group \eqn{j}{j},
#' and \eqn{S_{jk}}{Sjk} a continuous auxiliary variable for each feature
#' \eqn{k}{k} associated with each feature group \eqn{j}{j}.
#' Given this terminology, the method involves replacing the probabilistic
#' constraints with the following linear constraints.
#'
#' \deqn{
#' \sum_{i = 1}^{I} (x_i \times R_{ijk}) - \eta_j + S_{jk} \geq 0 \quad
#'   \forall \space j \in J, \space k \in K \\
#' \eta_j - \frac{1}{(1 - \alpha) \times K_j} \sum_{k=1}^{K_j} S_{jk}
#'   \geq T_{jk}\quad \forall \space j \in J \\
#' S_{jk} \geq 0 \quad \forall \space j \in J, \space k \in K \\
#' \eta_j \in \mathbb{R}  \quad  \forall \space j \in J
#' }{
#' \sum_i^I xi Rijk - ej + Sjk >= 0, for all j in J, k in K
#' ej - (1 / ((1 - a) * K_j)) sum_k^Kj Sjk >= Tjk for all j in J
#' Sjk >= 0, for all j in J, k in K
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
#' target the feature (i.e., \eqn{T_{jk}}{Tjk}).
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
#' An updated `ConservationProblem` object (i.e., [prioritizr::problem()]) with
#' the objective added to it.
#'
#' @examplesIf robust.prioritizr::run_example()
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
#' # Create base problem
#' x <- problem(pu, features)
#'
#' # Add robust minimum set objective (using CVaR method)
#' p <- add_robust_min_set_objective(x, method = "cvar") |>
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
#'
#' @srrstats {G1.0} Charnes & Cooper (1959) and Rockafellar & Uryasev (2000)
#'   are cited in @references.
#' @srrstats {G1.3} Probabilistic constraints, confidence level, CVaR, and
#'   chance constraint programming are defined in the Details section.
#' @srrstats {G2.0, G2.0a} method is asserted to be a single string;
#'   validated against permitted values.
#' @srrstats {G2.1, G2.1a} assertthat::is.string(method) enforces type;
#'   documented in @param.
#' @srrstats {G2.3a} method restricted to "chance" or "cvar" via assert().
#' @srrstats {G2.3b} tolower() applied to method for case-insensitive input.
#' @srrstats {G2.6} is_conservation_problem(x) validates the primary input.
#' @srrstats {G3.0} No floating-point equality comparisons; solver tolerance
#'   is managed by the ILP solver (e.g., HiGHS).
#' @srrstats {SP4.0, SP4.0a} solve() returns a SpatRaster matching the class
#'   of the input planning unit raster.
#' @srrstats {SP4.2} @return explicitly documents the class of the return
#'   value.
#' @srrstats {G5.3} Absence of NA and out-of-range values in solve() return
#'   objects is explicitly tested.
#' @srrstats {G5.4} Correctness tests confirm LP matrix structure for fixed
#'   test datasets via expect_snapshot.
#' @srrstats {G5.4a} Correctness tested against trivial case (conf_level = 1
#'   recovers non-robust result) and structural LP matrix snapshots.
#' @srrstats {G5.6} Parameter recovery tests confirm solve() meets specified
#'   targets for data with known properties.
#' @srrstats {G5.6a} Tests succeed within solver tolerance.
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
        has_targets = TRUE,
        has_weights = FALSE,
        calculate = function(x) {
          # assert argument is valid
          assert(is_conservation_problem(x), .internal = TRUE)
          # assert that robust constraints are specified correctly
          assert(has_robust_constraints(x))
          # return success
          invisible(TRUE)
        },
        apply = function(x, y, ...) {
          # assert valid arguments
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # get feature grouping data
          d <- get_feature_group_data(y)
          targets <- transform_targets(y$feature_targets(), d)
          # apply objective
          invisible(
            rcpp_apply_robust_min_set_objective(
              x$ptr,
              targets,
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
                  targets,
                  d$ids,
                  d$conf_level
                )
              )
            } else {
              invisible(
                rcpp_apply_robust_probability_constraints(
                  x$ptr,
                  targets,
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
