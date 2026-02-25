#' @include internal.R
NULL

#' Add robust constraints
#'
#' Add constraints to a conservation planning problem to
#' explicitly account for uncertainty.
#'
#' @details
#' The robust constraints functions are designed to be used with a robust
#' objective function (e.g., [add_robust_min_set_objective()],
#' [add_robust_min_shortfall_objective()]).
#' In particular, these functions are used to specify which
#' features should be grouped together for characterizing plausible
#' realizations. For example, if considering considering multiple projections
#' for a species' distribution, these constraints are used to specify
#' which features correspond to the same species. In addition to
#' specifying feature groups, these constraints are also used to
#' specify a confidence level that describes the level of robustness
#' required for solutions.
#'
#' The following robust constraint functions can be added to a
#' conservation planning problem:
#'
#' \describe{
#'
#' \item{[add_constant_robust_constraints()]}{
#' Add robust constraints to a conservation problem to specify that
#' the solution should ideally aim for the same level of robustness for each
#' feature group.
#' }
#'
#' \item{[add_variable_robust_constraints()]}{
#' Add robust constraints to a conservation problem to specify that
#' the solution should ideally aim for different levels of robustness for each
#' feature group.
#' }
#'
#' }
#'
#' @family overviews
#'
#' @examplesIf robust.prioritizr::run_example()
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
#' # Build problem with constant robust constraints
#' p1 <-
#'   problem(pu, features) |>
#'   add_constant_robust_constraints(groups = groups, conf_level = 0.5) |>
#'   add_robust_min_set_objective() |>
#'   add_relative_targets(0.1) |>
#'   add_binary_decisions() |>
#'   add_default_solver(verbose = FALSE)
#'
#' # Next, we will create the input data for adding variable robust
#' # constraints. In particular, we specify a confidence level of 0.95 for
#' # group A, and a confidence level of 0.5 for group B
#' constraint_data <- tibble(
#'   features = split(names(features), groups),
#'   conf_level = c(0.95, 0.5)
#' )
#'
#' # Display constraint data
#' print(constraint_data)
#'
#' # Build problem with variable robust constraints
#' p2 <-
#'   problem(pu, features) |>
#'   add_variable_robust_constraints(data = constraint_data) |>
#'   add_robust_min_set_objective() |>
#'   add_relative_targets(0.1) |>
#'   add_binary_decisions() |>
#'   add_default_solver(verbose = FALSE)
#'
#' # Solve the problems
#' soln <- c(solve(p1), solve(p2))
#' names(soln) <- c(
#'   "constant robust constraints", "variable robust constraints"
#' )
#'
#' # Plot the solutions
#' plot(soln)
#'
#' @name robust_constraints
NULL
