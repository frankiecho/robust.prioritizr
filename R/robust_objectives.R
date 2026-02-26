#' @include internal.R
NULL

#' Add a robust objective function
#'
#' Add an objective function to a conservation planning problem that
#' accounts for uncertainty.
#'
#' @details
#' Robust objective functions are used to find solutions that are likely to
#' meet conservation targets across a range of different scenarios or
#' realizations of the input data. This is particularly useful when working
#' with data that is uncertain, such as species distribution models under
#' climate change scenarios. Note that robust constraints must
#' also be used when using these objective functions
#' (e.g., [add_constant_robust_constraints()],
#' [add_variable_robust_constraints()]).
#'
#' The following robust objective functions can be added to a
#' conservation planning problem:
#'
#' \describe{
#'
#' \item{[add_robust_min_set_objective()]}{
#' Add an objective to a conservation planning problem to minimize the cost
#' of the solution while ensuring that the targets for each feature group are
#' met in a manner that is robust to uncertainty.
#' This function provides a robust alternative to
#' [prioritizr::add_min_set_objective()].
#' }
#'
#' \item{[add_robust_min_shortfall_objective()]}{
#' Add an objective to a conservation planning problem that minimizes the
#' target shortfalls for each feature group in a manner that is robust
#' to uncertainty, whilst ensuring that the total cost of the solution
#' does not exceed a budget.
#' This function provides a robust alternative to
#' [prioritizr::add_min_shortfall_objective()].
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
#'
#' # Get planning unit data
#' pu <- get_sim_pu_raster()
#' features <- get_sim_features()
#'
#' # Define the feature groups,
#' # Here, we will assign the first 2 features to the group A, and
#' # the remaining features to the group B
#' groups <- c(rep("A", 2), rep("B", nlyr(features) - 2))
#'
#' # Build problem with robust min set objective
#' p1 <-
#'   problem(pu, features) %>%
#'   add_robust_min_set_objective() %>%
#'   add_constant_robust_constraints(groups = groups, conf_level = 0.4) %>%
#'   add_binary_decisions() %>%
#'   add_relative_targets(0.3) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # Build problem with robust min shortfall objective,
#' # and budget set to 30% of the total cost of all planning units
#' p2 <-
#'   problem(pu, features) %>%
#'   add_robust_min_shortfall_objective(
#'     budget = terra::global(pu, "sum", na.rm = TRUE)[[1]] * 0.3
#'   ) %>%
#'   add_constant_robust_constraints(groups = groups, conf_level = 0.4) %>%
#'   add_binary_decisions() %>%
#'   add_relative_targets(0.3) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # Solve problems
#' soln <- c(solve(p1), solve(p2))
#' names(soln) <- c("robust min set", "robust min shortfall")
#' plot(soln, axes = FALSE)
#' 
#' @name robust_objectives
NULL
