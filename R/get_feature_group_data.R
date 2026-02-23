#' @include internal.R
NULL

#' Get feature group data
#'
#' Get the feature group data from a [prioritizr::problem()] object.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @details
#' This function is used internally by the robust objective functions
#' to extract the feature groupings from a [prioritizr::problem()] based on the
#' robust constraints.
#'
#' @seealso
#' The [add_constant_robust_constraints()] function is used to specify feature
#' groupings.
#'
#' @return
#' A `list` containing the (`$ids`) `integer` group identifiers for each
#' feature, (`$confidence_level`) `numeric` confidence_level for each group,
#' and ($target_trans`) `character` method for transforming targets.
#'
#' @examples
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
#' # Build problem
#' p <-
#'   problem(pu, features) |>
#'   add_robust_min_set_objective() |>
#'   add_constant_robust_constraints(groups = groups, conf_level = 0.9) |>
#'   add_relative_targets(0.1) |>
#'   add_binary_decisions() |>
#'   add_default_solver(verbose = FALSE)
#' 
#' feature_group_data <- get_feature_group_data(p)
#' print(feature_group_data)
#' 
get_feature_group_data <- function(x) {
  # assert valid argument
  assert_required(x)
  assert(is_conservation_problem(x))
  assert(has_robust_constraints(x))

  # extract the feature group data from constraints
  i <- which(vapply(x$constraints, inherits, logical(1), "RobustConstraint"))
  data <- x$constraints[[i]]$get_data("data")

  # identify group id for each feature
  group_ids <- rep(seq_along(data$features) - 1, lengths(data$features))
  group_feature_names <- unlist(
    data$features, recursive = TRUE, use.names = FALSE
  )
  target_feature_names <- x$feature_names()[x$feature_targets()$feature]
  idx <- match(target_feature_names, group_feature_names)
  assert(
    assertthat::noNA(idx),
    msg = "Failed to match feature groupings.",
    .internal = TRUE
  )
  feature_groupings <- group_ids[idx]

  # return result
  list(
    ids = feature_groupings,
    conf_level = data$conf_level,
    target_trans = data$target_trans
  )
}
