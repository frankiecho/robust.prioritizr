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
#' The [add_robust_constraints()] function is used to specify feature
#' groupings.
#'
#' @return
#' A `list` containing the (`$ids`) `integer` group identifiers for each feature
#' and (`$confidence_level`) `numeric` confidence_level for each group.
#'
#' @noRd
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
  target_feature_names <- x$feature_names()[x$targets$output()$feature]
  idx <- match(target_feature_names, group_feature_names)
  assert(
    assertthat::noNA(idx),
    msg = "Failed to match feature groupings.",
    .internal = TRUE
  )
  feature_groupings <- group_ids[idx]

  # return result
  list(ids = feature_groupings, conf_level = data$conf_level)
}
