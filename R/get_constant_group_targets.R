#' @include internal.R
NULL

#' Check whether the targets are consistent within the same group, and change the
#' targets to make it consistent by setting it to a common target based on the
#' mean, median, min or max.
#'
#' @param x [prioritizr::problem()] object.
#' @param target_trans a string specifying the method used to transform the target such that
#' it is consistent within each feature group. Can be "none" (keeping the original values), "mean"
#' (mean), "min" (minimum of the target), or "max" (maximum).
#'
#' @details
#' This function is used internally by the robust objective functions
#' to check for consistency of targets within the same group and alter the targets
#' such that it is constant within the same group.
#'
#'
#' @return
#' A `targets` object that is in the same format as `x$feature_targets()`
#'
#' @noRd
get_constant_group_targets <- function(x, target_trans) {
  # assert valid argument
  assert_required(x)
  assert(is_conservation_problem(x))
  assert(has_robust_constraints(x))

  # Check if the user explicitly set target trans or is it implied behaviour
  target_trans_missing <- is.null(target_trans)

  # Set the default value for target trans
  if (target_trans_missing) {
    target_trans <- "mean"
  }

  assert(
    isTRUE(target_trans %in% c("none", "min", "mean", "max")),
    msg = "{.arg target_trans} must be {.val none}, {.val mean}, {.val min} or {.val max}."
  )

  feature_groupings <- get_feature_group_data(x)

  targets <- x$feature_targets()

  # Check if there is any within-group variation
  within_group_var <- sum(tapply(targets$value, feature_groupings$id,
                                 function(x) (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))) > 1e-5)



  if (identical(target_trans, "none")) {
    if (within_group_var > 0) {
      cli::cli_inform(
        c(
          "i" = paste(
            "{.arg x} has {within_group_var}",
            "feature group{?s} with multiple distinct targets."
          )
        ),
        call = NULL
      )
    }
    return(targets)
  } else if (identical(target_trans, "min")) {
    if (within_group_var) {
      cli::cli_inform(
        c(
          "i" = paste(
            "{.arg x} has {within_group_var}",
            "feature group{?s} with multiple distinct targets."
          ),
          "i" = paste(
            "{cli::qty(within_group_var)} ",
            "The target{?s} for {?this/these} group{?s} are",
            "transformed to the group min target value."
          )
        ),
        call = NULL
      )
    }
    group_min <- tapply(targets$value, feature_groupings$id, min)
    idx <- match(feature_groupings$id, names(group_min))
    targets$value <- group_min[idx]
  } else if (identical(target_trans, "mean")) {
    if (within_group_var & target_trans_missing) {
      cli::cli_inform(
        c(
          "i" = paste(
            "{.arg x} has {within_group_var}",
            "feature group{?s} with multiple distinct targets."
          ),
          "i" = paste(
            "{cli::qty(within_group_var)} ",
            "The target{?s} for {?this/these} group{?s} are",
            "transformed to the group mean target value."
          )
        ),
        call = NULL
      )
    }

    group_mean <- tapply(targets$value, feature_groupings$id, mean)
    idx <- match(feature_groupings$id, names(group_mean))
    targets$value <- group_mean[idx]
  } else if (identical(target_trans, "max")) {
    if (within_group_var) {
      cli::cli_inform(
        c(
          "i" = paste(
            "{.arg x} has {within_group_var}",
            "feature group{?s} with multiple distinct targets."
          ),
          "i" = paste(
            "{cli::qty(within_group_var)} ",
            "The target{?s} for {?this/these} group{?s} are",
            "transformed to the group max target value."
          )
        ),
        call = NULL
      )
    }

    group_max <- tapply(targets$value, feature_groupings$id, max)
    idx <- match(feature_groupings$id, names(group_max))
    targets$value <- group_max[idx]
  }
  return(targets)
}
