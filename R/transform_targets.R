#' Transform targets
#'
#' Standardize targets for features that belong to the same feature group.
#'
#' @param target_data `data.frame` with information on the target thresholds.
#' It should be generated with `p$feature_targets()`, where `p` is a
#' [prioritizr::problem()] object.
#'
#' @param feature_group_data `data.frame` with information on which
#' features belong to each feature group. It should be generated with
#' `get_feature_group_data()`.
#'
#' @return
#' A modified version of `targets` where the values in the threshold column
#' are altered according to the target transformation methods specified
#' by `feature_group_data`.
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
#' x <- transform_targets(p$feature_targets(), get_feature_group_data(p))
#' print(x)
#' 
transform_targets <- function(target_data, feature_group_data) {
  # assert arguments are valid
  assert(
    !inherits(target_data, "Waiver"),
    msg = "{.fn problem} does must have defined targets."
  )
  assert(
    is.data.frame(target_data),
    assertthat::has_name(target_data, "feature"),
    assertthat::has_name(target_data, "zone"),
    assertthat::has_name(target_data, "sense"),
    assertthat::has_name(target_data, "value"),
    is.list(feature_group_data),
    assertthat::has_name(feature_group_data, "ids"),
    assertthat::has_name(feature_group_data, "conf_level"),
    assertthat::has_name(feature_group_data, "target_trans"),
    .internal = TRUE
  )

  # identify feature group id and target transformation method for each feature
  target_data$group <- feature_group_data$ids[target_data$feature]
  target_data$target_trans <-
    feature_group_data$target_trans[feature_group_data$ids + 1]

  # identify feature groups that have variation in the target
  # values for their features
  group_has_within_target_var <- tapply(
    target_data$value,
    target_data$group,
    function(x) diff(range(x, na.rm = TRUE)) > 1e-5
  )

  # check if target_trans is NA, and so that additional information
  # may need to be displayed
  is_missing_target_trans <- any(is.na(feature_group_data$target_trans))

  # set default behavior if target_trans is NA
  target_data$target_trans[is.na(target_data$target_trans)] <- "mean"

  # prepare message to display about target transformations
  if (isTRUE(any(group_has_within_target_var))) {
    # initialize message
    msg <- c(
      "i" = paste(
        "{.arg x} has {sum(group_has_within_target_var)}",
        "feature group{?s} that have features associated with multiple",
        "distinct targets."
      )
    )
    # identify which transformation methods for these groups
    target_trans_names <- unique(
      target_data$target_trans[group_has_within_target_var]
    )

    # if targets will be transformed, then add a message to describe
    # how the targets will transformed
    if (!identical(target_trans_names, "none")) {
      if (identical(length(target_trans_names), 1L)) {
        # if only one target transformation method will be used, then name it
        msg <- c(
          "i" = paste(
            "{cli::qty(sum(group_has_within_target_var))} ",
            "The target{?s} for {?this/these} group{?s} are",
            "transformed based on the {.fn {target_trans_names}} target value."
          )
        )
      } else {
        # otherwise update the message to say that a combination of methods
        # will be applied
        msg <- c(
          msg,
          "i" = paste(
            "{cli::qty(sum(group_has_within_target_var))} ",
            "The target{?s} for {?this/these} group{?s} are",
            "transformed based on the specified methods."
          )
        )
      }
    }
    # display message
    cli::cli_inform(msg, call = NULL)
  }

  # compute target value for each feature group
  ## note that if a feature group has "none", then the result is NA
  new_targets <- data.frame(group = unique(target_data$group))
  new_targets$value <- vapply(
    unique(target_data$group),
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE,
    function(i) {
      idx <- which(target_data$group == i)
      fun_name <- target_data$target_trans[idx[[1]]]
      if (identical(fun_name, "none")) return(NA_real_)
      get(fun_name)(target_data$value[idx], na.rm = TRUE)
    }
  )

  # initialize output
  out <- target_data

  # set targets according to transformed target values
  out$value <- new_targets$value[match(out$group, new_targets$group)]

  # if targets have NA values, then this is because target_trans is
  # "none", and so these values will be overwritten with the original target
  # values
  idx <- is.na(out$value)
  out$value[idx] <- target_data$value[idx]

  # remove extra columns from output
  out$group <- NULL
  out$target_trans <- NULL

  # return result
  out
}
