#' @include internal.R
NULL

#' Add constant robust constraints
#'
#' Add robust constraints to a conservation problem to specify that
#' the solution should have the same minimum level of robustness for each
#' feature group.
#'
#' @param groups `character` vector indicating which features
#'  should be grouped together for the purposes of characterizing uncertainty.
#'  In particular, `groups` is used to specify a group name for each feature
#'  and features with the same group name will be grouped together.
#'  For example, if some of the features correspond to alternative predictions
#'  for the same species under different scenarios, then these features should
#'  have the same group name.
#'
#' @param conf_level `numeric` value describing the level of robustness
#'  required for the solution (ranging between 0 and 1).
#'  Defaults to 1, corresponding to a maximally robust solution.
#'  See the Details section for more information on this parameter.
#'
#' @param target_trans `character` value or vector of values specifying the method(s) for
#' transforming and standardizing target thresholds for features
#' that belong to the same feature group.
#' Available options include computing the (`"mean"`) average,
#' (`"min"`) minimum, or (`"max"`) maximum target threshold for each
#' feature group. Additionally, (`"none"`) can be specified to ensure that the
#' target thresholds considered during optimization are based on exactly the
#' same values as specified when building the problem---even though different
#' features in the same group may have different targets.
#' Defaults to `NA` such that the average value is computed
#' (similar to `target_trans = "mean"`) and
#' a message indicating this behavior is displayed.
#' If it is a vector, then it must specify the transformation methods in a vector,
#' such as `c('min', 'max')` in case there are two feature groups and we want the
#' first target to be taken at the minimum and the second to be taken at the maximum.
#' The vector length must equal the length of `groups`.
#'
#' @inheritParams add_robust_min_set_objective
#'
#' @details
#' The robust constraints are used to generate solutions that are robust to
#' uncertainty. In particular, `conf_level` controls how
#' important it is for a solution to be robust to uncertainty.
#' To help explain how these constraints operate, we will consider
#' the minimum set formulation of the reserve selection problem
#' (per [prioritizr::add_min_set_objective()].
#' If `conf_level = 1`, then the solution must be maximally robust to
#' uncertainty and this means that the solution must meet all of the targets
#' for the features associated with each feature group.
#' Although such a solution would be highly robust to uncertainty,
#' it may not be especially useful because this it might have
#' especially high costs (in other words, setting a high `conf_level`
#' may result in a solution with a poor objective value).
#' By lowering `conf_level`, this means that the solution must only meet
#' certain percentage of the targets associated with each feature group.
#' For example, if `conf_level = 0.95`, then the solution must meet, at least,
#' 95% of the targets for the features associated with each feature group.
#' Alternatively, if `conf_level = 0.5`, then the solution must meet, at least,
#' half of the targets for the features associated with each feature group.
#' Finally, if `conf_level = 0`, then the solution does not need
#' to meet any of the targets for the features associated with each
#' feature group. As such, it is not recommended to use `conf_level = 0`.
#'
#' @section Data requirements:
#' The robust constraints require that you have multiple alternative
#' realizations for each biodiversity element of interest (e.g.,
#' species, ecosystems, ecosystem services). For example, we might have 5
#' species of interest. By applying different spatial modeling techniques,
#' we might have 10 different models for each of the 5 different species
#' We can use these models to generate 10 alternative realizations
#' for each of the 5 species (yielding 50 alternative realizations in total).
#' To use these data, we would input these 50 alternative realizations
#' as 50 features when initializing a conservation planning problem
#' (i.e., [prioritizr::problem()]) and then use this function to specify which
#' of the of the features correspond to the same species (based on the feature
#' groupings parameter).
#'
#' @seealso
#' See [robust_constraints] for an overview of all functions for adding
#' robust constraints.
#'
#' @return
#' An updated [prioritizr::problem()] object with the constraint added
#' to it.
#'
#' @family constraints
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
#' # Build problem
#' p <-
#'   problem(pu, features) |>
#'   add_robust_min_set_objective() |>
#'   add_constant_robust_constraints(groups = groups, conf_level = 0.9) |>
#'   add_relative_targets(0.1) |>
#'   add_binary_decisions() |>
#'   add_default_solver(verbose = FALSE)
#'
#' # Solve the problem
#' soln <- solve(p)
#'
#' # Plot the solution
#' plot(soln)
#' }
#'
#' @name add_constant_robust_constraints
NULL

#' @rdname add_constant_robust_constraints
#' @export
add_constant_robust_constraints <- function(x, groups, conf_level = 1,
                                            target_trans = NA) {
  # assert arguments are valid
  assert_required(x)
  assert_required(groups)
  assert(
    is_conservation_problem(x),
    is.character(groups),
    assertthat::noNA(groups),
    assertthat::is.number(conf_level),
    assertthat::noNA(conf_level),
    conf_level >= 0,
    conf_level <= 1,
    assertthat::is.scalar(target_trans)
  )
  if (is.na(target_trans)[[1]]) {
    target_trans = NA_character_
  }
  assert(
    assertthat::is.string(target_trans),
    is_match_of(target_trans, c("none", "mean", "max", "min", NA_character_))
  )

  # additional validation for feature groupings
  assert(
    identical(length(groups), as.integer(prioritizr::number_of_features(x))),
    msg = c(
      "{.arg groups} must specify a value for each feature in {.arg x}.",
      "x" = "{.arg x} has {x$number_of_features()} feature{?s}.",
      "x" = "{.arg groups} has {length(groups)} value{?s}."
    )
  )

  # if at least one feature group only has a single feature,
  # then display message
  n_group_with_one_feature <- sum(as.data.frame(table(groups))[[2]] == 1L)
  if (isTRUE(n_group_with_one_feature > 0.5)) {
    cli::cli_inform(
      c(
        "i" = paste(
          "{.arg groups} specifies that {n_group_with_one_feature} feature",
          "group{?s} {?contains/contain} a single feature."
        ),
        "i" = paste(
          "{cli::qty(n_group_with_one_feature)}",
          "As such, the robust optimization procedures cannot",
          "account for uncertainty in {?this/these} feature group{?s}."
        )
      )
    )
  }

  # add constraints to problem
  suppressMessages(
    add_variable_robust_constraints(
      x,
      data = tibble::tibble(
        features = split(prioritizr::feature_names(x), groups)[unique(groups)],
        conf_level = conf_level,
        target_trans = target_trans
      )
    )
  )
}
