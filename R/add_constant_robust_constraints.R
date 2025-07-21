#' @include internal.R
NULL

#' Add constant robust constraints
#'
#' Add robust or probabilistic constraints to a conservation problem to ensure that
#' the priority areas are robust to uncertainty up to a certain confidence level. In particular, this
#' function is useful when the confidence level that the constraint is held
#' is constant across all features.
#'
#' The robust/ chance constraints ensures that the proportion of constraints that are held
#' is greater than a specified `confidence_level`. If `confidence_level = 1`,
#' all constraints within the feature group must be held, meaning that the solution is fully
#' robust to uncertainty. Lowering the `confidence_level` to less than
#' 1 allows a certain percentage of the constraints for each feature group to be
#' violated, enabling the algorithm to search of solutions with better objective values, while
#' keeping the percentage of constraints violated less than `1 - confidence_level`.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @param groups `character` vector indicating which features
#'  should be grouped together for the purposes of characterizing uncertainty.
#'  In particular, `groups` is used to specify a group name for each feature
#'  and features with the same group name will be grouped together.
#'  For example, if some of the features correspond to alternative predictions
#'  for the same species under different scenarios, then these features should
#'  have the same grouping name.
#'
#' @param confidence_level `numeric` value describing the level of robustness
#'  required for the prioritization (ranging between 0 and 1). For instance, a value
#'  of 0.95 guarantees that at least 95% of the constraints will met for each
#'  feature group. A value of 1 ensures full robustness, i.e., all constraints
#'  are met. A value of 0 will cause none of the constraints to be applied,
#'  and is thus not recommended.
#'
#' @section Data requirements:
#' The robust constraints require that you have multiple alternative
#' realizations for each biodiversity element of interest (e.g.,
#' species, ecosystems, ecosystem services). For example, we might have 5
#' species of interest. By applying different spatial modeling techniques,
#' we might have 10 different models for each of the 5 different species.
#' We can use these models to generate 10 alternative realizations
#' for each of the 5 species (yielding 50 alternative realizations in total).
#' To use these data, we would input these 50 alternative realizations
#' as 50 features when initializing a conservation planning problem
#' (i.e., [prioritizr::problem()]) and then use this function to specify which
#' of the of the features correspond to the same species (based on the feature
#' groupings parameter).
#'
#' @references
#' Charnes, A., & Cooper, W. W. (1959). Chance-constrained programming. Management Science, 6(1), 73–79.
#'
#' @seealso
#' See [robust_objectives] for an overview of all functions for adding
#' robust objectives.
#'
#' @return An updated [prioritizr::problem()] object with the constraint added
#' to it.
#'
#' @examples
#' \dontrun{
#' TODO.
#' }
#'
#' @name add_constant_robust_constraints
NULL

#' @rdname add_constant_robust_constraints
#' @export
add_constant_robust_constraints <- function(x, groups, confidence_level = 1) {
  # assert arguments are valid
  assert_required(x)
  assert_required(groups)
  assert(
    is_conservation_problem(x),
    is.character(groups),
    assertthat::noNA(groups),
    assertthat::is.number(confidence_level),
    assertthat::noNA(confidence_level),
    confidence_level >= 0,
    confidence_level <= 1
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
  assert(
    all(as.data.frame(table(groups))[[2]] > 1),
    msg = c(
      "!" = paste(
        "Each group in {.arg groups} must have at least two",
        "realizations."
      )
    )
  )

  # add objective to problem
  add_variable_robust_constraints(
    x,
    data = tibble::tibble(
      features = split(prioritizr::feature_names(x), groups),
      confidence_level = confidence_level
    )
  )
}
