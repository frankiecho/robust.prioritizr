#' @include internal.R
NULL

#' Get feature groupings
#'
#' Get the feature groupings from a [prioritizr::problem()] object.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @param convert_to_integer `logical` value indicating if the feature
#' groupings should be converted to integer values. Defaults to `TRUE`.
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
#' @return An invisible `logical` value indicating success.
#'
#' @noRd
get_feature_groupings <- function(x, convert_to_integer = TRUE) {
  # assert valid argument
  assert_required(x)
  assert_required(convert_to_integer)
  assert(
    is_conservation_problem(x),
    assertthat::is.flag(convert_to_integer),
    assertthat::noNA(convert_to_integer)
  )
  assert(has_robust_constraints(x))

  # extract the feature groupings
  i <- which(vapply(x$constraints, inherits, logical(1), "RobustConstraint"))
  feature_groupings <- x$constraints[[i]]$get_data("feature_groupings")

  # assert feature groupings are valid given the problem
  if (!inherits(x$targets, "Waiver") && !is.null(x$targets)) {
    ## if targets specified, then we should have a grouping id for
    ## each target
    n_targets <- nrow(x$targets$output())
    assert(
      identical(length(unique(feature_groupings)), n_targets),
      call = rlang::expr(add_robust_constraints()),
      msg = c(
        "!" = paste(
          "{.arg feature_groupings} must have a value",
          "for each target."
        ),
        "x" = paste(
          "Number of feature groupings = ",
          "{.val {length(unique(feature_groupings))}}."
        ),
        "x" = "Number of targets = {.val {n_targets}}."
      )
    )
  } else {
    ## otherwise, we should have a grouping id for each feature
    n_features <- prioritizr::number_of_features(x)
    assert(
      identical(length(feature_groupings), n_features),
      call = rlang::expr(add_robust_constraints()),
      msg = c(
        "!" = paste(
          "{.arg feature_groupings} must have a value",
          "for each feature."
        ),
        "x" = paste(
          "Number of feature groupings = ",
          "{.val {length(feature_groupings)}}."
        ),
        "x" = "Number of features = {.val {n_features}}."
      )
    )
  }

  # if needed, convert the feature groupings to an integer values
  # note that we use a zero-based index for convenience with C++
  if (isTRUE(convert_to_integer)) {
    feature_groupings <- as.integer(factor(feature_groupings)) - 1
  }

  # return result
  feature_groupings
}
