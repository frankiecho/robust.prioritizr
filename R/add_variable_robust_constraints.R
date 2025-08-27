#' @include internal.R
NULL

#' Add variable robust constraints
#'
#' Add robust constraints to a conservation problem to ensure that
#' the priority areas are robust to uncertainty. In particular, this
#' function is useful when the confidence level is different for
#' different features. For example, this function may be especially
#' useful when it is important to ensure that a prioritization is
#' highly robust to uncertainty in the spatial distribution of threatened
#' species, and only moderately robust to uncertainty in the spatial
#' distribution of widespread species.
#'
#' @inherits add_constant_robust_constraints details
#'
#' @param x [prioritizr::problem()] object.
#'
#' @param data [tibble::tibble()] data frame containing information the feature
#' groupings and their desired confidence level. See the Data format
#' section for details. Also, see the Examples section for example usage.
#'
#' @section Data format:
#' The `data` argument must be a [tibble::tibble()] data frame that has
#' information on the feature groupings and their confidence levels.
#' Here, each row corresponds to a different feature group and
#' columns contain information about the groups.
#' In particular, it has the following columns.
#' \describe{
#' \item{features}{
#' A `list` column with the names of the features that belong to each group.
#' In particular, if a particular set of features should belong to the same
#' group, then they should be stored in the same element of this column.
#' }
#' \item{conf_level}{
#' A `numeric` column with values that describe the confidence level
#' associated with each feature group (ranging between 0 and 1).
#'  For example, a value of zero corresponds
#'  to a low confidence level, and so the optimization process will not
#'  constrained by any constraints in that particular group.
#'  Alternatively, a value of of one corresponds to a high level of robustness,
#'  and so the optimization process will be highly constrained to be
#'  robust against uncertainty for that particular group.
#' }
#' }
#'
#' @inheritSection add_constant_robust_constraints Data requirements
#'
#' @seealso
#' See [robust_objectives] for an overview of all functions for adding
#' robust objectives.
#'
#' @inherit add_constant_robust_constraints seealso return references
#'
#' @examples
#' \dontrun{
#' TODO.
#' }
#'
#' @name add_variable_robust_constraints
NULL

#' @rdname add_variable_robust_constraints
#' @export
add_variable_robust_constraints <- function(x, data) {
  # assert arguments are valid
  assert_required(x)
  assert_required(data)
  assert(
    is_conservation_problem(x),
    is.data.frame(data),
    assertthat::has_name(data, "features"),
    assertthat::has_name(data, "conf_level"),
    is.list(data$features),
    is.numeric(data$conf_level),
    assertthat::noNA(data$conf_level)
  )
  # additional validation for feature groupings
  assert(
    all(vapply(data$features, is.character, logical(1))),
    msg = c(
      "!" = paste(
        "Each element in {.arg data$features} must contain a",
        "{.cls character} vector."
      )
    )
  )
  assert(
    all(
      unlist(data$features, recursive = TRUE, use.names = TRUE) %in%
      prioritizr::feature_names(x)
    ),
    msg = c(
      "!" = paste(
        "{.arg data$features} must contain only the names of features in",
        "{.arg x}."
      ),
      "i" = paste(
        "To see the features names in {.arg x},",
        "use {.code feature_names(x)}."
      )
    )
  )
  assert(
    all(lengths(data$features) >= 2),
    msg = c(
      "!" = paste(
        "Each group in {.arg data$features} must have at least 2",
        "feature names."
      )
    )
  )

  # add constraints
  x$add_constraint(
    R6::R6Class(
      "RobustConstraint",
      inherit = prioritizr::Constraint,
      public = list(
        name = ifelse(
          identical(length(unique(data$conf_level)), 1L),
          "constant robust constraints",
          "variable robust constraints"
        ),
        data = list(data = data),
        apply = function(x, y) {
          # note that these constraints are just used as a dummy place holder
          # to store the feature grouping information, and so the $apply()
          # method does not actually do anything
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # return success
          invisible(TRUE)
        }
      )
    )$new()
  )
}
