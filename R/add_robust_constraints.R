#' @include internal.R
NULL

#' Add robust constraints
#'
#' TODO.
#'
#' @param x [prioritizr::problem()] object.
#'
#' @param feature_groupings `character` vector indicating which features
#'  should be grouped together for the purposes of characterizing uncertainty.
#'  For example, if some of the features correspond to alternative predictions
#'  for the same species under different scenarios, then these features should
#'  contain the same grouping name.
#'
#' @details
#' TODO.
#'
#' @section Mathematical formulation:
#' TODO.
#'
#' @references
#' TODO.
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
#' @name add_robust_constraints
NULL

#' @rdname add_robust_constraints
#' @export
add_robust_constraints <- function(x, feature_groupings) {
  # assert argument is valid
  assert_required(x)
  assert_required(feature_groupings)
  assert(
    is_conservation_problem(x),
    is.character(feature_groupings),
    assertthat::noNA(feature_groupings)
  )
  # add objective to problem
  x$add_constraint(
    R6::R6Class(
      "RobustConstraint",
      inherit = prioritizr::Constraint,
      public = list(
        name = "robust constraints",
        data = list(feature_groupings = feature_groupings),
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
