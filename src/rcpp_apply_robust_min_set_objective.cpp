#include "package.h"
#include "prioritizr_optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_robust_min_set_objective(
  SEXP x,
  const Rcpp::List targets_list,
  const Rcpp::NumericMatrix costs,
  const Rcpp::IntegerVector feature_group_ids
) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];

  // add objective
  for (std::size_t i = 0; i < (ptr->_number_of_planning_units * ptr->_number_of_zones); ++i) {
    if (Rcpp::NumericVector::is_na(costs[i])) {
      // If cost is NA then the planning unit cannot be selected, i.e., lb and ub becomes 0
      ptr->_obj.push_back(0.0);
      ptr->_lb[i] = 0.0;
      ptr->_ub[i] = 0.0;
    } else {
      // Include the cost in the objective function directly
      ptr->_obj.push_back(costs[i]);
    }
  }

  // Find the maximum target within the feature group, for use within relative targets
  Rcpp::NumericVector robust_target_value(targets_value.size());
  for (std::size_t i = 0; i < static_cast<std::size_t>(targets_value.size()); ++i) {
    int max_target = 0;
    for (std::size_t j = 0; j < static_cast<std::size_t>(targets_value.size()) ; ++j) {
      if (targets_value[feature_group_ids[j]] > max_target) {
        max_target = targets_value[feature_group_ids[j]];
      }
    }
    robust_target_value[i] = max_target;
  }

  // add constraints
  for (std::size_t i = 0; i < static_cast<std::size_t>(targets_value.size()); ++i) {
    // Loop through all the targets and find the corresponding features

    for (std::size_t j = 0; j < static_cast<std::size_t>(feature_group_ids.size()); ++j) {
      // Loop through all the feature grouping indices

      if (i == static_cast<std::size_t>(feature_group_ids[j])) {
        // If the target index is equal to the feature group ID, meaning that this target applies to this feature
        ptr->_rhs.push_back(robust_target_value[i]);
        ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[i]));
        ptr->_row_ids.push_back("spp_target");
      }
    }

  }

  ptr->_modelsense="min";

  // return succes
  return true;
}
