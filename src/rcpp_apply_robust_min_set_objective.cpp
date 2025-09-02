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
  const std::size_t n_targets = static_cast<std::size_t>(targets_value.size());

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

  // declare constants
  const std::size_t n_groups = *std::max_element(
    feature_group_ids.begin(), feature_group_ids.end()
  ) + 1;

  // Find maximum target for each group to deal with two use cases:
  // 1. User specified relative targets, leading to different targets
  //    for the same feature across realizations
  // 2. User specified manual targets with different targets for the
  //    same feature
  Rcpp::NumericVector feature_group_target(
      n_groups, std::numeric_limits<double>::lowest()
  );
  for (std::size_t i = 0; i < n_targets; ++i) {
    feature_group_target[feature_group_ids[i]] = std::max(
      feature_group_target[feature_group_ids[i]], targets_value[i]
    );
  }

  // add rhs values
  for (std::size_t i = 0; i < n_targets; ++i) {
    ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[i]));
  }

  // add sense values
  for (std::size_t i = 0; i < n_targets; ++i) {
    ptr->_rhs.push_back(feature_group_target[feature_group_ids[i]]);
  }

  // add row_ids values
  for (std::size_t i = 0; i < n_targets; ++i) {
    ptr->_row_ids.push_back("spp_target");
  }

  ptr->_modelsense="min";

  // return succes
  return true;
}
