#include "package.h"
#include "prioritizr_optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_robust_probability_constraints(
    SEXP x,
    const Rcpp::List targets_list,
    const Rcpp::IntegerVector feature_group_ids,
    const Rcpp::NumericVector conf_levels
) {
  // Initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];
  const std::size_t n_targets = static_cast<std::size_t>(targets_value.size());

  // Store counter variables with problem size
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // Calculate number of feature groups
  const std::size_t n_groups = *std::max_element(
    feature_group_ids.begin(), feature_group_ids.end()
  ) + 1;

  // Find cardinality of each feature group
  std::vector<double> feature_group_cardinality(n_groups, 0.0);
  for (std::size_t i = 0; i < n_targets; ++i) {
    ++feature_group_cardinality[feature_group_ids[i]];
  }

  // Calculate values for big m constraints
  std::vector<double> big_m(n_targets, 0.0);
  double scale_coef;
  for (std::size_t i = 0; i < n_targets; ++i) {
    scale_coef =
      (targets_sense[i] == "<=") || (targets_sense[i] == "<") ? -1.0 : 1.0;
    big_m[i] = scale_coef * targets_value[i];
  }

  // Pre-compute the rhs of the probability of violation constraint first
  // If the rhs is smaller than 1 (due to a low number of realizations in
  // the feature group), as the sum of the lhs is integer, the lhs will all be
  // zero, meaning no constraint can be violated. This can improve efficiency.
  std::vector<double> conf_levels_rhs(n_groups, 0.0);
  std::vector<bool> apply_prob_constraint(n_groups, false);
  for (std::size_t i = 0; i < n_groups; ++i) {
    // RHS of the constraint
    conf_levels_rhs[i] = (1.0 - conf_levels[i]) * feature_group_cardinality[i];

    // If the RHS is smaller than 1, there is no other solution on the LHS other
    // than have everything 0. This means the constraint/ variable is redundant
    // and can be safely omitted
    apply_prob_constraint[i] = conf_levels_rhs[i] >= 1.0;
  }

  // Add to the constraint matrix and create new objective values
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_i.push_back(i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_j.push_back(A_original_ncol + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_x.push_back(big_m[i]);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_obj.push_back(0.0);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_col_ids.push_back("big_m");
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_vtype.push_back("B");
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_ub.push_back(1);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_lb.push_back(0);

  // Add in the constraint to ensure that the sum of the violations do not
  // exceed the probability
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_i.push_back(A_original_nrow + feature_group_ids[i]);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_j.push_back(A_original_ncol + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_x.push_back(1.0);
  /// note that if we don't apply probability constraint, then set this to 0
  /// to reduce solve times
  for (std::size_t i = 0; i < n_groups; ++i)
    ptr->_rhs.push_back(apply_prob_constraint[i] ? conf_levels_rhs[i] : 0.0);
  for (std::size_t i = 0; i < n_groups; ++i)
    ptr->_row_ids.push_back("conf_levels");
  for (std::size_t i = 0; i < n_groups; ++i)
    ptr->_sense.push_back("<=");

  // Return success
  return true;
}
