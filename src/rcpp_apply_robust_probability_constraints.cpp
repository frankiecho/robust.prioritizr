#include "package.h"
#include "prioritizr_optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_robust_probability_constraints(
    SEXP x,
    const Rcpp::List targets_list,
    const Rcpp::IntegerVector feature_group_ids,
    const Rcpp::NumericVector conf_levels
) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];
  const std::size_t n_targets = static_cast<std::size_t>(targets_value.size());

  // Adds probability constraints to the min set objective function or the min shortfall functions

  std::size_t A_extra_ncol;
  std::size_t A_extra_nrow;

  // declare constants
  const std::size_t n_groups = *std::max_element(
    feature_group_ids.begin(), feature_group_ids.end()
  ) + 1;

  // Find feature group cardinality
  Rcpp::NumericVector feature_group_cardinality(n_groups, 0.0);
  for (std::size_t i = 0; i < n_targets; ++i) {
    ++feature_group_cardinality[feature_group_ids[i]];
  }



  // Determine the current A matrix size and work out the index to start from
  A_extra_nrow = static_cast<std::size_t>(*std::max_element(ptr->_A_i.begin(), ptr->_A_i.end())) + 1;
  A_extra_ncol = ptr->_number_of_zones * ptr->_number_of_planning_units;

  // Find maximum target for each group
  Rcpp::NumericVector feature_group_target(
      n_groups, std::numeric_limits<double>::lowest()
  );
  for (std::size_t i = 0; i < n_targets; ++i) {
    feature_group_target[feature_group_ids[i]] = std::max(
      targets_value[feature_group_ids[i]], targets_value[i]
    );
  }

  // Initialise new variables to allow for violations to the robust constraints
  // Checks for the sense of the targets list and assigns the sign accordingly
  Rcpp::NumericVector big_m(n_targets, 0.0);

  for (std::size_t i = 0; i < n_targets; ++i) {
    if ((targets_sense[i] == "<=") || (targets_sense[i] == "<")) {
      big_m[i] = -feature_group_target[feature_group_ids[i]];
    } else {
      big_m[i] = feature_group_target[feature_group_ids[i]];
    }
  }

  // Pre-compute the rhs of the probability of violation constraint first
  // If the rhs is smaller than 1 (due to a low number of realizations in
  // the feature group), as the sum of the lhs is integer, the lhs will all be
  // zero, meaning no constraint can be violated. This can improve efficiency.
  Rcpp::NumericVector conf_levels_rhs(n_groups, 0.0);
  Rcpp::LogicalVector apply_prob_constraint(n_groups, FALSE);

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
    ptr->_A_j.push_back(A_extra_ncol + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_x.push_back(big_m[i]);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_obj.push_back(0.0);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_col_ids.push_back("big_m");
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_vtype.push_back("B");

  // If we don't apply probability constraint set this to 0 to reduce solve times
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_ub.push_back(1);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_lb.push_back(0);

  // Add in the constraint to ensure that the sum of the violations do not
  // exceed the probability
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_i.push_back(A_extra_nrow + feature_group_ids[i]);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_j.push_back(A_extra_ncol + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_x.push_back(1.0);

  for (std::size_t i = 0; i < n_groups; ++i)
    ptr->_rhs.push_back(apply_prob_constraint[i] ? conf_levels_rhs[i] : 0);
  for (std::size_t i = 0; i < n_groups; ++i)
    ptr->_row_ids.push_back("conf_levels");
  for (std::size_t i = 0; i < n_groups; ++i)
    ptr->_sense.push_back("<=");

  return true;
}
