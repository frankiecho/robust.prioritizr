#include "package.h"
#include "prioritizr_optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_robust_min_shortfall_objective(
  SEXP x,
  const Rcpp::List targets_list,
  const Rcpp::NumericMatrix costs,
  Rcpp::NumericVector budget,
  const Rcpp::IntegerVector feature_group_ids,
  const Rcpp::NumericVector weights
) {
  // Initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_extra_ncol;
  std::size_t A_extra_nrow;

  // Extract targets
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];

  // Compute counters
  const std::size_t n_targets = static_cast<std::size_t>(targets_value.size());
  const std::size_t n_feature_groups = Rcpp::max(feature_group_ids) + 1;

  // The index to start for adding in shortfall and target variables
  if (ptr->_compressed_formulation) {
    A_extra_ncol = 0;
    A_extra_nrow = 0;
  } else {
    A_extra_ncol = ptr->_number_of_zones * ptr->_number_of_planning_units *
      ptr->_number_of_features;
    A_extra_nrow = *(ptr->_A_i.rbegin()) - n_targets + 1;
  }

  // Declare constants
  const std::size_t n_groups = *std::max_element(
    feature_group_ids.begin(), feature_group_ids.end()
  ) + 1;

  // Find cardinality of each group
  std::vector<double> feature_group_cardinality(n_groups, 0.0);
  for (std::size_t i = 0; i < n_targets; ++i) {
    ++feature_group_cardinality[feature_group_ids[i]];
  }

  // Find average weight value for each group
  std::vector<double> feature_group_weight(n_groups, 0.0);
  for (std::size_t i = 0; i < n_targets; ++i) {
    feature_group_weight[feature_group_ids[i]] += weights[i];
  }
  for (std::size_t i = 0; i < n_groups; ++i) {
    feature_group_weight[i] /= feature_group_cardinality[i];
  }

  // Specify model components for planning unit status variables
  /// obj coefficients for planning unit status variables,
  /// also set lb and ub planning units with NA costs to 0
  for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z) {
    for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j) {
      if (Rcpp::NumericMatrix::is_na(costs(j, z))) {
        ptr->_obj.push_back(0.0);
        ptr->_lb[(z * ptr->_number_of_planning_units) + j] = 0.0;
        ptr->_ub[(z * ptr->_number_of_planning_units) + j] = 0.0;
      } else {
        ptr->_obj.push_back(0);
      }
    }
  }
  /// note that constraint matrix coefficients have already been added
  /// for the planning unit status variables denoting the amount
  /// of each feature within each planning unit,
  /// this is done in: priorizir/src/rcpp_add_rij_data.R

  // Specify model components for shortfall variables
  // model constraint matrix coefficients
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_i.push_back(A_extra_nrow + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_j.push_back(
      (ptr->_number_of_zones * ptr->_number_of_planning_units) +
      A_extra_ncol + i
    );
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_x.push_back(targets_value[i]);
  // rhs
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_rhs.push_back(targets_value[i]);
  // sense
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[i]));
  // obj
  if (!ptr->_compressed_formulation)
    for (std::size_t i = 0; i < A_extra_ncol; ++i)
      ptr->_obj.push_back(0.0);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_obj.push_back(0.0);
  // lb
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_ub.push_back(1.0);
  // ub
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_lb.push_back(0.0);
  // vtype
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_vtype.push_back("C");

  // Specify model components for budget constraints
  // model constraint coefficients
  if (budget.size() == 1) {
    for (std::size_t i = 0;
         i < (ptr->_number_of_zones) * (ptr->_number_of_planning_units); ++i)
      ptr->_A_i.push_back(A_extra_nrow + n_targets);
  } else {
    for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z)
      for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j)
        ptr->_A_i.push_back(A_extra_nrow + n_targets + z);
  }
  for (std::size_t i = 0;
       i < (ptr->_number_of_zones) * (ptr->_number_of_planning_units); ++i)
    ptr->_A_j.push_back(i);
  for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z) {
    for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j) {
      ptr->_A_x.push_back(
          Rcpp::NumericMatrix::is_na(costs(j, z)) ? 0 : costs(j, z)
      );
    }
  }
  // rhs
  for (std::size_t z = 0; z < static_cast<std::size_t>(budget.size()); ++z)
    ptr->_rhs.push_back(budget[z]);
  /// sense
  for (std::size_t z = 0; z < static_cast<std::size_t>(budget.size()); ++z)
    ptr->_sense.push_back("<=");

  // Compute indices for adding remaining constraints
  std::size_t A_row_start = A_extra_nrow + n_targets + budget.size();
  std::size_t A_col_start =
    A_extra_ncol +
    ((ptr->_number_of_zones) * (ptr->_number_of_planning_units));

  // Specify model components for robust constraints
  /// model constraint coefficents for target shortfall of the realization
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_i.push_back(A_row_start + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_j.push_back(A_col_start + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_x.push_back(1.0);
  /// model constraint coefficients to ensure that target shortfall of the
  /// realization is larger than the max of the target shortfall
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_i.push_back(A_row_start + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_j.push_back(A_col_start + n_targets + feature_group_ids[i]);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_x.push_back(-1.0);
  // model rhs for previously added constraint coefficients
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_rhs.push_back(0.0);
  // model sense for previously constraint coefficients
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_sense.push_back("<=");
  /// model obj coefficents for the feature group shortfall variable
  for (std::size_t i = 0; i < n_feature_groups; ++i)
    ptr->_obj.push_back(feature_group_weight[i]);
  /// model lb and ub for the feature group shortfall variable
  for (std::size_t i = 0; i < n_feature_groups; ++i)
    ptr->_ub.push_back(1.0);
  for (std::size_t i = 0; i < n_feature_groups; ++i)
    ptr->_lb.push_back(0.0);
  /// model vtype for the feature group shortfall variable
  for (std::size_t i = 0; i < n_feature_groups; ++i)
    ptr->_vtype.push_back("C");

  // add in row and col ids
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_col_ids.push_back("spp_met");
  for (std::size_t i = 0; i < n_feature_groups; ++i)
    ptr->_col_ids.push_back("feature_group");
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_row_ids.push_back("spp_target");
  for (std::size_t i = 0; i < static_cast<std::size_t>(budget.size()); ++i)
    ptr->_row_ids.push_back("budget");
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_row_ids.push_back("feature_targets");

  // Specify model sense
  ptr->_modelsense = "min";

  // Return success
  return true;
}
