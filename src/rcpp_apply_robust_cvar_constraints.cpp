#include "package.h"
#include "prioritizr_optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_robust_cvar_constraints(
    SEXP x,
    const Rcpp::List targets_list,
    const Rcpp::IntegerVector feature_group_ids,
    const Rcpp::NumericVector conf_levels // Used to derive beta_levels (1 - conf_levels)
) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);

  // --- Input Extraction ---
  // `targets_value` contains the T_jk targets for each k_idx (realization).
  Rcpp::NumericVector T_jk_original_values = targets_list["value"];
  const std::size_t n_realizations = static_cast<std::size_t>(T_jk_original_values.size());

  // --- Input Validation ---
  if (n_realizations == 0) {
    Rcpp::stop("No targets (realizations) provided. CVaR constraints require at least one realization.");
  }

  const std::size_t n_groups = *std::max_element(
    feature_group_ids.begin(), feature_group_ids.end()
  ) + 1;

  if (static_cast<std::size_t>(conf_levels.size()) != n_groups) {
    Rcpp::stop("Mismatch between number of feature groups (%d) and confidence levels provided (%d).", n_groups, static_cast<std::size_t>(conf_levels.size()));
  }

  // --- CVaR Specific Calculations ---

  // Calculate `K_j`, the cardinality (number of realizations) for each feature group `j`.
  Rcpp::NumericVector feature_group_cardinality(n_groups, 0.0);
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx) {
    feature_group_cardinality[static_cast<std::size_t>(feature_group_ids[k_idx])]++;
  }

  // Derive `T_j`, the CVaR target for each group `j`.
  // This will be the RHS of the main CVaR constraint.
  // Using the minimum of the original T_jk values within each group.
  Rcpp::NumericVector cvar_main_targets_T_j(n_groups, std::numeric_limits<double>::infinity());
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx) {
    std::size_t group_idx = static_cast<std::size_t>(feature_group_ids[k_idx]);
    cvar_main_targets_T_j[group_idx] = std::min(cvar_main_targets_T_j[group_idx], T_jk_original_values[k_idx]);
  }

  // Derive `beta_levels` (tail probability, $\beta$) from `conf_levels` (confidence level, $1-\alpha$).
  Rcpp::NumericVector beta_levels(n_groups);
  for (std::size_t j = 0; j < n_groups; ++j) {
    beta_levels[j] = 1.0 - conf_levels[j];
    if (beta_levels[j] <= 0.0 || beta_levels[j] >= 1.0) {
      Rcpp::stop("Calculated beta_level (%.4f) for group %d is out of (0,1) range. Please ensure confidence levels result in beta values > 0 and < 1 for proper CVaR interpretation.", beta_levels[j], j);
    }
  }

  // --- Determine Starting Indices for New Variables ---
  // These will be appended after all existing columns (which include x_i variables).
  std::size_t current_col_idx = 0;
  if (!ptr->_A_j.empty()) {
    current_col_idx = static_cast<std::size_t>(*std::max_element(ptr->_A_j.begin(), ptr->_A_j.end())) + 1;
  }

  // --- Add New Variables: `eta_j` and `s_jk` ---
  // 1. `eta_j` variables (VaR level)
  std::size_t col_start_eta = current_col_idx;
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_obj.push_back(0.0); // Not directly in objective for this formulation
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_col_ids.push_back("cvar_eta_" + std::to_string(j));
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_vtype.push_back("C"); // Continuous
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_lb.push_back(-std::numeric_limits<double>::infinity()); // Unconstrained lower bound
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_ub.push_back(std::numeric_limits<double>::infinity()); // Unconstrained upper bound
  current_col_idx += n_groups;

  // 2. `s_jk` variables (shortfall)
  std::size_t col_start_s = current_col_idx;
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_obj.push_back(0.0); // Not directly in objective
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_col_ids.push_back("cvar_s_" + std::to_string(k_idx));
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_vtype.push_back("C"); // Continuous
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_lb.push_back(0.0); // s_jk >= 0
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_ub.push_back(std::numeric_limits<double>::infinity());
  current_col_idx += n_realizations;

  // --- REUSE/MODIFY FIRST n_realizations CONSTRAINTS AS CVaR LINKING CONSTRAINTS ---
  // These are the constraints (rows 0 to n_realizations-1) that were originally
  // set up by rcpp_apply_robust_min_set_objective with T_jk targets.
  // We now modify them to become the CVaR linking constraints: (sum_i r_ijk * x_i) - eta_j + s_jk >= 0
  // for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
  //   std::size_t group_idx = static_cast<std::size_t>(feature_group_ids[k_idx]);

  // 1. Modify RHS to 0.0 and sense to ">=".
  //    The existing r_ijk * x_i coefficients for this row are already in place and remain.
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx) {
    // Rcout << "The value of k:" << k_idx << "\n";
    ptr->_rhs[k_idx] = 0.0;
  }
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_sense[k_idx] = ">=";
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_row_ids[k_idx] = "spp_target"; // Rename for clarity

  // 2. Add coefficients for the new variables (eta_j and s_jk) to THIS EXISTING row.
  //    We append new entries to _A_i, _A_j, _A_x, associating them with the current row_index (k_idx).

  // Add coefficient for `s_jk`: `+1.0`
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_A_i.push_back(k_idx); // Row index is the existing k_idx
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_A_j.push_back(static_cast<int>(col_start_s + k_idx)); // Column index for s_jk
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_A_x.push_back(1.0);

  // Add coefficient for `eta_j`: `-1.0`
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_A_i.push_back(k_idx); // Row index is the existing k_idx
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx) {
    std::size_t group_idx = static_cast<std::size_t>(feature_group_ids[k_idx]);
    ptr->_A_j.push_back(static_cast<int>(col_start_eta + group_idx)); // Column index for eta_j
  }

  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx)
    ptr->_A_x.push_back(-1.0);

  // Determine starting row for the *next* set of constraints (Main CVaR Constraints).
  // This will be exactly n_realizations, as we've modified rows 0 to n_realizations-1.
  std::size_t current_row_idx = n_realizations;


  // --- Add New MAIN CVaR Constraints ---
  std::size_t row_start_cvar_main = current_row_idx;
  Rcpp::NumericVector coeff_s_sum(n_groups);

  for (std::size_t j = 0; j < n_groups; ++j) {
    double beta_val = beta_levels[j];
    double K_j = feature_group_cardinality[j];
    // double T_j_target = cvar_main_targets_T_j[j]; // The derived CVaR target for this group

    if (beta_val <= 0.0 || K_j <= 0.0) {
      Rcpp::stop("Invalid beta_level (%.4f) or feature_group_cardinality (%.0f) for group %d. Check your input data.", beta_val, K_j, j);
    }

    coeff_s_sum[j] = -1.0 / beta_val * K_j;
  }

  // Add coefficient for `eta_j`: `+1.0`
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_A_i.push_back(static_cast<int>(row_start_cvar_main + j));
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_A_j.push_back(static_cast<int>(col_start_eta + j));
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_A_x.push_back(1.0);

  // Add coefficients for the sum of `s_jk` terms in this group
  // Optimized: Iterate only through k_idx values known to be in group j
  std::vector<std::vector<std::size_t>> group_realizations(n_groups);
  for (std::size_t k_idx = 0; k_idx < n_realizations; ++k_idx) {
    group_realizations[static_cast<std::size_t>(feature_group_ids[k_idx])].push_back(k_idx);
  }
  for (std::size_t j = 0; j < n_groups; ++j) {
    for (std::size_t k_in_group_idx : group_realizations[j]) {
      ptr->_A_i.push_back(static_cast<int>(row_start_cvar_main + j));
      ptr->_A_j.push_back(static_cast<int>(col_start_s + k_in_group_idx));
      ptr->_A_x.push_back(coeff_s_sum[j]);
    }
  }

  // Set RHS, sense, and ID for the main CVaR constraint
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_rhs.push_back(cvar_main_targets_T_j[j]);
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_row_ids.push_back("cvar_main_j" + std::to_string(j));
  for (std::size_t j = 0; j < n_groups; ++j)
    ptr->_sense.push_back(">=");

  return true;
}
