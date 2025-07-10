#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _robust_prioritizr_rcpp_apply_robust_min_set_objective(SEXP, SEXP, SEXP, SEXP);
extern SEXP _robust_prioritizr_rcpp_apply_robust_min_shortfall_objective(SEXP, SEXP, SEXP, SEXP);
extern SEXP _robust_prioritizr_rcpp_apply_robust_probability_constraints(SEXP, SEXP, SEXP, SEXP);
extern SEXP _robust_prioritizr_rcpp_copy_optimization_problem(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_A(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_col_ids(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_compressed_formulation(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_lb(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_modelsense(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_ncell(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_ncol(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_nrow(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_number_of_features(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_number_of_planning_units(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_number_of_zones(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_obj(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_rhs(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_row_ids(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_sense(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_ub(SEXP);
extern SEXP _robust_prioritizr_rcpp_get_optimization_problem_vtype(SEXP);
extern SEXP _robust_prioritizr_rcpp_new_optimization_problem(SEXP, SEXP, SEXP);
extern SEXP _robust_prioritizr_rcpp_optimization_problem_as_list(SEXP);
extern SEXP _robust_prioritizr_rcpp_predefined_optimization_problem(SEXP);
extern SEXP _robust_prioritizr_rcpp_set_optimization_problem_shuffled(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_robust_prioritizr_rcpp_apply_robust_min_set_objective",                    (DL_FUNC) &_robust_prioritizr_rcpp_apply_robust_min_set_objective,                    4},
    {"_robust_prioritizr_rcpp_apply_robust_min_shortfall_objective",              (DL_FUNC) &_robust_prioritizr_rcpp_apply_robust_min_shortfall_objective,              5},
    {"_robust_prioritizr_rcpp_apply_robust_probability_constraints",              (DL_FUNC) &_robust_prioritizr_rcpp_apply_robust_probability_constraints,              4},
    {"_robust_prioritizr_rcpp_copy_optimization_problem",                         (DL_FUNC) &_robust_prioritizr_rcpp_copy_optimization_problem,                         1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_A",                        (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_A,                        1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_col_ids",                  (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_col_ids,                  1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_compressed_formulation",   (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_compressed_formulation,   1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_lb",                       (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_lb,                       1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_modelsense",               (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_modelsense,               1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_ncell",                    (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_ncell,                    1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_ncol",                     (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_ncol,                     1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_nrow",                     (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_nrow,                     1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_number_of_features",       (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_number_of_features,       1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_number_of_planning_units", (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_number_of_planning_units, 1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_number_of_zones",          (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_number_of_zones,          1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_obj",                      (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_obj,                      1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_rhs",                      (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_rhs,                      1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_row_ids",                  (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_row_ids,                  1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_sense",                    (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_sense,                    1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_ub",                       (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_ub,                       1},
    {"_robust_prioritizr_rcpp_get_optimization_problem_vtype",                    (DL_FUNC) &_robust_prioritizr_rcpp_get_optimization_problem_vtype,                    1},
    {"_robust_prioritizr_rcpp_new_optimization_problem",                          (DL_FUNC) &_robust_prioritizr_rcpp_new_optimization_problem,                          3},
    {"_robust_prioritizr_rcpp_optimization_problem_as_list",                      (DL_FUNC) &_robust_prioritizr_rcpp_optimization_problem_as_list,                      1},
    {"_robust_prioritizr_rcpp_predefined_optimization_problem",                   (DL_FUNC) &_robust_prioritizr_rcpp_predefined_optimization_problem,                   1},
    {"_robust_prioritizr_rcpp_set_optimization_problem_shuffled",                 (DL_FUNC) &_robust_prioritizr_rcpp_set_optimization_problem_shuffled,                 2},
    {NULL, NULL, 0}
};

void R_init_robust_prioritizr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
