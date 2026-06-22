#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0, G1.1, G1.2, G1.3, G1.4, G1.4a} Documented in package.R; algorithms referenced and documented with roxygen2.
#' @srrstats {G2.0, G2.0a, G2.1, G2.1a} Type and length checking enforced in add_constant_robust_constraints.R, add_robust_min_set_objective.R, and add_variable_robust_constraints.R via assertthat.
#' @srrstats {G2.2, G2.3, G2.3a, G2.3b} Univariate character input restricted via match.arg() equivalents; case-handling with tolower().
#' @srrstats {G2.6, G2.7, G2.8, G2.9} Input preprocessing and class handling documented in transform_targets.R and function implementations.
#' @srrstats {G2.13, G2.14a, G2.15} Missing data and NA handling checked in input validation; errors on invalid data.
#' @srrstats {G3.0} No floating-point equality comparisons; solver tolerance respected.
#' @srrstats {G5.0, G5.1, G5.2, G5.2a, G5.2b, G5.3, G5.4, G5.4a} Tests use prioritizr standard datasets; snapshot and correctness tests documented in test files.
#' @srrstats {G5.6, G5.6a} Parameter recovery tests confirm solve() output meets specified targets within solver tolerance.
#' @srrstats {SP1.0, SP1.1, SP2.0, SP2.0a, SP2.1, SP2.2, SP2.2a, SP2.2b, SP2.3, SP2.4, SP2.4a, SP2.5} Spatial domain, data classes, and PROJ/WKT2 compliance documented in package.R and data.R.
#' @srrstats {SP2.6, SP2.7} Input types documented in roxygen; validation via is_conservation_problem().
#' @srrstats {SP4.0, SP4.0a, SP4.1, SP4.2} Return values in SpatRaster class matching input; units maintained via terra.
#' @noRd
NULL

#' NA_standards
#'
#' Standards not applicable to this package.
#'
#' @srrstatsNA {G1.5} Reproducibility code provided through vignettes (climate-sdm and vic-cons-planning).
#' @srrstatsNA {G1.6} Comparative performance demonstrated through vignettes integrating with prioritizr.
#' @srrstatsNA {G2.4, G2.4a, G2.4b, G2.4c, G2.4d, G2.4e} Type conversion out of scope; input data must be in standard spatial formats (sf/SpatRaster) per prioritizr.
#' @srrstatsNA {G2.5} Factor handling not applicable; package works with numeric spatial data and character identifiers.
#' @srrstatsNA {G2.10, G2.11, G2.12} Tabular column extraction not core functionality; operations on spatial objects via prioritizr.
#' @srrstatsNA {G2.14b, G2.14c} NA handling options delegated to prioritizr and input data validation.
#' @srrstatsNA {G2.16} Undefined values (NaN, Inf) handled by optimization solver in prioritizr.
#' @srrstatsNA {G3.1, G3.1a} Covariance algorithms handled by prioritizr's solver, not this package.
#' @srrstatsNA {G4.0} Package does not write files; operates on in-memory ConservationProblem objects.
#' @srrstatsNA {G5.4b, G5.4c} Vignette provides comparison with existing prioritizr implementations.
#' @srrstatsNA {G5.5, G5.7} Extended testing managed in tests/testthat/ directory with appropriate documentation.
#' @srrstatsNA {G5.6b, G5.9, G5.9a, G5.9b, G5.10, G5.11, G5.11a, G5.12} Extended test suite configuration managed in tests/.
#' @srrstatsNA {SP2.5a, SP2.8, SP2.9} Spatial input validation and preprocessing inherited from prioritizr, terra, and sf packages.
#' @srrstatsNA {SP3.0, SP3.0a, SP3.0b, SP3.1, SP3.2, SP3.3, SP3.4, SP3.5, SP3.6} Spatial neighbor handling and weighting inherited from prioritizr.
#' @srrstatsNA {SP4.0b} Return class determined by prioritizr's ConservationProblem.
#' @srrstatsNA {SP5.0, SP5.1, SP5.2, SP5.3} Plotting and visualization provided by prioritizr.
#' @srrstatsNA {SP6.0, SP6.1, SP6.1a, SP6.1b, SP6.2, SP6.3, SP6.4, SP6.5, SP6.6} Coordinate transformation and geographic testing inherited from terra and sf packages.
NULL
