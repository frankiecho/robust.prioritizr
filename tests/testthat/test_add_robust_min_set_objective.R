library(prioritizr)
library(terra)

test_that("compile (single zone)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective() |>
    add_constant_robust_constraints(groups = x) |>
    prioritizr::add_absolute_targets(0.1) |>
    prioritizr::add_binary_decisions()

  # compile problem
  c <- prioritizr::compile(p)

  # run preliminary checks
  expect_s3_class(c, "OptimizationProblem")
  expect_equal(c$modelsense(), "min")
  expect_equal(c$rhs(), rep(0.1, 5))
  expect_equal(c$sense(), rep(">=", 5))
})

test_that("solve (single zone)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective() |>
    prioritizr::add_relative_targets(0.1) |>
    add_constant_robust_constraints(groups = x) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_default_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run preliminary checks
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 1)
  expect_gt(terra::global(s, "min", na.rm = TRUE)[[1]], -1e-5)
  expect_lt(terra::global(s, "max", na.rm = TRUE)[[1]], 1 + 1e-5)
  expect_true(
    all(
      prioritizr::eval_feature_representation_summary(p, s)$relative_held >= 0.1
    )
  )

  conf_level <- 0.5

  # Test for chance constraints
  sim_features <- rep(prioritizr::get_sim_features(), 5)
  names(sim_features) <- paste0("feature_", 1:terra::nlyr(sim_features))
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = 'CondValueAtRisk') |>
    prioritizr::add_absolute_targets(5) |>
    add_constant_robust_constraints(groups = x, conf_level = conf_level) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_default_solver(verbose = FALSE)

  s <- solve(p)
  summary_eval <- prioritizr::eval_feature_representation_summary(p, s)
  expect_lte(mean(summary_eval$absolute_held[x=='a'] < 5), conf_level)
  expect_lte(mean(summary_eval$absolute_held[x=='b'] < 5), conf_level)

  # Test for conditional value-at-risk
  sim_features <- rep(prioritizr::get_sim_features(), 5)
  names(sim_features) <- paste0("feature_", 1:terra::nlyr(sim_features))
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = 'CondValueAtRisk') |>
    prioritizr::add_absolute_targets(5) |>
    add_constant_robust_constraints(groups = x, conf_level = conf_level) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_default_solver(verbose = FALSE)

  s <- solve(p)
  summary_eval <- prioritizr::eval_feature_representation_summary(p, s)
  expect_lte(mean(summary_eval$absolute_held[x=='a'] < 5), conf_level)
  expect_lte(mean(summary_eval$absolute_held[x=='b'] < 5), conf_level)

})

test_that("compile (multiple zones)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- c(prioritizr::get_sim_pu_raster(),
                     prioritizr::get_sim_pu_raster(),
                     prioritizr::get_sim_pu_raster())
  sim_features <- prioritizr::get_sim_features()
  sim_zones <-
    prioritizr::zones(a = sim_features, b = sim_features, c = sim_features)
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_zones) |>
    add_robust_min_set_objective() |>
    add_constant_robust_constraints(groups = x) |>
    prioritizr::add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) |>
    prioritizr::add_binary_decisions()

  # compile problem
  c <- prioritizr::compile(p)

  # run preliminary checks
  expect_s3_class(c, "OptimizationProblem")
  expect_equal(c$modelsense(), "min")
})

test_that("solve (multiple zones)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- c(prioritizr::get_sim_pu_raster(),
                     prioritizr::get_sim_pu_raster(),
                     prioritizr::get_sim_pu_raster())
  sim_features <- prioritizr::get_sim_features()
  sim_zones <-
    prioritizr::zones(a = sim_features, b = sim_features, c = sim_features)
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_zones) |>
    add_robust_min_set_objective() |>
    prioritizr::add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) |>
    prioritizr::add_binary_decisions() |>
    add_constant_robust_constraints(groups = x) |>
    prioritizr::add_default_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run preliminary checks
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 3)
  expect_true(
    all(
      prioritizr::eval_feature_representation_summary(p, s)$relative_held >= 0.1
    )
  )
})

test_that("invalid arguments", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features)

  # tests
  expect_error(add_robust_min_set_objective(p, budget = -5))
  expect_error(add_robust_min_set_objective(p, budget = c(0.1, 0.2)))
})
