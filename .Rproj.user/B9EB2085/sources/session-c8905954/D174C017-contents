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
    add_robust_min_shortfall_objective(budget = 1) |>
    add_constant_robust_constraints(groups = x, conf_level = 0.9) |>
    prioritizr::add_absolute_targets(0.1) |>
    prioritizr::add_binary_decisions()

  # compile problem
  c <- prioritizr::compile(p)

  # run preliminary checks
  expect_s3_class(c, "OptimizationProblem")
  expect_equal(c$modelsense(), "min")
  expect_equal(c$sense(), c(rep(">=", 5), "<=", rep("<=", 5), "<=", "<="))
})

test_that("solve (single zone)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  budget <- as.numeric(terra::global(sim_pu_raster, 'sum', na.rm = T)) * 0.1

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_shortfall_objective(budget = budget) |>
    prioritizr::add_relative_targets(.3) |>
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
})

test_that("compile (multiple zones)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- c(prioritizr::get_sim_pu_raster(),
                     prioritizr::get_sim_pu_raster(),
                     prioritizr::get_sim_pu_raster())

  budget <- as.numeric(terra::global(sim_pu_raster[[1]], 'sum', na.rm = T)) * 0.1
  sim_features <- prioritizr::get_sim_features()
  sim_zones <-
    prioritizr::zones(a = sim_features, b = sim_features, c = sim_features)
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_zones) |>
    add_robust_min_shortfall_objective(budget = budget) |>
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
    add_robust_min_shortfall_objective(budget = c(1, 1, 1)) |>
    prioritizr::add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) |>
    prioritizr::add_binary_decisions() |>
    add_constant_robust_constraints(groups = x) |>
    prioritizr::add_default_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run preliminary checks
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 3)
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
  expect_error(add_robust_min_shortfall_objective(p, budget = -5))
  expect_error(add_robust_min_shortfall_objective(p, budget = c(0.1, 0.2)))
})
