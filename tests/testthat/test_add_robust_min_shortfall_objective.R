test_that("compile (single zone)", {
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
  o <- prioritizr::compile(p)

  # run tests
  expect_s3_class(o, "OptimizationProblem")
  expect_equal(o$modelsense(), "min")
  expect_equal(o$sense(), c(rep(">=", 5), "<=", rep("<=", 5), "<=", "<="))
})

test_that("solve (single zone)", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define prioritization parameters
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))
  budget <- as.numeric(terra::global(sim_pu_raster, "sum", na.rm = TRUE)) * 0.1

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_shortfall_objective(budget = budget) |>
    prioritizr::add_relative_targets(0.3) |>
    add_constant_robust_constraints(groups = x) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run tests
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 1)
  expect_gt(terra::global(s, "min", na.rm = TRUE)[[1]], -1e-5)
  expect_lt(terra::global(s, "max", na.rm = TRUE)[[1]], 1 + 1e-5)
})

test_that("compile (multiple zones)", {
  # define skip cases
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define parameters for prioritization
  budget <- as.numeric(
    terra::global(sim_pu_raster, "sum", na.rm = T)
  ) * 0.1
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(
      sim_pu_raster[[rep(1, 3)]],
      prioritizr::zones(sim_features, sim_features, sim_features)
    ) |>
    add_robust_min_shortfall_objective(budget = budget) |>
    add_constant_robust_constraints(groups = x) |>
    prioritizr::add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p)

  # run tests
  expect_s3_class(o, "OptimizationProblem")
  expect_equal(o$modelsense(), "min")
})

test_that("solve (multiple zones)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(
      sim_pu_raster[[rep(1, 3)]],
      prioritizr::zones(sim_features, sim_features, sim_features)
    ) |>
    add_robust_min_shortfall_objective(budget = c(1, 1, 1)) |>
    prioritizr::add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) |>
    prioritizr::add_binary_decisions() |>
    add_constant_robust_constraints(groups = x) |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run tests
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 3)
})

test_that("invalid arguments", {
  # import datad
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # build problem
  p <- prioritizr::problem(sim_pu_raster, sim_features)

  # run tests
  expect_error(
    add_robust_min_shortfall_objective(p, budget = -5),
    "budget"
  )
  expect_error(
    add_robust_min_shortfall_objective(p, budget = c(0.1, 0.2)),
    "budget"
  )
})
