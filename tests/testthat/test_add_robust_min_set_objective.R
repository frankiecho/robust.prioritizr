test_that("compile (single zone, conf_level < 1, method = chance)", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # define conf level
  conf_level <- 0.5

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    add_constant_robust_constraints(groups = x, conf_level = conf_level) |>
    prioritizr::add_absolute_targets(0.1) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p)

  # compute values for tests
  group_cardinality <- c(unname(table(x)))
  A_row_sum <- rowSums(as.matrix(o$A()))

  # run tests
  expect_s3_class(o, "OptimizationProblem")
  expect_snapshot(as.list(o))
  ## expect total number of constraints to be equal to:
  ## number of features + number of feature groups
  expect_length(o$rhs(), length(x) + length(unique(x)))
  ## expect the row sums of the A matrix for the chance constraints
  ## to be equal to the cardinality of the group
  expect_equal(group_cardinality, A_row_sum[6:7])
  ## expect the confidence level to be reflected in the RHS
  expect_equal(
    group_cardinality * (1 - conf_level),
    o$rhs()[6:7]
  )
})

test_that("solve (single zone, conf_level = 1, method = chance)", {
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
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(0.1) |>
    add_constant_robust_constraints(groups = x, conf_level = 1) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run preliminary tests
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 1)
  expect_gte(terra::global(s, "min", na.rm = TRUE)[[1]], 0)
  expect_lte(terra::global(s, "max", na.rm = TRUE)[[1]], 1)
  expect_true(
    all(
      prioritizr::eval_feature_representation_summary(p, s)$relative_held >= 0.1
    )
  )
})

test_that("solve (single zone, conf_level < 1, method = chance)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # prepare features and define feature groupings
  sim_features <- rep(prioritizr::get_sim_features(), 5)
  names(sim_features) <- paste0("feature_", seq_len(terra::nlyr(sim_features)))
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # define conf level
  conf_level <- 0.5

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_absolute_targets(5) |>
    add_constant_robust_constraints(groups = x, conf_level = conf_level) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run tests for feature representation
  summary_eval <- prioritizr::eval_feature_representation_summary(p, s)
  expect_lte(mean(summary_eval$absolute_held[x == "a"] < 5), conf_level)
  expect_lte(mean(summary_eval$absolute_held[x == "b"] < 5), conf_level)
})

test_that("compile (single zone, conf_level < 1, method = cvar)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # prepare features and define feature groupings
  sim_features <- rep(prioritizr::get_sim_features(), 5)
  names(sim_features) <- paste0("feature_", seq_len(terra::nlyr(sim_features)))
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "cvar") |>
    prioritizr::add_absolute_targets(5) |>
    add_constant_robust_constraints(groups = x, conf_level = 0.5) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # compile problem
  o <- prioritizr::compile(p)

  # run tests
  expect_s3_class(o, "OptimizationProblem")
  expect_snapshot(as.list(o))
})

test_that("solve (single zone, conf_level < 1, method = cvar)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # prepare features and define feature groupings
  sim_features <- rep(prioritizr::get_sim_features(), 5)
  names(sim_features) <- paste0("feature_", seq_len(terra::nlyr(sim_features)))
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # define conf level
  conf_level <- 0.5

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "cvar") |>
    prioritizr::add_absolute_targets(5) |>
    add_constant_robust_constraints(groups = x, conf_level = conf_level) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run tests for feature representation
  summary_eval <- prioritizr::eval_feature_representation_summary(p, s)
  expect_lte(mean(summary_eval$absolute_held[x == "a"] < 5), conf_level)
  expect_lte(mean(summary_eval$absolute_held[x == "b"] < 5), conf_level)
})

test_that("compile (multiple zones, conf_level = 1, method = chance)", {
  # define skip cases
  skip_if_not_installed("terra")

  # create data with 3 zones and 5 features
  grid_raster <- terra::rast(matrix(rep(1, 25), nrow = 5))
  sim_pu_raster <- grid_raster[[rep(1, 3)]] * seq_len(3)
  sim_features <- grid_raster[[rep(1, 5)]] * seq_len(5)
  sim_zones <-
    prioritizr::zones(a = sim_features, b = sim_features, c = sim_features)
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_zones) |>
    add_robust_min_set_objective(method = "chance") |>
    add_constant_robust_constraints(groups = x, conf_level = 1) |>
    prioritizr::add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p)

  # run tests
  expect_s3_class(o, "OptimizationProblem")
  expect_snapshot(as.list(o))
  expect_true(
    all(
      apply(as.matrix(o$A()), 1, sum)[seq_len(15)] >
      o$rhs()[seq_len(15)]
    )
  )
})

test_that("solve (multiple zones, conf_level = 1, method = chance)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- terra::rast(list(
    prioritizr::get_sim_pu_raster(),
    prioritizr::get_sim_pu_raster(),
    prioritizr::get_sim_pu_raster()
  ))
  sim_features <- prioritizr::get_sim_features()
  sim_zones <-
    prioritizr::zones(a = sim_features, b = sim_features, c = sim_features)
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_zones) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) |>
    prioritizr::add_binary_decisions() |>
    add_constant_robust_constraints(groups = x, conf_level = 1) |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run tests
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 3)
  expect_true(
    all(
      prioritizr::eval_feature_representation_summary(p, s)$relative_held >= 0.1
    )
  )
})

test_that("invalid arguments", {
  # define skip cases
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # build problem
  p <- prioritizr::problem(sim_pu_raster, sim_features)

  # run tests
  expect_error(
    add_robust_min_set_objective(p, budget = -5),
    "unused"
  )
  expect_error(
    add_robust_min_set_objective(p, budget = c(0.1, 0.2)),
    "unused"
  )
  expect_error(
    add_robust_min_set_objective(p, method = "invalid"),
    "method"
  )
  expect_error(
    add_robust_min_set_objective(p, method = 1),
    "method"
  )
  expect_error(
    add_robust_min_set_objective(p, method = c("cvar", "chance")),
    "is not a string"
  )
})
