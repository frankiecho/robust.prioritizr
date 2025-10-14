test_that("compile (single zone, conf_level < 1, method = chance)", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # define conf level
  conf_level <- 0.9

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(0.1) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p)

  # compute values for tests
  group_cardinality <- unname(table(x))
  A_row_sum <- rowSums(as.matrix(o$A()))

  # run tests
  ## expect the length of rhs to be equal to 7
  expect_length(o$rhs(), 7)
  ## expect the rhs length to be equal to the second dimension of A
  expect_equal(nrow(o$A()), length(o$rhs()))
  ## expect the row sums to be less than the cardinality of the group
  expect_true(all((group_cardinality - A_row_sum[6:7]) > -1e-5))
  ## expect the confidence level to be reflected in the RHS
  expect_true(
    all(
      (group_cardinality * (1 - conf_level) - o$rhs()[6:7]) > 1e-5
    )
  )
})

test_that("solve (single zone, conf_level < 1, method = chance)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")

  # set seed
  set.seed(500)

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define parameters for prioritization
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))
  conf_level <- 0.6
  target <- 0.1

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(target) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # calculate values for tests
  feature_rep <- prioritizr::eval_feature_representation_summary(p, s)
  feature_target_a <- feature_rep$relative_held[x == "a"] >= target
  feature_target_b <- feature_rep$relative_held[x == "b"] >= target

  # run tests
  expect_gte(mean(feature_target_a), conf_level)
  expect_gte(mean(feature_target_b), conf_level)
})

test_that("solve (single zone, conf_level < 1, method = cvar)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define parameters for prioritization
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))
  conf_level <- 0.6
  target <- 0.1

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "cvar") |>
    prioritizr::add_relative_targets(target) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # calculations for tests
  feature_rep <- prioritizr::eval_feature_representation_summary(p, s)
  feature_target_a <- feature_rep$relative_held[x == "a"] >= target
  feature_target_b <- feature_rep$relative_held[x == "b"] >= target

  # run tests
  expect_gte(mean(feature_target_a), conf_level)
  expect_gte(mean(feature_target_b), conf_level)
})

test_that("compile (multiple zones, conf_level < 1, method = chance)", {
  # define skip cases
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define parameters for prioritization
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))
  targets <- matrix(NA, ncol = 2, nrow = 5)
  targets[, 1] <- 0.1
  targets[, 2] <- 0.05
  conf_level <- 0.6

  # build problem
  p <-
    prioritizr::problem(
      sim_pu_raster[[rep(1, 2)]],
      prioritizr::zones(z1 = sim_features, z2 = sim_features)
    ) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(targets) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p)

  # run tests
  expect_equal(nrow(o$A()), length(o$row_ids()))
  expect_equal(nrow(o$A()), length(o$rhs()))
  expect_equal(ncol(o$A()), length(o$obj()))
})

test_that("solve (multiple zones)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define parameters for prioritization
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))
  targets <- matrix(NA, ncol = 2, nrow = 5)
  targets[, 1] <- 0.1
  targets[, 2] <- 0.05
  conf_level <- 0.6

  # build problem
  p <-
    prioritizr::problem(
      sim_pu_raster[[rep(1, 2)]],
      prioritizr::zones(z1 = sim_features, z2 = sim_features)
    ) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(targets) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve proble
  s <- solve(p)

  # run tests
  expect_lte(
    max(terra::values(s[[1]] + s[[2]]), na.rm = TRUE),
    1
  )
})

test_that("invalid arguments", {
  # define skip cases
  skip_if_not_installed("terra")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define parameters for prioritization
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))
  targets <- matrix(NA, ncol = 2, nrow = 5)
  targets[, 1] <- 0.1
  targets[, 2] <- 0.05
  conf_level <- 0.6

  # initialize problem
  p <- prioritizr::problem(
    sim_pu_raster[[rep(1, 2)]],
    prioritizr::zones(z1 = sim_features, z2 = sim_features)
  )

  # run tests
  ## groups not specified
  expect_error(
    add_constant_robust_constraints(p, conf_level = 0.1),
    "`groups` is absent"
  )
  ## conf_level is a vector
  expect_error(
    add_constant_robust_constraints(p, conf_level = c(0.1, 0.5), groups = x),
    "is not a number"
  )
 ## conf_level is greater than 1
  expect_error(
    add_constant_robust_constraints(p, conf_level = 1.1, groups = x),
    "not less than or equal to 1"
  )
  ## conf_level is less than 0
  expect_error(
    add_constant_robust_constraints(p, conf_level = -0.1, groups = x),
    "not greater than or equal to 0"
  )
  ## groups is a vector that has a different length to the number of features
  expect_error(
    add_constant_robust_constraints(
      p,
      groups = c("a", "a", "b", "d", "d", "d")
    ),
    "must specify a value for each feature"
  )
  ## some groups do not contain at least two features
  expect_error(
    add_constant_robust_constraints(p, groups = c(rep("a", 4), "d")),
    "must have at least two features"
  )
})
