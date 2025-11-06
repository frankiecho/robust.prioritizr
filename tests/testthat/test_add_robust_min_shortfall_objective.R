test_that("compile (single zone, compressed formulation, conf_level < 1)", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # define conf level
  conf_level <- 0.1

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_shortfall_objective(budget = 1) |>
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
  ## expect obj to have default weights of 1 for each feature group
  expect_equal(
    o$obj(),
    c(
      rep(0, p$number_of_planning_units()),
      rep(0, p$number_of_features()),
      rep(1, length(unique(x))),
      rep(0, p$number_of_features())
    )
  )
  ## expect total number of constraints to be equal to:
  ## (number of features * 2) + number of feature groups + 1
  expect_length(o$rhs(), (length(x) * 2) + length(unique(x)) + 1)
  ## expect the row sums of the A matrix for the chance constraints
  ## to be equal to the cardinality of the group
  expect_equal(group_cardinality, A_row_sum[12:13])
  ## expect the confidence level to be reflected in the RHS
  expect_equal(
    group_cardinality * (1 - conf_level),
    o$rhs()[12:13]
  )
})

test_that("compile (single zone, expanded formulation, conf_level < 1)", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # define conf level
  conf_level <- 0.1

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_shortfall_objective(budget = 1) |>
    add_constant_robust_constraints(groups = x, conf_level = conf_level) |>
    prioritizr::add_absolute_targets(0.1) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p, compressed_formulation = FALSE)

  # compute values for tests
  group_cardinality <- c(unname(table(x)))
  A_row_sum <- rowSums(as.matrix(o$A()))

  # run tests
  expect_s3_class(o, "OptimizationProblem")
  expect_snapshot(as.list(o))
  ## expect obj to have default weights of 1 for each feature group
  expect_equal(
    o$obj(),
    c(
      rep(0, p$number_of_planning_units()),
      rep(0, p$number_of_planning_units() * p$number_of_features()),
      rep(0, p$number_of_features()),
      rep(1, length(unique(x))),
      rep(0, p$number_of_features())
    )
  )
  ## expect total number of constraints to be equal to:
  ## (number of planning units * number of features) +
  ## (number of features * 2) + number of feature groups + 1
  expect_length(
    o$rhs(),
    (p$number_of_planning_units() * p$number_of_features()) +
      (length(x) * 2) + length(unique(x)) + 1
  )
  ## expect the row sums of the A matrix for the chance constraints
  ## to be equal to the cardinality of the group
  expect_equal(
    group_cardinality,
    tail(A_row_sum, 2)
  )
  ## expect the confidence level to be reflected in the RHS
  expect_equal(
    group_cardinality * (1 - conf_level),
    tail(o$rhs(), 2)
  )
})

test_that("compile (single zone, conf_level = 1)", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_shortfall_objective(budget = 1) |>
    add_constant_robust_constraints(groups = x, conf_level = 1) |>
    prioritizr::add_absolute_targets(0.1) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p)

  # run tests
  expect_s3_class(o, "OptimizationProblem")
  expect_snapshot(as.list(o))
  ## expect obj to have default weights of 1 for each feature group,
  ## and because conf_level = 1 the probabilistic constraints should
  ## not be applied - thus the problem should not contain auxiliary
  ## variables for the big-m variables
  expect_equal(
    o$obj(),
    c(
      rep(0, p$number_of_planning_units()),
      rep(0, p$number_of_features()),
      rep(1, length(unique(x)))
    )
  )
})

test_that("compile (single zone, weights)", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))
  wts <- rep_len(c(1, 3), terra::nlyr(sim_features))

  # build problem
  p1 <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_shortfall_objective(budget = 1) |>
    add_constant_robust_constraints(groups = x, conf_level = 0.1) |>
    prioritizr::add_feature_weights(wts) |>
    prioritizr::add_absolute_targets(0.1) |>
    prioritizr::add_binary_decisions()
  p2 <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_shortfall_objective(budget = 1) |>
    add_constant_robust_constraints(groups = x, conf_level = 0.1) |>
    prioritizr::add_absolute_targets(0.1) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o1 <- prioritizr::compile(p1)
  o2 <- prioritizr::compile(p2)

  # convert to list and set obj to NULL
  l1 <- as.list(o1)
  l1$obj <- NULL
  l2 <- as.list(o2)
  l2$obj <- NULL

  # run tests
  expect_s3_class(o1, "OptimizationProblem")
  ## test that weights are applied correctly
  expect_equal(
    o1$obj(),
    c(
      rep(0, p1$number_of_planning_units()), rep(0, length(x)),
      mean(wts[x == "a"]),  mean(wts[x == "b"]),  rep(0, length(x))
    )
  )
  ## test that default weights are applied correctly
  expect_equal(
    o2$obj(),
    c(
      rep(0, p2$number_of_planning_units()), rep(0, length(x)),
      1,  1,  rep(0, length(x))
    )
  )
  ## test that all model components are identical except for obj
  expect_equal(l1, l2)
})

test_that("solve (single zone)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")

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
    add_constant_robust_constraints(groups = x, conf_level = 0.9) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # run tests
  expect_s4_class(s, "SpatRaster")
  expect_equal(terra::nlyr(s), 1)
  expect_gte(terra::global(s, "min", na.rm = TRUE)[[1]], 0)
  expect_lte(terra::global(s, "max", na.rm = TRUE)[[1]], 1)
})

test_that("compile (multiple zones, single budget)", {
  # import data
  sim_zones_pu_raster <- prioritizr::get_sim_zones_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define parameters for prioritization
  budget <- as.numeric(
    terra::global(sim_pu_raster, "sum", na.rm = TRUE)
  ) * 0.1
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(
      sim_zones_pu_raster,
      prioritizr::zones(sim_features, sim_features, sim_features)
    ) |>
    add_robust_min_shortfall_objective(budget = budget) |>
    add_constant_robust_constraints(groups = x, conf_level = 0.1) |>
    prioritizr::add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p)

  # run tests
  expect_s3_class(o, "OptimizationProblem")
  expect_snapshot(as.list(o))
  expect_equal(
    o$rhs()[(terra::nlyr(sim_features) * 3) + 1],
    budget
  )
})

test_that("compile (multiple zones, multiple budget)", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define parameters for prioritization
  budget <- as.numeric(
    terra::global(sim_pu_raster, "sum", na.rm = TRUE)
  ) * c(0.1, 0.2, 0.5)
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(
      sim_pu_raster[[rep(1, 3)]],
      prioritizr::zones(sim_features, sim_features, sim_features)
    ) |>
    add_robust_min_shortfall_objective(budget = budget) |>
    add_constant_robust_constraints(groups = x, conf_level = 0.9) |>
    prioritizr::add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p)

  # run tests
  expect_s3_class(o, "OptimizationProblem")
  expect_snapshot(as.list(o))
  expect_equal(
    o$rhs()[(terra::nlyr(sim_features) * 3) + seq_len(3)],
    budget
  )
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
  expect_gte(min(terra::global(s, "min", na.rm = TRUE)[[1]]), 0)
  expect_lte(max(terra::global(s, "max", na.rm = TRUE)[[1]]), 1)
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

test_that("warnings", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings and weights
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))
  weights <- runif(length(x))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_shortfall_objective(budget = 1) |>
    prioritizr::add_feature_weights(weights) |>
    add_constant_robust_constraints(groups = x, conf_level = conf_level) |>
    prioritizr::add_absolute_targets(0.1) |>
    prioritizr::add_binary_decisions()

  # run tests
  expect_message(
    prioritizr::compile(p),
    "multiple distinct weight values"
  )
})
