library(prioritizr)
library(terra)

test_that("compile (single zone)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  conf_level <- 0.9
  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(0.1) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions()

  cp <- compile(p)

  group_cardinality <- unname(table(x))

  A_row_sum <- apply(cp$A(), 1, sum)

  # Expect the length of rhs to be equal to 7
  expect_equal(length(cp$rhs()), 7)

  # Expect the rhs length to be equal to the second dimension of A
  expect_equal(nrow(cp$A()), length(cp$rhs()))

  # Expect the row sums to be less than the cardinality of the group
  expect_true(all((group_cardinality - A_row_sum[6:7]) > -1e-5))

  # Expect the confidence level to be reflected in the RHS
  expect_true(all((group_cardinality*(1-conf_level) - cp$rhs()[6:7]) > 1e-5))

  var_rob_cons <- tibble(
    features = list(c("feature_1", "feature_3", "feature_5"), c("feature_2", "feature_4")),
    conf_level = c(0.3, 0.5)
  )

  p2 <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(0.1) |>
    add_variable_robust_constraints(var_rob_cons) |>
    prioritizr::add_binary_decisions()

  cp2 <- compile(p2)

  cardinality <- sapply(var_rob_cons$features, length)

  expect_equal(tail(cp2$rhs(), 2), cardinality*(1-var_rob_cons$conf_level))
})

test_that("solve (single zone)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  conf_level <- .6
  target <- 0.1
  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "Chance") |>
    prioritizr::add_relative_targets(target) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions() %>%    prioritizr::add_default_solver(verbose = F)

  sp <- solve(p)

  feature_rep <- eval_feature_representation_summary(p, sp)

  feature_target_a <- feature_rep$relative_held[x == 'a'] > target
  expect_gte(mean(feature_target_a), conf_level)
  feature_target_b <- feature_rep$relative_held[x == 'b'] > target
  expect_gte(mean(feature_target_b), conf_level)

  p2 <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "CondValueAtRisk") |>
    prioritizr::add_relative_targets(target) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions() %>%    prioritizr::add_default_solver(verbose = F)

  sp2 <- solve(p2)

  feature_rep2 <- eval_feature_representation_summary(p2, sp2)
  feature_target_a2 <- feature_rep$relative_held[x == 'a'] > target
  expect_gte(mean(feature_target_a2), conf_level)
  feature_target_b2 <- feature_rep$relative_held[x == 'b'] > target
  expect_gte(mean(feature_target_b2), conf_level)
})

test_that("compile (multiple zones)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  sim_features_z1 <- prioritizr::get_sim_features()
  sim_features_z2 <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  t2 <- matrix(NA, ncol = 2, nrow = 5)
  t2[, 1] <- 0.1
  t2[, 2] <- 0.05

  zones <- prioritizr::zones(z1 = sim_features_z1,
                             z2 = sim_features_z2)

  conf_level <- .6
  target <- 0.1
  # build problem
  p <-
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
    add_robust_min_set_objective(method = "Chance") |>
    prioritizr::add_relative_targets(t2) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions() %>%    prioritizr::add_default_solver(verbose = F)

  cp <- compile(p)

  expect_equal(nrow(cp$A()), length(cp$row_ids()))

  expect_equal(nrow(cp$A()), length(cp$rhs()))

  expect_equal(ncol(cp$A()), length(cp$obj()))
})

test_that("solve (multiple zones)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  sim_features_z1 <- prioritizr::get_sim_features()
  sim_features_z2 <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  t2 <- matrix(NA, ncol = 2, nrow = 5)
  t2[, 1] <- .2
  t2[, 2] <- .1

  zones <- prioritizr::zones(z1 = sim_features_z1,
                             z2 = sim_features_z2)

  conf_level <- .5
  # build problem
  p <-
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
    add_robust_min_set_objective(method = "Chance") |>
    prioritizr::add_relative_targets(t2) |>
    add_constant_robust_constraints(x, conf_level) |>
    prioritizr::add_binary_decisions() %>%    prioritizr::add_default_solver(verbose = F)

  sp <- solve(p)

  expect_lte(max(values(sp[[1]] + sp[[2]]), na.rm = T), 1)
})

test_that("invalid arguments", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  sim_features_z1 <- prioritizr::get_sim_features()
  sim_features_z2 <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  t2 <- matrix(NA, ncol = 2, nrow = 5)
  t2[, 1] <- .2
  t2[, 2] <- .1

  zones <- prioritizr::zones(z1 = sim_features_z1,
                             z2 = sim_features_z2)

  # Groups not specified
  expect_error(
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
      add_constant_robust_constraints(conf_level = .1) |>
      add_robust_min_set_objective(),
    regexp = "`groups` is absent"
  )

  # Not a method
  expect_error(
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
      add_robust_min_set_objective(method = "ABCDEF"),
    regexp = "`method` must be either"
  )

  # Vector of methods
  expect_error(
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
      add_robust_min_set_objective(method = c("cvar", "chance")),
    regexp = "is not a string"
  )

  # Conf_levels in constant robust constrants is a vector
  expect_error(
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
      add_constant_robust_constraints(conf_level = c(0.1, 0.5), groups = x),
    regexp = "is not a number"
  )

  # Conf_levels is greater than 1
  expect_error(
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
      add_constant_robust_constraints(conf_level = 1.1, groups = x),
    regexp = "not less than or equal to 1"
  )

  # Conf_levels is less than 0
  expect_error(
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
      add_constant_robust_constraints(conf_level = -.1, groups = x),
    regexp = "not greater than or equal to 0"
  )

  # Groups length is not consistent with number of layers
  expect_error(
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
      add_constant_robust_constraints(groups = c('a', 'a', 'b', 'd', 'd', 'd')),
    regexp = "must specify a value for each feature"
  )

  # Some groups do not contain two realizations
  expect_error(
    prioritizr::problem(c(sim_pu_raster,sim_pu_raster), zones) |>
      add_constant_robust_constraints(groups = c('a', 'a', 'b', 'd', 'd')),
    regexp = "must have at least two realizations"
  )
})
