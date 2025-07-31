test_that("add_constant_robust_constraints()", {
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
    add_constant_robust_constraints(x, 0.1) |>
    prioritizr::add_binary_decisions()
  # extract groupings
  y <- get_feature_group_data(p)
  # tests
  expect_true(is.list(y))
  expect_equal(
    y$ids,
    rep_len(c(0, 1), terra::nlyr(sim_features))
  )
  expect_equal(
    y$conf_level,
    rep(0.1, 2)
  )
})

test_that("add_variable_robust_constraints()", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()[[rep(1, 7)]]
  names(sim_features) <- paste0("l", seq_len(terra::nlyr(sim_features)))
  # define feature groupings
  x <- tibble::tibble(
    features = list(
      c("l1", "l3"),
      c("l2", "l4", "l6"),
      c("l5", "l7")
    ),
    conf_level = c(0.5, 0.2, 0.9)
  )
  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective() |>
    prioritizr::add_relative_targets(0.1)  |>
    add_variable_robust_constraints(x) |>
    prioritizr::add_binary_decisions()
  # extract groupings
  y <- get_feature_group_data(p)
  # tests
  expect_true(is.list(y))
  expect_equal(
    y$ids,
    c(0, 1, 0, 1, 2, 1, 2)
  )
  expect_equal(
    y$conf_level,
    x$conf_level
  )
})

test_that("invalid inputs", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))
  # not a problem
  expect_error(
    get_feature_group_data(1),
    "ConservationProblem"
  )
  # throws error if no robust constraints
  expect_error(
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective() |>
    prioritizr::add_relative_targets(0.1)  |>
    prioritizr::add_binary_decisions() |>
    get_feature_group_data(),
    "must have robust constraints"
  )
  # throws error if multiple robust constraints
  expect_error(
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective() |>
    prioritizr::add_relative_targets(0.1)  |>
    add_constant_robust_constraints(x, 0.1) |>
    add_constant_robust_constraints(x, 0.1) |>
    prioritizr::add_binary_decisions() |>
    get_feature_group_data(),
    "multiple"
  )
})
