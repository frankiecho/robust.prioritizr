test_that("add_constant_robust_constraints()", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_constant_robust_constraints(groups = x, conf_level = 0.5)

  # run tests
  expect_true(has_robust_constraints(p))
})

test_that("add_variable_robust_constraints()", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- tibble::tibble(
    features = list(
      c("feature_1", "feature_3", "feature_5"),
      c("feature_2", "feature_4")
    ),
    conf_level = c(0.3, 0.5)
  )

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_variable_robust_constraints(data = x)

  # run tests
  expect_true(has_robust_constraints(p))
})

test_that("throws correct error", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # build problem
  p <- prioritizr::problem(sim_pu_raster, sim_features)

  # run tests
  ## no constraints
  expect_false(has_robust_constraints(p))
  expect_error(
    assert(has_robust_constraints(p)),
    "robust constraints"
  )
  ## multiple constraints
  expect_error(
    assert(
      p |>
      add_constant_robust_constraints(x) |>
      add_constant_robust_constraints(x) |>
      has_robust_constraints()
    ),
    "multiple robust constraints"
  )
})
