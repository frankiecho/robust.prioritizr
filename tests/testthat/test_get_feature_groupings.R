test_that("works (convert_to_integer = TRUE)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b", "c"), terra::nlyr(sim_features))
  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective() |>
    prioritizr::add_relative_targets(0.1)  |>
    add_robust_constraints(feature_groupings = x) |>
    prioritizr::add_binary_decisions()
  # extract groupings
  y <- get_feature_groupings(p, convert_to_integer = TRUE)
  # tests
  expect_s3_class(p, "ConservationProblem")
  expect_equal(
    y,
    rep_len(c(0, 1, 2), terra::nlyr(sim_features))
  )
})

test_that("works (convert_to_integer = FALSE)", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  # define feature groupings
  x <- rep_len(c("a", "b", "c"), terra::nlyr(sim_features))
  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective() |>
    prioritizr::add_relative_targets(0.1)  |>
    add_robust_constraints(feature_groupings = x) |>
    prioritizr::add_binary_decisions()
  # extract groupings
  y <- get_feature_groupings(p, convert_to_integer = FALSE)
  # tests
  expect_s3_class(p, "ConservationProblem")
  expect_equal(x, y)
})

test_that("invalid inputs", {
  skip_if_not_installed("terra")
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  # not a problem
  expect_error(
    get_feature_groupings(1),
    "ConservationProblem"
  )
  # throws error if no robust constraints
  expect_error(
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective() |>
    prioritizr::add_relative_targets(0.1)  |>
    prioritizr::add_binary_decisions() |>
    get_feature_groupings(),
    "must have robust constraints"
  )
  # throws error if multiple robust constraints
  expect_error(
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective() |>
    prioritizr::add_relative_targets(0.1)  |>
    add_robust_constraints(feature_groupings = x) |>
    add_robust_constraints(feature_groupings = x) |>
    prioritizr::add_binary_decisions() |>
    get_feature_groupings(),
    "multiple"
  )
})
