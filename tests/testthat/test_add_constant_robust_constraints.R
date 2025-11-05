test_that("invalid arguments", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # initialize problem
  p <- prioritizr::problem(sim_pu_raster, sim_features)

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
})

test_that("messages", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings
  x <- rep_len(c("a", "b"), terra::nlyr(sim_features))

  # initialize problem
  p <- prioritizr::problem(sim_pu_raster, sim_features)

  # run tests
  ## single group contains a single feature
  expect_message(
    add_constant_robust_constraints(p, groups = c(rep("a", 4), "d")),
    "group contains a single feature"
  )
  ## multiple groups contain a single feature
  expect_message(
    add_constant_robust_constraints(p, groups = c(rep("a", 3), "b", "d")),
    "groups contain a single feature"
  )
})
