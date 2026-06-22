# @srrstats {G5.0} Tests use prioritizr::get_sim_pu_raster() and
#   get_sim_features() - standard shared datasets with known properties.
# @srrstats {G5.1} Simulation datasets are exported from prioritizr.
# @srrstats {G5.2} Appropriate error and warning behaviour of all functions
#   is explicitly demonstrated below.
# @srrstats {G5.2a} Every message produced by stop()/warning()/message() in
#   add_constant_robust_constraints() is unique.
# @srrstats {G5.2b} Explicit tests demonstrate conditions which trigger every
#   message, comparing results with expected values.
# @srrstats {G5.3} Return values from solve() are checked to contain no NA
#   or undefined values (see test_add_robust_min_set_objective.R).
# @srrstats {G5.4} Correctness tests confirm the LP matrix structure matches
#   expected output for fixed test datasets via expect_snapshot.
# @srrstats {G5.4a} For this novel method, correctness is tested against
#   trivial cases (conf_level = 1 recovers non-robust result) and structural
#   LP matrix snapshots.
# @srrstats {G5.6} Parameter recovery tests confirm that the implementation
#   produces expected results for data with known properties.
# @srrstats {G5.6a} Parameter recovery tests succeed within solver tolerance
#   (relative_held >= target).
# @srrstats {G5.8} Edge condition tests confirm expected behaviour for
#   data with extreme properties.
# @srrstats {G5.8a} Zero-length groups vector tested via wrong-length test.
# @srrstats {G5.8b} Data of unsupported types tested (e.g. numeric groups).
# @srrstats {G5.8c} Single-feature groups (all-identical group) warned.
# @srrstats {G5.8d} Groups vector longer than number of features tested.
# @srrstats {SP2.0b} Errors (not warnings) are raised for invalid inputs.

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
