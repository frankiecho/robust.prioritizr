test_that("constant target_trans method", {
  # load data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()[[rep(1, 11)]]
  names(sim_features) <- paste0("feature_", seq_len(terra::nlyr(sim_features)))

  # define feature grouping data
  #       1     2      3    4     5     6     7     8     9     10    11
  x <- c("g1", "g2", "g1", "g2", "g3", "g4", "g3", "g4", "g4", "g5", "g5")

  # define targets
  targets <- c(100, 200, 300, 400, 500, 600, 700, 800, 760, 1000, 1100)

  # build problem
  p <- suppressWarnings({
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_absolute_targets(targets) |>
    add_constant_robust_constraints(x, conf_level = 0.5, target_trans = NA) |>
    prioritizr::add_binary_decisions()
  })

  # compute transformed targets
  expect_message(
    x <- transform_targets(p$feature_targets(), get_feature_group_data(p)),
    "mean"
  )

  # manually compute correct result
  y <- tibble::tibble(
    feature = seq_len(terra::nlyr(sim_features)),
    zone = list(1L)[rep(1, terra::nlyr(sim_features))],
    sense = ">=",
    value = c(
      200, # feature_1 = mean(100, 300)
      300, # feature_2, mean(200, 400)
      200, # feature_3 = mean(100, 300)
      300, # feature_4, mean(200, 400)
      600, # feature_5, mean(500, 700)
      720, # feature_6, mean(600, 800, 760)
      600, # feature_7, mean(500, 700)
      720, # feature_8, mean(700, 800, 760)
      720, # feature_9, mean(700, 800, 760)
      1050, # feature_10, mean(1000, 1100)
      1050 # feature_11, mean(1000, 1100)
    )
  )

  # run tests
  expect_equal(x$feature, y$feature)
  expect_equal(x$zone, y$zone)
  expect_equal(x$sense, y$sense)
  expect_equal(x$value, y$value)
})

test_that("varying target_trans methods", {
  # load data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()[[rep(1, 11)]]
  names(sim_features) <- paste0("feature_", seq_len(terra::nlyr(sim_features)))

  # define feature grouping data
  x <- tibble::tibble(
    features = list(
      c("feature_1", "feature_3"),
      c("feature_2", "feature_4"),
      c("feature_5", "feature_7"),
      c("feature_6", "feature_8", "feature_9"),
      c("feature_10", "feature_11")
    ),
    conf_level = c(0.3, 0.5, 0.8, 0.1, 0.23),
    target_trans = c("none", "min", "max", "mean", NA)
  )

  # define targets
  targets <- c(100, 200, 300, 400, 500, 600, 700, 800, 760, 1000, 1100)

  # build problem
  p <- suppressWarnings({
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_absolute_targets(targets) |>
    add_variable_robust_constraints(x) |>
    prioritizr::add_binary_decisions()
  })

  # compute transformed targets
  expect_message(
    x <- transform_targets(p$feature_targets(), get_feature_group_data(p)),
    "specified methods"
  )

  # manually compute correct result
  y <- tibble::tibble(
    feature = seq_len(terra::nlyr(sim_features)),
    zone = list(1L)[rep(1, terra::nlyr(sim_features))],
    sense = ">=",
    value = c(
      100, # feature_1 = 100
      200, # feature_2, min(200, 400)
      300, # feature_3 = 300
      200, # feature_4, min(200, 400)
      700, # feature_5, max(500, 700)
      720, # feature_6, mean(600, 800, 760)
      700, # feature_7, max(500, 700)
      720, # feature_8, mean(700, 800, 760)
      720, # feature_9, mean(700, 800, 760)
      1050, # feature_10, mean(1000, 1100)
      1050 # feature_11, mean(1000, 1100)
    )
  )

  # run tests
  expect_equal(x$feature, y$feature)
  expect_equal(x$zone, y$zone)
  expect_equal(x$sense, y$sense)
  expect_equal(x$value, y$value)
})
