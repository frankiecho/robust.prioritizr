test_that("compile (single zone, conf_level < 1, method = chance)", {
  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define feature groupings and conf level
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
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(0.1) |>
    add_variable_robust_constraints(x) |>
    prioritizr::add_binary_decisions()

  # compile problem
  o <- prioritizr::compile(p)

  # compute values for tests
  cardinality <- sapply(x$features, length)

  # run tests
  expect_equal(
    tail(o$rhs(), 2),
    cardinality * (1 - x$conf_level)
  )
})

test_that("solve (single zone, conf_level < 1, method = chance)", {
  # define skip cases
  skip_on_cran()
  skip_if_not_installed("highs")

  # import data
  sim_pu_raster <- prioritizr::get_sim_pu_raster()
  sim_features <- prioritizr::get_sim_features()

  # define parameters for prioritization
  x <- tibble::tibble(
    features = list(
      c("feature_1", "feature_3", "feature_5"),
      c("feature_2", "feature_4")
    ),
    conf_level = c(0.3, 0.5)
  )
  target <- 0.1

  # build problem
  p <-
    prioritizr::problem(sim_pu_raster, sim_features) |>
    add_robust_min_set_objective(method = "chance") |>
    prioritizr::add_relative_targets(0.1) |>
    add_variable_robust_constraints(x) |>
    prioritizr::add_binary_decisions() |>
    prioritizr::add_highs_solver(verbose = FALSE)

  # solve problem
  s <- solve(p)

  # calculate values for tests
  feature_rep <- prioritizr::eval_feature_representation_summary(p, s)
  feature_target_a <- feature_rep$relative_held[
    match(x$features[[1]], prioritizr::feature_names(p))
  ] >= target
  feature_target_b <- feature_rep$relative_held[
    match(x$features[[2]], prioritizr::feature_names(p))
  ] >= target

  # run tests
  expect_gte(mean(feature_target_a), x$conf_level[[1]])
  expect_gte(mean(feature_target_b), x$conf_level[[2]])
})

test_that("invalid arguments", {
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
    prioritizr::zones(
      z1 = sim_features, z2 = sim_features,
      feature_names = names(sim_features)
    )
  )

  # run tests
  ## data is not a data.frame
  expect_error(
    add_variable_robust_constraints(p, NULL),
    "data"
  )
  ## features not specified
  expect_error(
    add_variable_robust_constraints(
      p,
      tibble::tibble(conf_level = c(0.2, 0.5))
    ),
    "features"
  )
  ## features contains elements that are not a character vector
  expect_error(
    add_variable_robust_constraints(
      p,
      tibble::tibble(
        features = list(
          names(sim_features)[1:3],
          c(1, 2, 3)
        ),
        conf_level = c(0.2, 0.5)
      )
    ),
    "character"
  )
  ## features has invalid feature names
  expect_error(
    add_variable_robust_constraints(
      p,
      tibble::tibble(
        features = list(
          names(sim_features)[1:3],
          c(names(sim_features)[4], "ASDEF")
        ),
        conf_level = c(0.2, 0.5)
      )
    ),
    "contain only the names of features"
  )
  ## some groups do not contain at least two features
  expect_error(
    add_variable_robust_constraints(
      p,
      tibble::tibble(
        features = list(
          names(sim_features)[1:3],
          names(sim_features)[4]
        ),
        conf_level = c(0.2, 0.5)
      )
    ),
    "must have at least 2 feature names"
  )
  ## conf_level not specified
  expect_error(
    add_variable_robust_constraints(
      p,
      tibble::tibble(
        features = list(
          names(sim_features)[1:3],
          names(sim_features)[4:5]
        )
      )
    ),
    "conf_level"
  )
  ## conf_level is greater than 1
  expect_error(
    add_variable_robust_constraints(
      p,
      tibble::tibble(
        features = list(
          names(sim_features)[1:3],
          names(sim_features)[4:5]
        ),
        conf_level = c(0.5, 1.2)
      )
    ),
    "conf_level <= 1"
  )
  ## conf_level is less than 0
  expect_error(
    add_variable_robust_constraints(
      p,
      tibble::tibble(
        features = list(
          names(sim_features)[1:3],
          names(sim_features)[4:5]
        ),
        conf_level = c(0.5, -1.2)
      )
    ),
    "conf_level >= 0"
  )
})
