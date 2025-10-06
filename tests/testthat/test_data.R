test_that("get_vic_study_area()", {
  # import data
  x <- get_vic_study_area()
  # run tests
  expect_s3_class(x, "sf")
})

test_that("get_vic_cost()", {
  # import data
  x <- get_vic_cost()
  # run tests
  expect_s4_class(x, "SpatRaster")
  expect_equal(terra::nlyr(x), 1)
  expect_true(all(terra::values(x)[[1]] >= 0, na.rm = TRUE))
})

test_that("get_vic_pa()", {
  # import data
  x <- get_vic_pa()
  # run tests
  expect_s4_class(x, "SpatRaster")
  expect_equal(terra::nlyr(x), 1)
  expect_true(all(terra::values(x)[[1]] >= 0, na.rm = TRUE))
  expect_true(all(terra::values(x)[[1]] <= 1, na.rm = TRUE))
})

test_that("get_vic_species()", {
  # import data
  x <- get_vic_species()
  # run tests
  expect_s4_class(x, "SpatRaster")
  expect_equal(terra::nlyr(x), 306)
  expect_true(all(terra::values(x)[[1]] >= 0, na.rm = TRUE))
  expect_true(all(terra::values(x)[[1]] <= 1, na.rm = TRUE))
})

test_that("get_vic_species_metadata()", {
  # import data
  x <- get_vic_species_metadata()
  # run tests
  expect_s3_class(x, "tbl_df")
  expect_equal(
    names(x),
    c("id", "name", "species", "class", "proj", "timestep", "scenario", "sum")
  )
  expect_true(is.numeric(x$id))
  expect_true(is.character(x$species))
  expect_true(is.character(x$class))
  expect_true(is.character(x$proj))
  expect_true(is.numeric(x$timestep))
  expect_true(is.character(x$scenario))
  expect_true(is.numeric(x$sum))
})

test_that("data integrity", {
  # import data
  vic_cost <- get_vic_cost()
  vic_pa <- get_vic_pa()
  vic_species <- get_vic_species()
  vic_species_metadata <- get_vic_species_metadata()

  # run tests
  ## all raster layers conform to the same spatial grid
  expect_true(inherits(
    terra::rast(list(vic_cost, vic_pa, vic_species[[1]])),
    "SpatRaster"
  ))
  ## check that metadata describes layers
  expect_equal(
    vic_species_metadata$id,
    seq_len(terra::nlyr(vic_species))
  )
  expect_equal(
    vic_species_metadata$name,
    names(vic_species)
  )
})
