test_that("works", {
  expect_type(run_example(), "logical")
  expect_length(run_example(), 1)
})
