context("Test aggregate function all_randicators")


test_that("returned vector has correct length", {
  expect_equal(length(all_randicators(ginsburg1994, 10)), 15)
  expect_equal(length(all_randicators(ginsburg1994, 10, indices = "reg_index")), 1)
  expect_equal(length(all_randicators(
    ginsburg1994,
    options = 10,
    indices = c("reg_index", "rng_index", "runs_index")
  )), 3)

})
