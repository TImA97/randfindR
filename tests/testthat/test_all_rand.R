context("Test aggregate function all_rand")


test_that("returned vector has correct length", {
  expect_equal(length(all_rand(ginsburg1994, 10)), 15)
})
