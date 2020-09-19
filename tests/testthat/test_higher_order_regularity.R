context("higher order regularity")


test_that("regularity index retuns correct result", {
  expect_equal(reg_index(rep(1, times = 20), 2), 1)
})
