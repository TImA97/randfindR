context("higher order regularity")


test_that("regularity index retuns correct result", {
  expect_equal(reg_index(rep(1, times = 20), 2), 1)
  expect_equal(round(reg_index(rep(c(0, 1), each = 10), 2), 4), 1 - 0.4226)
  expect_equal(round(reg_index(rep(c(0, 1), times = 10), 2), 4), 1 - 0.4226)
})
