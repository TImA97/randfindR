context("further regularity indices")

test_that("runs_index returns correct result", {
  expect_equal(round(runs_index(ginsburg1994), 2), 1.76)
})

test_that("coupon_score returns correct result", {
  expect_equal(round(coupon_score(ginsburg1994, 10), 1), 19.8)
})

test_that("gap_score returns correct result", {
  # test data taken from RGCalc (Towse & Neil, 1998)
  expect_equal(round(gap_score(ginsburg1994), 2), 8.0)
})

test_that("poker_score returns correct result", {
  expect_equal(poker_score(ginsburg1994), 4)
})
