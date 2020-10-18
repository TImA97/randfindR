context("zero_order_dependencies")

test_that("redundancy index returns correct result", {
  expect_equal(redundancy_index(c(1,1,1),2), 100)
  expect_equal(redundancy_index(c(0,1,2),3), 0)
  # last example taken from RGCalc by Neil & Towse
  expect_equal(round(redundancy_index(c(1,1,2),2),2),8.17)
  expect_equal(round(redundancy_index(ginsburg1994, 10), 4), 1.1820)
})

test_that("variance of digits returns correct result", {
  expect_equal(round(var_digits(ginsburg1994, 10), 2), 5.00)
})
