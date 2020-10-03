context("turning point index")

test_that("turning point index returns correct result", {
  expect_equal(round(tp_index(evans1978$sequence_one), 4), 94.8980)
  expect_equal(round(tp_index(evans1978$sequence_two), 4), 62.7551)
  expect_equal(round(tp_index(ginsburg1994), 4), 70.4082)
})
