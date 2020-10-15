context("RNG indices of randomness")

test_that("rng_index returns correct result", {
  expect_equal(round(rng_index(evans1978$sequence_one, 10), 3), 0.216)
  expect_equal(round(rng_index(evans1978$sequence_two, 10), 3), 0.427)
  expect_equal(round(rng_index(ginsburg1994, 10), 2), 0.34)
})
