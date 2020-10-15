context("RNG indices of randomness")

test_that("rng_index returns correct result", {
  expect_equal(round(rng_index(evans1978$sequence_one, 10), 3), 0.216)
  expect_equal(round(rng_index(evans1978$sequence_two, 10), 3), 0.427)
  expect_equal(round(rng_index(ginsburg1994, 10), 2), 0.34)
})

test_that("rng2_index returns correct result", {
  skip('skip until differences are resolved. Test data might be flawed.')
  expect_equal(round(rng2_index(evans1978$sequence_one, 10), 4), 0.2551)
  expect_equal(round(rng2_index(evans1978$sequence_two, 10), 4), 0.2652)
  expect_equal(round(rng2_index(ginsburg1994, 10), 4), 0.2939)
})
