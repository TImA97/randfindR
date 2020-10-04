context("first order indizes of randomness")

test_that("rng_index returns correct result", {
  expect_equal(round(rng_index(evans1978$sequence_one, 10), 3), 0.216)
  expect_equal(round(rng_index(evans1978$sequence_two, 10), 3), 0.427)
  expect_equal(round(rng_index(ginsburg1994, 10), 2), 0.34)

})

test_that("digram_rep returns correct result", {
  expect_equal(digram_rep(ginsburg1994, 10), 47)
})

test_that("repetitions returns correct result", {
  expect_equal(repetitions(ginsburg1994, 10), 2)
})

test_that("series returns correct result", {
  expect_equal(series(ginsburg1994, 10), 39)
})

test_that("cluster ratio returns correct result", {
  expect_equal(round(cluster_ratio(ginsburg1994, 10), 2), 1.52)
})

test_that("Guttmann's Null-Score Quotient returns correct result", {
  expect_equal(round(null_score(evans1978$sequence_one, 10), 2), 33.33)
  expect_equal(round(null_score(evans1978$sequence_two, 10), 4), 52.5253)
  expect_equal(round(null_score(ginsburg1994, 10), 4), 47.4747)
})
