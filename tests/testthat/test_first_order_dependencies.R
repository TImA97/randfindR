context("first order indizes of randomness")

# provide environment for test. Example taken from Evans (1978)
sequence_one <- c(7,1,5,8,3,4,10,9,6,2,
                  1,4,7,9,5,2,3,10,8,6,
                  7,2,1,5,4,9,6,1,10,4,
                  2,3,7,8,9,4,5,6,2,10,
                  8,9,4,3,7,5,6,1,9,8,
                  3,4,6,10,9,8,7,2,3,1,
                  9,4,5,7,10,6,9,2,8,7,
                  3,5,1,6,4,3,9,5,6,1,
                  8,10,7,5,9,2,4,3,8,1,
                  9,7,6,3,2,7,10,9,5,1,7)

sequence_two <- c(1,3,5,7,6,8,5,3,2,1,
                  7,6,5,4,2,1,3,7,10,9,
                  8,6,5,4,3,2,1,7,6,5,
                  9,10,6,5,10,10,10,3,2,1,
                  10,9,8,7,6,4,3,7,5,10,
                  9,8,6,7,9,8,3,2,1,3,
                  2,6,5,9,8,2,1,4,10,4,
                  5,7,10,9,8,7,10,1,2,3,
                  5,9,3,2,1,6,7,8,9,3,
                  9,2,1,3,7,5,4,3,2,1,1)

#Example taken from Ginsburg & Karpiuk (1994)
sequence_three <- c(2,5,7,8,9,1,0,4,2,3,
                    9,6,7,9,2,1,4,0,5,3,
                    2,5,4,2,3,5,6,7,9,8,
                    0,3,5,6,9,8,2,1,8,7,
                    6,2,0,9,7,5,6,7,9,1,
                    0,3,4,5,9,8,0,1,7,3,
                    2,0,3,5,7,6,6,5,7,6,
                    9,8,0,2,4,3,9,8,7,6,
                    4,0,3,1,8,9,7,0,2,3,
                    2,3,8,8,7,2,0,3,5,9,2)



test_that("rng_index returns correct result", {
  expect_equal(round(rng_index(sequence_one, 10), 3), 0.216)
  expect_equal(round(rng_index(sequence_two, 10), 3), 0.427)
  expect_equal(round(rng_index(sequence_three, 10), 2), 0.34)

})

test_that("digram_rep returns correct result", {
  expect_equal(digram_rep(c(1,1,1,2,1,2),2),2)
  expect_equal(digram_rep(sequence_three, 10), 47)
})

test_that("repetitions returns correct result", {
  expect_equal(repetitions(sequence_three, 10), 2)
})

test_that("series returns correct result", {
  expect_equal(series(sequence_three, 10), 39)
})

test_that("cluster ratio returns correct result", {
  expect_equal(round(cluster_ratio(sequence_three, 10), 2), 1.52)
})


