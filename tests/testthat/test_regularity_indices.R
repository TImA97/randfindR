context("further regularity indices")

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
                    2,3,8,8,7,2,0,3,5,9)


test_that("runs_index returns correct result", {
  expect_equal(round(runs_index(sequence_three), 2), 1.76)
})

test_that("coupon_score returns correct result", {
  expect_equal(round(coupon_score(sequence_three, 10), 1), 19.8)
})

test_that("gap_score returns correct result", {
  expect_equal(round(gap_score(sequence_three), 1), 7.9)
})
