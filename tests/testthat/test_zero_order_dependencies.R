context("redundancy index")

test_that("redundancy index returns correct result", {
  expect_equal(redundancy_index(c(1,1,1),2), 100)
  expect_equal(redundancy_index(c(0,1,2),3), 0)
  # last example taken from RGCalc by Neil & Towse
  expect_equal(round(redundancy_index(c(1,1,2),2),2),8.17)
})


#Example taken from Gisnburg & Karpiuk (1994)
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

test_that("variance of digits retunrs correct result", {
  expect_equal(round(variance_of_digits(sequence_three, 10), 2), 5.00)
})
