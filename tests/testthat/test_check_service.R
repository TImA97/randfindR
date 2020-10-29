context("tests by check_service")

test_that("checks return errors", {
  expect_error(is_vector_long_enough(1))
  expect_error(is_vector_long_enough(1:10, min_length = 11))
  expect_error(sufficient_options_provided(options = 3, min_options = 4))
  expect_error(is_number_of_distinct_options_too_high(1:4, options = 3))
})

test_that("checks return no error", {
  expect_error(sufficient_options_provided(options = 3, min_options = 3), NA)
  expect_error(is_vector_long_enough(1:10, min_length = 10), NA)
  expect_error(is_number_of_distinct_options_too_high(1:4, options = 4), NA)
})

