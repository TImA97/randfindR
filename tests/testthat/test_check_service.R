context("tests by check_service")

test_that("checks return error", {
  expect_error(is_vector_long_enough(1))
  expect_error(is_vector_long_enough(1:10, min_length = 11))
  expect_error(sufficient_options_provided(options = 3, min_options = 4))
  expect_error(is_number_of_distinct_options_too_high(1:4, options = 3))
  expect_error(correct_indices_provided(c("reg_4index"), c("reg_index", "rng_index")))
})

test_that("checks return no error", {
  expect_error(sufficient_options_provided(options = 3, min_options = 3), NA)
  expect_error(is_vector_long_enough(1:10, min_length = 10), NA)
  expect_error(is_number_of_distinct_options_too_high(1:4, options = 4), NA)
  expect_error(correct_indices_provided(c("reg_index"), c("reg_index", "rng_index")), NA)
})

