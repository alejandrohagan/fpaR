# Load necessary library

# Define the test context
test_that("is_yyyy_mm_dd works as expected", {

  # Test valid YYYY-MM-DD date formats
  expect_true(is_yyyy_mm_dd("2024-10-05"))
  expect_true(is_yyyy_mm_dd("1999-01-01"))

  # Test invalid date formats
  expect_false(is_yyyy_mm_dd("2024-13-05"))    # Invalid month (13)
  expect_false(is_yyyy_mm_dd("2024-10-32"))    # Invalid day (32)

  # Test empty string and NA values
  expect_false(is_yyyy_mm_dd(""))              # Empty string
  expect_false(is_yyyy_mm_dd(NA_character_))   # NA value

  # Test non-character input (shouldn't match)
  expect_false(is_yyyy_mm_dd(12345))           # Numeric input
  expect_false(is_yyyy_mm_dd(TRUE))            # Boolean input
})

