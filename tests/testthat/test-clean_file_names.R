library(testthat)
library(mockery)

# Create a temporary directory for testing
temp_dir <- tempdir()

# Create a function to set up the test environment
setup_test_environment <- function() {
  # Create a temporary directory and some files for testing
  dir.create(file.path(temp_dir, "test_dir"), showWarnings = FALSE)
  write.csv(data.frame(x = 1:5, y = letters[1:5]),
            file = file.path(temp_dir, "test_dir", "example_file_1.csv"))
  write.csv(data.frame(x = 1:5, y = letters[1:5]),
            file = file.path(temp_dir, "test_dir", "example_file_2.csv"))
}

# Create a function to clean up the test environment
cleanup_test_environment <- function() {
  unlink(file.path(temp_dir, "test_dir"), recursive = TRUE)
}

# Setup tests
setup_test_environment()

test_that("clean_file_names() works with valid input", {
  # Mock the cli alert functions to prevent actual output
  mockery::stub(clean_file_names, "cli::cli_alert", NULL)
  mockery::stub(clean_file_names, "cli::cli_alert_success", NULL)

  # Call the function with the test directory
  clean_file_names(file.path(temp_dir, "test_dir"), case = "snake")

  # Check if files were renamed correctly
  new_files <- list.files(file.path(temp_dir, "test_dir"), full.names = TRUE)
  expect_true(all(grepl("example_file_1", new_files)))
  expect_true(all(grepl("example_file_2", new_files)))
})

test_that("clean_file_names() throws error when directory does not exist", {
  expect_error(
    clean_file_names(file.path(temp_dir, "non_existent_dir")),
    "Directory non_existent_dir does not exist."
  )
})

test_that("clean_file_names() throws error when no files found", {
  # Create an empty directory
  empty_dir <- file.path(temp_dir, "empty_dir")
  dir.create(empty_dir)

  expect_error(
    clean_file_names(empty_dir),
    "No files found in directory"
  )
})

# Clean up after tests
cleanup_test_environment()
