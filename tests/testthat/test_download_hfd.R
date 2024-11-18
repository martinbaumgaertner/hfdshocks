library(testthat)
library(mockery)
library(openxlsx)  # For writing Excel files

# Replace 'hfdshocks' with your actual package name
library(hfdshocks)

# Sample mock data to be used in tests
mock_data <- list(
  prw = data.frame(date = as.Date("2021-01-01"), value = 1),
  pcw = data.frame(date = as.Date("2021-01-01"), value = 2),
  mew = data.frame(date = as.Date("2021-01-01"), value = 3)
)

# Setup: Define mocks before tests
setup({
  # Mock validate_inputs to bypass actual validation
  mock_validate_inputs <- mock(TRUE)
  stub(hfdshocks::download_hfd, "validate_inputs", mock_validate_inputs)
  
  # Mock download_file to simulate successful download
  mock_download_file <- mock(TRUE)
  stub(hfdshocks::download_hfd, "download_file", mock_download_file)
  
  # Mock process_sheets to return predefined mock data
  mock_process_sheets <- mock(mock_data)
  stub(hfdshocks::download_hfd, "process_sheets", mock_process_sheets)
})

# Cleanup after tests
teardown({
  rm(list = ls())
})

# Test suite for validate_inputs function
test_that("validate_inputs works correctly", {
  # Access the original validate_inputs function
  # Since it's not exported, use getFromNamespace
  validate_inputs <- getFromNamespace("validate_inputs", "hfdshocks")
  
  # Test with invalid URL
  expect_error(
    validate_inputs("https://example.com/file.pdf", "path/to/save"),
    "Please insert a link which refers to an existing .xlsx file."
  )
  
  # Test with valid URL
  expect_silent(
    validate_inputs("https://example.com/file.xlsx", tempdir())
  )
  
  # Uncomment and test path validation if enabled
  # expect_error(
  #   validate_inputs("https://example.com/file.xlsx", "/non/existing/path"),
  #   "The specified path does not exist."
  # )
})

# Test suite for download_file function
test_that("download_file works correctly", {
  # Access the original download_file function
  download_file <- getFromNamespace("download_file", "hfdshocks")
  
  # Test successful download
  expect_silent(
    download_file("https://example.com/file.xlsx", tempfile())
  )
  
  # Verify that curl::curl_download was called once
  expect_called(mock_download_file, 1)
  expect_args(mock_download_file, 1, "https://example.com/file.xlsx", ANY())
})