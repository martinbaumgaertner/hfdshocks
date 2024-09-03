library(testthat)
library(mockery)

# Mock data for testing
mock_data <- list(
  prw = data.frame(date = as.POSIXct("2021-01-01"), value = 1),
  pcw = data.frame(date = as.POSIXct("2021-01-01"), value = 2),
  mew = data.frame(date = as.POSIXct("2021-01-01"), value = 3)
)

# Mock functions
mock_validate_inputs <- function(url, path) {
  return(TRUE)
}

mock_download_file <- function(url, destfile) {
  return(TRUE)
}

mock_process_sheets <- function(temp_file, prw_file, pcw_file, mew_file) {
  return(mock_data)
}

test_that("validate_inputs works correctly", {
  expect_error(validate_inputs("invalid_url", "path"), "Please insert a link which refers to an existing .xlsx file.")
  expect_error(validate_inputs("https://example.com/file.xlsx", "invalid_path"), "The specified path does not exist.")
  expect_silent(validate_inputs("https://example.com/file.xlsx", tempdir()))
})

test_that("download_file works correctly", {
  mock_curl_download <- mock(TRUE)
  stub(download_file, "curl::curl_download", mock_curl_download)
  expect_silent(download_file("https://example.com/file.xlsx", tempfile()))
  expect_called(mock_curl_download, 1)
})