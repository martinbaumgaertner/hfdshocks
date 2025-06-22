library(testthat)
library(mockery)
library(dplyr)
library(readxl)

# Replace 'hfdshocks' with your actual package name
library(hfdshocks)

# Sample mock data to be used in tests
mock_data <- list(
  prw = data.frame(date = as.Date("2008-09-04"), value = 100),
  pcw = data.frame(date = as.Date("2008-09-04"), value = 200),
  mew = data.frame(date = as.Date("2008-09-04"), value = 300)
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
    validate_inputs("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx", tempdir())
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
    download_file("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx", tempfile())
  )

  # Verify that curl::curl_download was called once
  expect_called(mock_download_file, 1)
  expect_args(mock_download_file, 1, "https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx", ANY())
})

# Test suite for process_sheets function
test_that("process_sheets works correctly", {
  # Access the original process_sheets function
  process_sheets <- getFromNamespace("process_sheets", "hfdshocks")

  # Create a temporary Excel file with the required sheets
  temp_excel <- tempfile(fileext = ".xlsx")

  # Write mock data to sheets using openxlsx
  openxlsx::write.xlsx(mock_data$prw, temp_excel, sheetName = "Press Release Window", overwrite = TRUE)
  openxlsx::write.xlsx(mock_data$pcw, temp_excel, sheetName = "Press Conference Window", overwrite = TRUE)
  openxlsx::write.xlsx(mock_data$mew, temp_excel, sheetName = "Monetary Event Window", overwrite = TRUE)

  # Test processing of sheets
  result <- process_sheets(temp_excel)

  expect_type(result, "list")
  expect_named(result, c("prw", "pcw", "mew"))
  expect_equal(result$prw, mock_data$prw)
  expect_equal(result$pcw, mock_data$pcw)
  expect_equal(result$mew, mock_data$mew)

  # Clean up temporary file
  unlink(temp_excel)
})

# Test suite for process_sheets function with missing sheets
test_that("process_sheets fails gracefully with missing sheets", {
  # Access the original process_sheets function
  process_sheets <- getFromNamespace("process_sheets", "hfdshocks")

  # Create a temporary Excel file with missing 'Press Conference Window' sheet
  temp_excel <- tempfile(fileext = ".xlsx")

  # Write only 'Press Release Window' and 'Monetary Event Window' sheets
  openxlsx::write.xlsx(mock_data$prw, temp_excel, sheetName = "Press Release Window", overwrite = TRUE)
  openxlsx::write.xlsx(mock_data$mew, temp_excel, sheetName = "Monetary Event Window", overwrite = TRUE)

  # Expect an error due to missing sheet
  expect_error(
    process_sheets(temp_excel),
    "Failed to read or process the Excel file"
  )

  # Clean up temporary file
  unlink(temp_excel)
})

# Test suite for download_hfd function
test_that("download_hfd works correctly", {
  # Access the original download_hfd function
  download_hfd <- hfdshocks::download_hfd

  # Define parameters
  url <- "https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx"
  path <- tempdir()

  # Call the download_hfd function
  result <- download_hfd(url, path)

  # Assertions
  expect_type(result, "list")
  expect_named(result, c("prw", "pcw", "mew"))
  expect_equal(result, mock_data)

  # Verify that validate_inputs was called once with correct arguments
  expect_called(mock_validate_inputs, 1)
  expect_args(mock_validate_inputs, 1, url, path)

  # Verify that download_file was called once with correct arguments
  expect_called(mock_download_file, 1)
  expect_args(mock_download_file, 1, url, file.path(path, "temporary.xlsx"))

  # Verify that process_sheets was called once with the temporary file path
  expect_called(mock_process_sheets, 1)
  expect_args(mock_process_sheets, 1, file.path(path, "temporary.xlsx"))
})

# Test suite for download_hfd function with download failure
test_that("download_hfd fails gracefully when download_file fails", {
  # Mock download_file to throw an error
  mock_download_file_error <- mock(stop("Download failed"))
  stub(hfdshocks::download_hfd, "download_file", mock_download_file_error)

  # Define parameters
  url <- "https://example.com/file.xlsx"
  path <- tempdir()

  # Expect an error when download_file fails
  expect_error(
    hfdshocks::download_hfd(url, path),
    "Failed to download the file: Download failed"
  )

  # Verify that validate_inputs was called
  expect_called(mock_validate_inputs, 1)
  expect_args(mock_validate_inputs, 1, url, path)

  # Verify that download_file was called
  expect_called(mock_download_file_error, 1)
  expect_args(mock_download_file_error, 1, url, file.path(path, "temporary.xlsx"))
})

# Test suite for download_hfd function with processing failure
test_that("download_hfd fails gracefully when process_sheets fails", {
  # Mock process_sheets to throw an error
  mock_process_sheets_error <- mock(stop("Processing failed"))
  stub(hfdshocks::download_hfd, "process_sheets", mock_process_sheets_error)

  # Define parameters
  url <- "https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx"
  path <- tempdir()

  # Expect an error when process_sheets fails
  expect_error(
    hfdshocks::download_hfd(url, path),
    "Failed to read or process the Excel file: Processing failed"
  )

  # Verify that validate_inputs was called
  expect_called(mock_validate_inputs, 1)
  expect_args(mock_validate_inputs, 1, url, path)

  # Verify that download_file was called
  expect_called(mock_download_file, 1)
  expect_args(mock_download_file, 1, url, file.path(path, "temporary.xlsx"))

  # Verify that process_sheets was called
  expect_called(mock_process_sheets_error, 1)
  expect_args(mock_process_sheets_error, 1, file.path(path, "temporary.xlsx"))
})

# Test suite for download_hfd function with invalid file type
test_that("download_hfd fails when URL does not point to an .xlsx file", {
  # Access the original download_hfd function
  download_hfd <- hfdshocks::download_hfd

  # Define parameters with invalid URL
  invalid_url <- "https://example.com/file.pdf"
  path <- tempdir()

  # Expect an error due to invalid URL
  expect_error(
    download_hfd(invalid_url, path),
    "Please insert a link which refers to an existing .xlsx file."
  )

  # Verify that validate_inputs was called
  expect_called(mock_validate_inputs, 1)
  expect_args(mock_validate_inputs, 1, invalid_url, path)

  # Verify that download_file and process_sheets were not called
  expect_called(mock_download_file, 0)
  expect_called(mock_process_sheets, 0)
})

# Test suite for validate_inputs function with missing path check (if enabled)
# Uncomment the following block if path validation is re-enabled in validate_inputs

# test_that("validate_inputs fails when path does not exist", {
#   # Access the original validate_inputs function
#   validate_inputs <- getFromNamespace("validate_inputs", "hfdshocks")
#
#   # Define parameters with non-existing path
#   url <- "https://example.com/file.xlsx"
#   invalid_path <- "/non/existing/path"
#
#   # Expect an error due to non-existing path
#   expect_error(
#     validate_inputs(url, invalid_path),
#     "The specified path does not exist."
#   )
# })
