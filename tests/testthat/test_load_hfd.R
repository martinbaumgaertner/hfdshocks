library(testthat)
library(dplyr)
library(mockery)

# Mock data for testing
mock_data <- data.frame(
  date = as.POSIXct(c("2011-07-07", "2012-01-01")),
  ois_2y = c(NA, 0.5),
  de2y = c(0.3, NA),
  ois_5y = c(NA, 1.0),
  de5y = c(0.8, NA),
  ois_10y = c(NA, 1.5),
  de10y = c(1.2, NA),
  stringsAsFactors = FALSE
)

test_that("determine_suffix works correctly", {
  expect_equal(determine_suffix(data.frame(mew = 1)), "mew")
  expect_equal(determine_suffix(data.frame(prw = 1)), "release")
  expect_equal(determine_suffix(data.frame(pcw = 1)), "conference")
})

test_that("preprocess_data works correctly", {
  processed_data <- preprocess_data(mock_data)
  expect_equal(processed_data$ois_2y, c(0.3, 0.5))
  expect_equal(processed_data$ois_5y, c(0.8, 1.0))
  expect_equal(processed_data$ois_10y, c(1.2, 1.5))
})

test_that("select_ois_data works correctly", {
  processed_data <- preprocess_data(mock_data)
  selected_data <- select_ois_data(processed_data, c("2011-01-01", "2012-12-31"))
  expect_equal(nrow(selected_data), 2)
  expect_equal(names(selected_data), c("date", "ois_2y", "ois_5y", "ois_10y"))
})

test_that("filter_date_range works correctly", {
  filtered_data <- filter_date_range(mock_data, c("2011-01-01", "2011-12-31"))
  expect_equal(nrow(filtered_data), 1)
  expect_equal(filtered_data$date, as.POSIXct("2011-07-07"))
})

test_that("exclude_dates works correctly", {
  excluded_data <- exclude_dates(mock_data, c("2011-07-07"))
  expect_equal(nrow(excluded_data), 1)
  expect_equal(excluded_data$date, as.POSIXct("2012-01-01"))
})

test_that("load_hfd works correctly", {
  result <- load_hfd(mock_data, exclude_date = c("2011-07-07"), range = c("2011-01-01", "2012-12-31"), reproduce = TRUE, select_ois = TRUE)
  expect_equal(nrow(result), 1)
  expect_equal(result$date, as.POSIXct("2012-01-01"))
  expect_equal(names(result), c("date", "ois_2y", "ois_5y", "ois_10y"))
})
