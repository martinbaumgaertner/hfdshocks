library(testthat)
library(mockery)

# Mock data for testing
mock_data <- list(
  prw = data.frame(date = as.POSIXct("2021-01-01"), value = 1),
  pcw = data.frame(date = as.POSIXct("2021-01-01"), value = 2),
  mew = data.frame(date = as.POSIXct("2021-01-01"), value = 3)
)

# Mock functions
mock_download_hfd <- function(url, path) {
  return(mock_data)
}

mock_load_hfd <- function(data, exclude_date, range, reproduce, select_ois = FALSE) {
  return(data)
}

mock_get_factors <- function(data, crisis_date, extended, extended_release_date, rotation_test, window) {
  return(data.frame(factor = 1))
}

mock_get_loadings <- function(release, conference, release_factors, conference_factors) {
  return(list(release = data.frame(loading = 1), conference = data.frame(loading = 2)))
}

mock_get_data <- function(return_data, data, exclude_date, range, reproduce, release, conference) {
  return(list(release = release, conference = conference, monetary = data$mew))
}

test_that("get_factors works correctly", {
  data <- data.frame(value = 1)
  result <- get_factors(data, "2008-09-04", FALSE, "2015-12-03", FALSE, "release")
  expect_equal(result, data.frame(factor = 1))
})

test_that("get_loadings works correctly", {
  release <- data.frame(value = 1)
  conference <- data.frame(value = 2)
  release_factors <- data.frame(factor = 1)
  conference_factors <- data.frame(factor = 2)
  result <- get_loadings(release, conference, release_factors, conference_factors)
  expect_equal(result$release, data.frame(loading = 1))
  expect_equal(result$conference, data.frame(loading = 2))
})

test_that("get_data works correctly", {
  result <- get_data("all", mock_data, c("2021-01-01"), c("2021-01-01", "2021-12-31"), FALSE, mock_data$prw, mock_data$pcw)
  expect_equal(result$release, mock_data$prw)
  expect_equal(result$conference, mock_data$pcw)
  expect_equal(result$monetary, mock_data$mew)
})