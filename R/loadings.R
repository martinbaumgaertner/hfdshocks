#' @title loadings
#'
#' @description Computes the loadings of OIS data on factors.
#'
#' @param ois_data A data frame containing OIS data.
#' @param factors A data frame containing factors.
#'
#' @return A tibble containing the loadings, standard errors, and R-squared values.
#'
#' @importFrom dplyr full_join select bind_rows starts_with
#' @importFrom tidyr pivot_longer separate
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom sandwich vcovHC
#' @importFrom stats lm coef
loadings <- function(ois_data, factors) {
  # Join OIS data with factors by date
  joined_data <- dplyr::full_join(ois_data, factors, by = "date") %>%
    dplyr::select(-date)

  # Pivot data to long format for factors and OIS values
  long_data <- joined_data %>%
    tidyr::pivot_longer(cols = !dplyr::starts_with("ois"), names_to = "factor", values_to = "shock") %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("ois"), names_to = "ois", values_to = "ois_value")

  # Split data by factor and OIS
  split_data <- split(long_data, list(long_data$factor, long_data$ois))

  # Compute loadings, standard errors, and R-squared values
  loadings_list <- purrr::map(split_data, function(data) {
    model <- lm(ois_value ~ shock, data = data)
    tibble::tibble(
      coef = coef(model)[2],
      ser = sqrt(sandwich::vcovHC(model)[2, 2]),
      r2 = summary(model)$r.squared
    )
  })

  # Combine results into a single tibble
  loadings_release <- dplyr::bind_rows(loadings_list, .id = "id") %>%
    tidyr::separate(id, c("factor", "ois"), "\\.")

  return(loadings_release)
}