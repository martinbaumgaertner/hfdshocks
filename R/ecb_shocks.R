#' @title ecb_shocks
#'
#' @description Wrapper around all functions to download, process, and return the ECB shocks based on Altavilla et al. (2019).
#'
#' @param url specify which data to process
#' @param path Defines the download location. Note that the folder must exist.
#' @param exclude_dates vector of dates to exclude
#' @param date_range Defines time window. Needs to be a vector of two dates
#' @param crisis_date specify the starting date of the crisis for the rotation of the QE shocks
#' @param reproduce logical; TRUE for exact factors Altavilla et al. 2019 factors
#' @param extended logical; TRUE for identification based on Baumgärtner (2021)
#' @param extended_release_date character; Date for the second rotation
#' @param include_loadings logical; TRUE to include loadings in the output
#' @param return_data_type character; "all" to return all data, "ois" to return only OIS data
#'
#' @return A list containing the factors per window, the loadings per window, and the raw data per window if chosen.
#'
#' @examples
#' x <- ecb_shocks("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx", getwd(),
#'   exclude_dates = c("2001-09-17", "2008-10-08", "2008-11-06"),
#'   date_range = c("2001-12-31", "2018-09-13"), crisis_date = "2008-09-04"
#' )
#' @export
#' @importFrom dplyr mutate
#' @importFrom readxl read_excel
#' @importFrom curl curl_download
#' @importFrom readr write_csv
ecb_shocks <- function(url = "https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",
                       path = getwd(), exclude_dates = c("2001-09-17", "2008-10-08", "2008-11-06"), date_range = c("2001-12-31", "2018-09-13"),
                       crisis_date = "2008-09-04", reproduce = FALSE, extended = FALSE, extended_release_date = "2015-12-03",
                       include_loadings = FALSE, return_data_type = "all") {
  data <- download_hfd(url, path)

  release <- load_hfd(data$prw, exclude_dates = exclude_dates, date_range = date_range, reproduce = reproduce)
  conference <- load_hfd(data$pcw, exclude_dates = exclude_dates, date_range = date_range, reproduce = reproduce)

  release_factors <- get_factors(release, crisis_date, extended, extended_release_date, "release")
  conference_factors <- get_factors(conference, crisis_date, extended, extended_release_date, "conference")

  out <- list(
    factors = list(release = release_factors, conference = conference_factors),
    loadings = if (include_loadings) get_loadings(release, conference, release_factors, conference_factors) else NULL,
    data = get_data(return_data_type, data, exclude_dates, date_range, reproduce, release, conference)
  )

  return(out)
}

get_factors <- function(data, crisis_date, extended, extended_release_date, window) {
  rotate(data, crisis_date = if (extended) extended_release_date else crisis_date, window = window, extended = extended)
}

get_loadings <- function(release, conference, release_factors, conference_factors) {
  list(
    release = loadings(release, release_factors),
    conference = loadings(conference, conference_factors)
  )
}

get_data <- function(return_data_type, data, exclude_dates, date_range, reproduce, release, conference) {
  if (return_data_type == "all") {
    list(
      release = load_hfd(data$prw, exclude_dates = exclude_dates, date_range = date_range, reproduce = reproduce, select_ois = FALSE),
      conference = load_hfd(data$pcw, exclude_dates = exclude_dates, date_range = date_range, reproduce = reproduce, select_ois = FALSE),
      monetary = load_hfd(data$mew, exclude_dates = exclude_dates, date_range = date_range, reproduce = reproduce, select_ois = FALSE)
    )
  } else if (return_data_type == "ois") {
    list(
      release = release,
      conference = conference,
      monetary = load_hfd(data$mew, exclude_dates = exclude_dates, date_range = date_range, reproduce = reproduce, select_ois = TRUE)
    )
  }
}
