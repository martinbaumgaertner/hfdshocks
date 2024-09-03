#' @title load_hfd
#'
#' @description Load and preprocess the Euro Area Monetary Policy Event-Study Database (EA-MPD)
#'
#' @param data A data frame containing the raw data.
#' @param exclude_dates Vector of dates to exclude.
#' @param date_range Defines the time window. Needs to be a vector of two dates.
#' @param reproduce Logical; TRUE for exact factors Altavilla et al. 2019 factors.
#' @param select_ois Logical; TRUE to select OIS data.
#'
#' @return A data frame containing the preprocessed data.
#'
#' @examples
#' data <- download_hfd("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx", getwd())
#' pcw <- load_hfd(data$pcw,
#'   exclude_dates = c("2001-09-17", "2008-10-08", "2008-11-06"),
#'   date_range = c("2001-12-31", "2018-09-13")
#' )
#' @export
#' @importFrom dplyr mutate filter filter_at vars any_vars select coalesce
#' @importFrom readr read_csv cols col_double col_datetime
load_hfd <- function(data, exclude_dates, date_range, reproduce = FALSE, select_ois = TRUE) {
  suffix <- determine_suffix(data)
  data <- preprocess_data(data)

  if (select_ois) {
    data <- select_ois_data(data, date_range)
  } else {
    data <- filter_date_range(data, date_range)
  }

  if (reproduce) {
    data <- apply_reproduce_corrections(data, suffix)
  }

  if (!is.null(exclude_dates)) {
    data <- exclude_dates_from_dat(data, exclude_dates)
  }

  return(data)
}

determine_suffix <- function(data) {
  if ("mew" %in% names(data)) {
    return("mew")
  } else if ("prw" %in% names(data)) {
    return("release")
  } else {
    return("conference")
  }
}

preprocess_data <- function(data) {
  data <- data %>%
    setNames(tolower(names(.))) %>%
    dplyr::mutate(ois_2y = dplyr::coalesce(ois_2y, de2y)) %>%
    dplyr::mutate(ois_5y = dplyr::coalesce(ois_5y, de5y)) %>%
    dplyr::mutate(ois_10y = dplyr::coalesce(ois_10y, de10y))
  return(data)
}

select_ois_data <- function(data, range) {
  data <- data %>%
    dplyr::select(date, contains("m"), contains("1y"), contains("2y"), contains("5y"), contains("10y"), -contains("15y")) %>%
    dplyr::select(date, starts_with("ois")) %>%
    dplyr::filter_at(dplyr::vars(-date), dplyr::any_vars(!is.na(.))) %>%
    dplyr::filter(date >= as.POSIXct(range[1], tz = "UTC") & date <= as.POSIXct(range[2], tz = "UTC"))
  return(data)
}

filter_date_range <- function(data, range) {
  data <- data %>%
    dplyr::filter(date >= as.POSIXct(range[1], tz = "UTC") & date <= as.POSIXct(range[2], tz = "UTC"))
  return(data)
}

apply_reproduce_corrections <- function(data, suffix) {
  if (suffix == "release") {
    data[data$date == as.POSIXct("2011-07-07", tz = "UTC"), "ois_10y"] <- -0.25 # (tiny) error in paper code uncomment to reproduce
  } else if (suffix == "conference") {
    data[data$date == as.POSIXct("2011-07-07", tz = "UTC"), "ois_10y"] <- 2.35 # (tiny) error in paper code uncomment to reproduce
  }
  return(data)
}

exclude_dates_from_dat <- function(data, exclude_date) {
  data <- data %>%
    dplyr::filter(!(date %in% as.POSIXct(exclude_date, tz = "UTC")))
  return(data)
}
