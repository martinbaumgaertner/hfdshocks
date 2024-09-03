#' @title download_hfd
#'
#' @description Downloads high-frequency data from a specified URL and stores it in the specified path.
#'
#' @param url A string. The URL of the high-frequency data (.xlsx file).
#' @param path A string. The directory where the downloaded data will be stored.
#'
#' @return NULL. The function downloads the data and saves it as CSV files in the specified path.
#'
#' @examples
#' download_hfd("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx", getwd())
#' @export
#' @importFrom dplyr mutate
#' @importFrom readxl read_excel
#' @importFrom curl curl_download
#' @importFrom readr write_csv
download_hfd <- function(url, path) {
  validate_inputs(url, path)

  temp_file <- file.path(path, "temporary.xlsx")

  download_file(url, temp_file)
  data <- process_sheets(temp_file)

  file.remove(temp_file)

  return(data)
}

validate_inputs <- function(url, path) {
  if (!grepl("\\.xlsx$", url)) {
    stop("Please insert a link which refers to an existing .xlsx file.")
  }

  if (!dir.exists(path)) {
    stop("The specified path does not exist.")
  }
}

download_file <- function(url, destfile) {
  tryCatch(
    {
      curl::curl_download(url, destfile)
    },
    error = function(e) {
      stop("Failed to download the file: ", e$message)
    }
  )
}

process_sheets <- function(temp_file) {
  tryCatch(
    {
      prw <- readxl::read_excel(temp_file, sheet = "Press Release Window")

      pcw <- readxl::read_excel(temp_file, sheet = "Press Conference Window") %>%
        dplyr::mutate(date = prw$date) # small formatting fix for excel mistake

      mew <- readxl::read_excel(temp_file, sheet = "Monetary Event Window")

      out <- list(prw = prw, pcw = pcw, mew = mew)

      return(out)

    },
    error = function(e) {
      stop("Failed to read or process the Excel file: ", e$message)
    }
  )
}
