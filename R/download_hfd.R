#' @title download_hfd
#'
#' @description Downloads the high-frequency data from a website and store it
#'
#' @param url url of the high frequency data
#' @param path Defines the download location
#'
#' @return
#'
#' @examples
#' download_hfd("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",getwd())
#' @export
#' @importFrom NlcOptim dplyr readxl curl
download_hfd<-function(url,path){
  if(!grepl('\\.xlsx$', url)){
    stop("Please insert a link which refers to an existing .xlsx file.")
  }

  curl::curl_download(url, paste0(path,"temporary.xlsx"))

  prw <- readxl::read_excel(paste0(path,"temporary.xlsx"),sheet = "Press Release Window", lazy = FALSE)
  readr::write_csv(prw,paste0(path, "prw.csv"))

  pcw <- readxl::read_excel(paste0(path,"temporary.xlsx"),sheet = "Press Conference Window", lazy = FALSE) %>%
    mutate(date=prw$date) #small formating fix for excel mistake
  readr::write_csv(pcw,paste0(path, "pcw.csv"))

  mew <- readxl::read_excel(paste0(path,"temporary.xlsx"),sheet = "Monetary Event Window", lazy = FALSE)
  readr::write_csv(mew,paste0(path, "mew.csv"))

  file.remove(paste0(path,"temporary.xlsx"))
}

