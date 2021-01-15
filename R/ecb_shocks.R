#' @title ecb_shocks
#'
#' @description wrapper around all functions to download, process and return the ecb shocks based on Altavilla, C., Brugnolini, L., Gürkaynak, R. S., Motto, R., & Ragusa, G. (2019). Measuring euro area monetary policy. Journal of Monetary Economics, 108, 162-179.
#'
#' @param url specify which data to process
#' @param path Defines the download location
#' @param exclude_date vector of dates to exclude
#' @param range Defines time window. Needs to be a vector of two dates
#' @param crisis_date specify the starting date of the crisis for the rotation of the QE shocks
#'
#' @return
#'
#' @examples
#' x<-ecb_shocks("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",getwd(),
#'               exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),
#'               range=c("2001-12-31","2018-09-13"),crisis_date="2008-09-04")
#' @export
#' @importFrom
ecb_shocks<-function(url="https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",
                     path=getwd(),exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),range=c("2001-12-31","2018-09-13"),crisis_date="2008-09-04"){
  download_hfd(url,path)
  pcw<-load_hfd(paste0(path,"/pcw.csv"),exclude_date=exclude_date,range=range)
  prw<-load_hfd(paste0(path,"/prw.csv"),exclude_date=exclude_date,range=range)
  release<-rotate(prw,crisis_date=crisis_date,window="release")
  conference<-rotate(pcw,crisis_date=crisis_date,window="conference")
  return(full_join(release,conference,by="date"))
}
