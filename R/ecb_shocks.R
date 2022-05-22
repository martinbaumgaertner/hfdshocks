#' @title ecb_shocks
#'
#' @description wrapper around all functions to download, process and return the ecb shocks based on Altavilla, C., Brugnolini, L., Gürkaynak, R. S., Motto, R., & Ragusa, G. (2019). Measuring euro area monetary policy. Journal of Monetary Economics, 108, 162-179.
#'
#' @param url specify which data to process
#' @param path Defines the download location
#' @param exclude_date vector of dates to exclude
#' @param range Defines time window. Needs to be a vector of two dates
#' @param crisis_date specify the starting date of the crisis for the rotation of the QE shocks
#' @param remove_data should local data be removed after calculation?
#' @param reproduce logical; TRUE for exact factors Altavilla et al. 2019 factors
#'
#' @return
#'
#' @examples
#' x<-ecb_shocks("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx","",
#'               exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),
#'               range=c("2001-12-31","2018-09-13"),crisis_date="2008-09-04")
#' @export
#' @importFrom
ecb_shocks<-function(url="https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",
                     path="",exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),range=c("2001-12-31","2018-09-13"),
                     crisis_date="2008-09-04",remove_data=T,reproduce=F,extended=F,extended_release_date="2015-12-03",
                     loadings=F,return_data="all",rotation_test=F){
  download_hfd(url,path)

  release<-load_hfd(paste0(path,"prw.csv"),exclude_date=exclude_date,range=range,reproduce=reproduce)
  conference<-load_hfd(paste0(path,"pcw.csv"),exclude_date=exclude_date,range=range,reproduce=reproduce)
  if(rotation_test==F){
    if(extended==F){
      release_factors<-rotate(release,crisis_date=crisis_date,window="release",extended=extended)
    }else{
      release_factors<-rotate(release,crisis_date=extended_release_date,window="release",extended=extended)
    }
  }else{
    if(extended==F){
      release_factors<-rotate1(release,crisis_date=crisis_date,window="release",extended=extended)
    }else{
      release_factors<-rotate2(release,crisis_date=extended_release_date,window="release",extended=extended)
    }
  }
  conference_factors<-rotate(conference,crisis_date=crisis_date,window="conference",extended=extended)

  out<-list("factors"=list("release"=release_factors,"conference"=conference_factors))

  if(loadings==T){
    loadings_release<-loadings(release,release_factors)
    loadings_conference<-loadings(conference,conference_factors)
    out$loadings=list("release"=loadings_release,"conference"=loadings_conference)
  }
  if(return_data=="all"){
    out$data=list("release"=load_hfd(paste0(path,"prw.csv"),exclude_date=exclude_date,range=range,reproduce=reproduce,select_ois = F),
                  "conference"=load_hfd(paste0(path,"pcw.csv"),exclude_date=exclude_date,range=range,reproduce=reproduce,select_ois = F),
                  "monetary"=load_hfd(paste0(path,"mew.csv"),exclude_date=exclude_date,range=range,reproduce=reproduce,select_ois = F))
  }else if(return_data=="ois"){
    out$data=list("release"=release,
                  "conference"=conference,
                  "monetary"=load_hfd(paste0(path,"mew.csv"),exclude_date=exclude_date,range=range,reproduce=reproduce,select_ois = T))
  }

  if(remove_data==T){
    unlink(paste0(path,"prw.csv"), recursive=TRUE)
    unlink(paste0(path,"pcw.csv"), recursive=TRUE)
    unlink(paste0(path,"mew.csv"), recursive=TRUE)
  }

  return(out)
}




