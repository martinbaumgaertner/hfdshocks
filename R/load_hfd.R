#' @title load_hfd
#'
#' @description Load and preprocesses the Euro Area Monetary Policy Event-Study Database (EA-MPD)
#'
#' @param path path to data
#' @param exclude_date vector of dates to exclude
#' @param range Defines time window. Needs to be a vector of two dates
#'  @param reproduce logical; TRUE for exact factors Altavilla et al. 2019 factors
#'
#' @return
#'
#' @examples
#' download_hfd("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",getwd())
#' pcw<-load_hfd(pcw.csv,exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),
#'               range=c("2001-12-31","2018-09-13"))
#' @export
#' @importFrom NlcOptim dplyr
load_hfd<-function(path,exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),range=c("2001-12-31","2018-09-13"),reproduce==T){

  if(grepl('mew', path)){
    suffix<-"mew"
  }else if(grepl('prw', path)){
    suffix<-"release"
  }else{
    suffix<-"conference"
  }

  data<-read_csv(path,col_types=cols(.default = col_double(),date = col_datetime(format = ""))) %>%
    setNames(tolower(names(.)))%>%
    mutate(ois_2y = coalesce(ois_2y, de2y)) %>%
    mutate(ois_5y = coalesce(ois_5y, de5y)) %>%
    mutate(ois_10y = coalesce(ois_10y, de10y))%>%
    select(date,contains("1m"),contains("3m"),contains("6m"),contains("1y"),contains("2y"),contains("5y"),contains("10y"),-contains("15y")) %>% #here too
    select(date,starts_with("ois"))%>%
    filter_at(vars(-date), any_vars(!is.na(.))) %>%
    filter(date >= range[1] & date<= range[2])

  if(reproduce==T){
    data[data$date==as.POSIXct("2011-07-07",tz="UTC"),"ois_10y"]<--0.24999999999999999 #(tiny) error in paper code uncomment to reproduce
  }

  if(!is.null(exclude_date)){
    data<-data %>%
      filter(!(date %in% as.POSIXct(exclude_date,tz="UTC")))
  }

  data<-data %>%
    setNames(paste0(names(.),"_",suffix)) %>%
    rename(date=paste0("date_",suffix))

  return(data)
}
