#' @title load_hfd
#'
#' @description Load and preprocesses the Euro Area Monetary Policy Event-Study Database (EA-MPD)
#'
#' @param path path to data
#' @param exclude_date vector of dates to exclude
#' @param range Defines time window. Needs to be a vector of two dates
#'
#' @return
#'
#' @examples
#' download_hfd("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",getwd())
#' pcw<-load_hfd(pcw.csv,exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),
#'               range=c("2001-12-31","2018-09-13"))
#' @export
#' @importFrom NlcOptim dplyr
load_hfd<-function(path,exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),range=c("2001-12-31","2018-09-13")){

  if(grepl('mew', path)){
    suffix<-"mew"
  }else if(grepl('prw', path)){
    suffix<-"release"
  }else{
    suffix<-"conference"
  }

  data<-read_csv(path) %>%
    setNames(tolower(names(.)))%>%
    mutate(ois_2y_d = coalesce(ois_2y_d, de2y_d)) %>%
    mutate(ois_5y_d = coalesce(ois_5y_d, de5y_d)) %>%
    mutate(ois_10y_d = coalesce(ois_10y_d, de10y_d))%>%
    select(date,contains("1m"),contains("3m"),contains("6m"),contains("1y"),contains("2y"),contains("5y"),contains("10y"),-contains("15y")) %>% #here too
    select(date,starts_with("ois"))%>%
    filter_at(vars(-date), any_vars(!is.na(.))) %>%
    filter(date >= range[1] & date<= range[2])

  if(!is.null(exclude_date)){
    data<-data %>%
      filter(!(date %in% exclude_date))
  }

  data<-data %>%
    setNames(paste0(names(.),"_",suffix)) %>%
    rename(date=paste0("date_",suffix))

  return(data)
}