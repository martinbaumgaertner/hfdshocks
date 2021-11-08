loadings<-function(ois_data,factors){
  loadings_release<-full_join(ois_data,factors,by="date") %>%
    dplyr::select(-date)%>%
    pivot_longer(cols=!starts_with("ois"),names_to = "factor",values_to = "shock") %>%
    pivot_longer(cols=starts_with("ois"),names_to = "ois",values_to = "ois_value")  %>%
    split(list(.$factor,.$ois))%>%
    map(~ lm(ois_value ~ shock, data = .x))%>%
    map( function(u) tibble(coef=coef(u)[2],ser=sqrt(vcovHC(u)[2,2]),r2=summary(u)$r.squared)) %>%
    rbindlist(idcol = TRUE ) %>%
    tibble() %>%
    separate(.id,c("shock","ois"),"\\.") %>%
    mutate(ois=str_remove(ois,"_release|_conference"))
  return(loadings_release)
}
