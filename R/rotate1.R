rotate1<-function(data,crisis_date="2008-09-04",window="release",extended){

  ois_matrix<-data %>%
    dplyr::select(starts_with("ois"))%>%
    as.matrix()

  date_vector<-data %>%
    pull(date)

  Tn<-nrow(ois_matrix)
  nn<-ncol(ois_matrix)

  #estimate facormodel
  fm<-factor_model(ois_matrix)

  #scale (not needed)
  scale<-apply(fm$factors, 2, sd)
  Factors<-sweep(fm$factors, 2, scale, "/")[,1] #I use a maximum of 3 factors

  #rotate factors
  rotate_factors<-Factors

  #rename and scale based on corresponding ois rate

    rotate_factors<-rotate_factors %>%
      tibble(Target=.)

      full<-bind_cols(data %>%
                        dplyr::select(date),rotate_factors,ois_matrix %>% as_tibble(.))

      scale_1 <-coef(lm(ois_1m~Target, data = full))[2]

      rotate_factors1<-rotate_factors %>%
        dplyr::mutate(Target = Target*scale_1)


  factors_scaled<-bind_cols(data %>%
                              dplyr::select(date),rotate_factors)

  return(factors_scaled)
  }
