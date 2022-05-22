rotate2<-function(data,crisis_date="2008-09-04",window="release",extended){

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
  Factors<-sweep(fm$factors, 2, scale, "/")[,1:2] #I use a maximum of 3 factors

  idx_pre<-1:(which(date_vector==as.POSIXlt(crisis_date,tz="UTC"))-1)
  idx_post<-(which(date_vector==as.POSIXlt(crisis_date,tz="UTC"))-1):Tn

  #restrict and solve model
  ID<-list(Fa=Factors[idx_pre,],L=(fm$loadings[,1:2]*scale[1:2]))

  con=function(x){
    loading<-ID$L
    f=NULL
    #orthogonal restrictions
    f=rbind(f,x[1]^2 + x[3]^2 - 1)
    f=rbind(f,x[2]^2 + x[4]^2 - 1)
    f=rbind(f,x[1]*x[2] + x[3]*x[4] - 0)
    #f=rbind(f,x[2]*x[1] + x[4]*x[3] - 0)
    #second and third factors does not load on one month rate
    f=rbind(f,x[2]*loading[1,1] + x[4]*loading[1,2] -0)
    return(list(ceq=f,c=NULL))
  }

    obj<-function(x){
      U=matrix(c(x[1],x[2],x[3],x[4]),nrow=2)
      xx<-ID$Fa%*%U[,2]

      out<-0.5*t(xx)%*%xx/length(xx)
      as.numeric(out)
    }

  sol<-solnl(c(diag(2)),objfun=obj,confun=con)



  #rotate factors
  rotate_factors<-Factors%*%matrix(sol$par,nrow=2) %>%
    as_tibble(.,.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))

  #rename and scale based on corresponding ois rate

    if(extended==T){
      rotate_factors<-rotate_factors %>%
        dplyr::select(1:2) %>%
        dplyr::rename(Target=1,
                      QE=2)

      full<-bind_cols(data %>%
                        dplyr::select(date),rotate_factors,ois_matrix %>% as_tibble(.))

      scale_1 <-coef(lm(ois_1m~Target, data = full))[2]
      scale_2 <-coef(lm(ois_10y~QE, data = full))[2]

      rotate_factors<-rotate_factors %>%
        dplyr::mutate(Target = Target*scale_1,
                      QE = QE*scale_2)
    }else{
      rotate_factors<-rotate_factors %>%
        dplyr::select(1) %>%
        dplyr::rename(Target=1)

      full<-bind_cols(data %>%
                        dplyr::select(date),rotate_factors,ois_matrix %>% as_tibble(.))

      scale_1 <-coef(lm(ois_1m~Target, data = full))[2]

      rotate_factors<-rotate_factors %>%
        dplyr::mutate(Target = Target*scale_1)
    }


  factors_scaled<-bind_cols(data %>%
                              dplyr::select(date),rotate_factors)

  return(factors_scaled)
  }
