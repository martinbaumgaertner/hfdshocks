#' @title rotate
#'
#' @description Computes the rotated factor modell according to Altavilla et al. 2019
#'
#' @param data specify which data to process
#' @param crisis_date specify the starting date of the crisis for the rotation of the QE shocks
#' @param window which window do you want to rotate? choose between "release" or "conference"
#'
#' @return
#'
#' @examples
#' download_hfd("https://www.ecb.europa.eu/pub/pdf/annex/Dataset_EA-MPD.xlsx",getwd())
#' pcw<-load_hfd(pcw.csv,exclude_date=c("2001-09-17","2008-10-08","2008-11-06"),
#'               range=c("2001-12-31","2018-09-13"))
#' rotate(pcw,crisis_date="2008-09-04",window="conference")
#' @export
#' @importFrom NlcOptim dplyr stats lm
rotate<-function(data,crisis_date="2008-09-04",window="release",extended){

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
  Factors<-sweep(fm$factors, 2, scale, "/")[,1:3] #I use a maximum of 3 factors

  idx_pre<-1:(which(date_vector==as.POSIXlt(crisis_date,tz="UTC"))-1)
  idx_post<-(which(date_vector==as.POSIXlt(crisis_date,tz="UTC"))-1):Tn

  #restrict and solve model
  ID<-list(Fa=Factors[idx_pre,],L=(fm$loadings[,1:3]*scale[1:3]))

  con=function(x){
    loading<-ID$L
    f=NULL
    #orthogonal restrictions
    f=rbind(f,x[1]^2 + x[4]^2 + x[7]^2-1)
    f=rbind(f,x[2]^2 + x[5]^2 + x[8]^2-1)
    f=rbind(f,x[3]^2 + x[6]^2 + x[9]^2-1)
    f=rbind(f,x[1]*x[2] + x[4]*x[5] + x[7]*x[8]-0)
    f=rbind(f,x[1]*x[3] + x[4]*x[6] + x[7]*x[9]-0)
    f=rbind(f,x[2]*x[3] + x[5]*x[6] + x[8]*x[9]-0)
    #second and third factors does not load on one month rate
    f=rbind(f,x[4]*loading[1,1] + x[5]*loading[1,2] + x[6]*loading[1,3]-0)
    f=rbind(f,x[7]*loading[1,1] + x[8]*loading[1,2] + x[9]*loading[1,3]-0)
    return(list(ceq=f,c=NULL))
  }


  if(extended==T&window=="release"){
    obj<-function(x){
      U=matrix(c(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9]),nrow=3)
      xx<-ID$Fa%*%U[,2]

      out<-0.5*t(xx)%*%xx/length(xx)
      as.numeric(out)
    }
  }else{
    obj<-function(x){
      U=matrix(c(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9]),nrow=3)
      xx<-ID$Fa%*%U[,3]

      out<-0.5*t(xx)%*%xx/length(xx)
      as.numeric(out)
    }
  }

  sol<-solnl(c(diag(3)),objfun=obj,confun=con)



  #rotate factors
  rotate_factors<-Factors%*%matrix(sol$par,nrow=3) %>%
    as_tibble(.,.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))

  #rename and scale based on corresponding ois rate
  if(window=="release"){

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

  }else{
    rotate_factors<-rotate_factors %>%
      dplyr::select(1:3) %>%
      dplyr::rename(Timing=1,FG=2,QE=3)

    full<-bind_cols(data %>%
                      dplyr::select(date),rotate_factors,ois_matrix %>% as_tibble(.))
    scale_4 <-coef(lm(ois_6m~Timing, data = full))[2]
    scale_5 <-coef(lm(ois_2y~FG, data = full))[2]
    scale_6 <-coef(lm(ois_10y~QE, data = full))[2]

    rotate_factors<-rotate_factors %>%
      dplyr::mutate(Timing = Timing*scale_4,
             FG = FG*scale_5,
             QE = QE*scale_6)
  }


  factors_scaled<-bind_cols(data %>%
                              dplyr::select(date),rotate_factors)

  return(factors_scaled)
}
