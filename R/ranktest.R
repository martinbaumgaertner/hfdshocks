# download_hfd(url,path)
# reproduce=T
# range=c("2001-12-31","2021-11-13")
# pcw<-load_hfd(paste0(path,"pcw.csv"),exclude_date=exclude_date,range=range,reproduce=reproduce)
# prw<-load_hfd(paste0(path,"prw.csv"),exclude_date=exclude_date,range=range,reproduce=reproduce)
#
#
#
# ois_matrix<-prw %>%
#   select(starts_with("ois"))%>%
#   as.matrix()
# write.table(ois_matrix,"Xrel2.csv",col.names = F,row.names = F)
# date_vector<-prw %>%
#   dplyr::pull(date)
# write.table(date_vector,"Drel2.csv",col.names = F,row.names = F)
#
# ois_matrix<-pcw %>%
#   select(starts_with("ois"))%>%
#   as.matrix()
# write.table(ois_matrix,"Xcon2.csv",col.names = F,row.names = F)
# date_vector<-pcw %>%
#   dplyr::pull(date)
# write.table(date_vector,"Dcon2.csv",col.names = F,row.names = F)
#
# date_vector <- read_csv("Drel.csv", col_names = FALSE)
# ois_matrix<-read_csv("Xrel.csv", col_names = FALSE) %>%
#   as.matrix()
#
#
#
# subsample=which(date_vector==as.POSIXct("2014-01-09",tz="UTC"))
#
# minrank=0
# maxrank=3
#
# #estimate facormodel
# fm<-factor_model(ois_matrix)
# factormodel<-fm
#
#
# theta_initial_value<-function(nn,k){
#   I3=matrix(1/3,1,nn)
#   ek=cbind(diag(rep(1,k)),matrix(0,k,(nn-k)))
#   if(k==0){
#     third_row=NULL
#     last_row=NULL
#   }else{
#     third_row=ek/(2*k)
#     last_row=(ek/(2*k))[, rev(seq_len(ncol(ek)))]
#   }
#
#
#   t0=list(rbind(I3,matrix(0,k,nn)),
#           rbind(I3,matrix(1/(2*k),k,nn)),
#           rbind(I3,third_row),
#           rbind(I3,last_row))
#   if(k==1){
#     rownames(t0[[4]])<-NULL
#   }
#
#   return(t0)
# }
#
#
#
# th=j
#
# th<-as.vector(t(theta0[[j]]))
#
# waldobjfun<-function(th,k,vecsigma,Vhat){
#   theta=matrix(th, nrow=1+k, ncol=nn,byrow = T)
#   if(k==0){
#     sigmamat=diag(theta[1,]^2)
#   }else{
#     sigmamat=diag(theta[1,]^2)+theta[2:(r+1),]%*%t(theta[2:(r+1),])
#   }
#
#   tempsigma=matrix(1,dim(sigmamat)[1],dim(sigmamat)[2])
#   tempsigma[upper.tri(tempsigma)]=0
#   tempsigma=sigmamat[get_cart(tempsigma)]
#   out=as.numeric(pracma::mrdivide((vecsigma -tempsigma), Vhat, pinv = F)%*%(vecsigma - tempsigma))
#
#   return(out)
# }
#
# waldtest<-function(factormodel,minrank,maxrank){
#  x<-factormodel$data
#  Tn<-nrow(x)
#  nn<-ncol(x)
#  #normalize
#  xs=t(t(x) / sqrt(diag(cov(x))))
#  covX = as.matrix(cov(xs))
#  meanX = colMeans(xs)
#  vecsigma=vech(covX)
#  bigN=length(vecsigma)
#
#  Vhat=matrix(NA,bigN,bigN)
#
#  varvecsig=array(0, dim=c(nn,nn,nn,nn))
#
#  for(i1 in 1:nn){
#    for(i2 in 1:nn){
#      for(i3 in 1:nn){
#        for(i4 in 1:nn){
#          varvecsig[i1,i2,i3,i4]=sum((xs[,i1]-meanX[i1])*(xs[,i2]-meanX[i2])*(xs[,i3]-meanX[i3])*(xs[,i4]-meanX[i4]))/Tn^2-covX[i1,i2]*covX[i3,i4]/Tn
#        }
#      }
#    }
#  }
#  idx=get_cart(covX)
#  for (i in 1:bigN){
#    for (j in 1:bigN){
#      Vhat[i,j]=varvecsig[idx[i,1],idx[i,2],idx[j,1],idx[j,2]]
#    }
#  }
#
#  dfa=list()
#
#  for(k in minrank:maxrank){
#    df = (nn-k)*(nn-k+1)/2 - nn
#    theta0=theta_initial_value(nn,k)
#    outs<-rep(NA,length(theta0))
#    for(j in 1:length(theta0)){
#
#      j1=as.vector(t(theta0[[j]]))
#
#      out<-nloptr(j1,eval_f=waldobjfun,vecsigma=vecsigma,k=k,Vhat=Vhat,opts = list("algorithm"=c("NLOPT_LD_LBFGS"),
#                                                                                   "maxeval"=1000000000,
#                                                                                   "xtol_rel"=NULL,
#                                                                                   "xtol_abs"=NULL))
#      outs[j]<-out$objective
#    }
#    out<-outs[which.min(outs)]
#    dfa[[k+1]]=tibble(rank=k,waldstat=out,df=df,pvalue=(1-pchisq(out, df =df)))
#
#    }
#  dfa %>%
#    bind_rows()
#  dist(idx)
# }
#
# library(JuliaCall)
# julia_setup()
#
# julia_eval("a = sqrt(2.0)")
#
# waldobjfun,vecsigma=vecsigma,k=k,Vhat=Vhat
#
#
# regjl <- juliaEval("
#   function reg(x,y)
#     n=size(x,1)
#     xreg=hcat(ones(size(x)[1],1),x)
#     k=size(xreg,2)
#     p1=((xreg'xreg)^(-1))
#     b=p1*xreg'y
#     r=y-xreg*b
#     sig=(r'r)/(n-k)
#     vmat=sig[1]*p1
#     sigb=sqrt(diag(vmat))
#     t=b./sigb
#     return (b,t)
#   end
# ")
# # = Tell R regjl is a function = #
# regjl_function=JuliaFunction(regjl)
#
#
# julia_eval("Optim.optimize(wf, j, BFGS(), Optim.Options(allow_f_increases=true); autodiff=:forward)")
#
#
#
# get_cart<-function(matrix){
#   idx=matrix(1,dim(matrix),dim(matrix))
#   lower<-lower.tri(idx)
#   diag(lower)<-T
#   idx[!lower]<-0
#
#   out<-which(idx !=0, arr.ind = T)
#   return(out)
# }
#
# vech<-function(mat){
#   #function to grap the lower matrix including diag
#   lower<-lower.tri(mat)
#   diag(lower)<-T
#   out<-mat[lower]
#   return(out)
# }
#
#
#
