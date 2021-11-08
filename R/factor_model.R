#' @title factor_model
#'
#' @description Computes the factor modell based on https://github.com/gragusa/Factotum.jl
#'
#'The Factotum.jl package is licensed under the MIT "Expat" License:
#'  Copyright (c) 2017: Giuseppe Ragusa.
#'Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#'  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#'
#' @param Z specify which data to process
#' @param center logical
#' @param scaleZ logical
#'
#' @return
#'
#' @examples
#'
#' @export
#' @importFrom
factor_model<-function(Z,center=T,scaleZ=F){
  Tn<-nrow(Z)
  nn<-ncol(Z)
  if(center==T){
    meanZ=colMeans(Z)
  }else{
    meanZ=rep(0,nn)
  }
  if(scaleZ==T){
    sdZ=apply(Z, 2, sd)
  }else{
    sdZ=rep(1,nn)
  }

  X=sweep(sweep(Z,2,meanZ), 2, sdZ, "/")

  ev<-eigen(t(X)%*%X,only.values=F)
  neg<-which(ev$values<0)

  if(!length(neg)==0){
    break
  }
  lamda<-ev$values
  sigma<-sqrt(lamda/Tn)
  v_k<-sigma^2/sum(sigma^2)
  Lambda<-sqrt(nn)*ev$vectors
  Fa=X%*%Lambda/nn

  return(list(factors=Fa,loadings=Lambda,eigenvalues=lamda,center=meanZ,scale=sdZ,data=Z))
}
