#' @title factor_model
#'
#' @description Computes the factor modell based on https://github.com/gragusa/Factotum.jl
#'
#' The Factotum.jl package is licensed under the MIT "Expat" License:
#'  Copyright (c) 2017: Giuseppe Ragusa.
#' Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#'  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FIt_nESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#'
#' @param z specify which data to process
#' @param center logical
#' @param scale_z logical
#'
#' @return
#'
#' @examples
#' @export
#' @importFrom
factor_model <- function(z, center = TRUE, scale_z = FALSE) {
  t_n <- nrow(z)
  nn <- ncol(z)
  if (center == TRUE) {
    mean_z <- colMeans(z)
  } else {
    mean_z <- rep(0, nn)
  }
  if (scale_z == TRUE) {
    sd_z <- apply(z, 2, sd)
  } else {
    sd_z <- rep(1, nn)
  }

  x <- sweep(sweep(z, 2, mean_z), 2, sd_z, "/")

  ev <- eigen(t(x) %*% x, only.values = FALSE)
  neg <- which(ev$values < 0)

  if (!length(neg) == 0) {
    break
  }
  lamda <- ev$values
  sigma <- sqrt(lamda / t_n)
  v_k <- sigma^2 / sum(sigma^2)
  lambda <- sqrt(nn) * ev$vectors
  fa <- x %*% lambda / nn

  return(list(factors = fa, loadings = lambda, eigenvalues = lamda, center = mean_z, scale = sd_z, data = z))
}
