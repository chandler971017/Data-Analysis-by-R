#' The 'MLP' class object
#'
#' This a R6 class. It inherits from 'kernel' class. It provides the mlp kernel function and the 1st order derivative of mlp kernel function. 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return an  \code{\link{R6Class}} object which can be used for the rkhs interpolation.
#' @format \code{\link{R6Class}} object.
#' @export
#' @author Mu Niu, \email{mu.niu@glasgow.ac.uk}


MLP <- R6Class("MLP",
  inherit = Kernel,
  public = list(

    greet = function() {
      cat(paste0("MLP is", self$k_par, ".\n"))
    },

    set_k_par = function(val) {
      self$k_par <- val
    },

    kern = function (t1,t2) {
      asin(   (self$k_par[1]*t1*t2+self$k_par[2]) /   sqrt(self$k_par[1]*t1^2+self$k_par[2]+1) / sqrt(self$k_par[1]*t2^2+self$k_par[2]+1)    )   
    },

    dkd_kpar = function(t1,t2) {
      w=self$k_par[1]; bb = self$k_par[2]
      x=t1;xt=t2
      num=(w*x*xt+bb)
      denom =  sqrt(w*x^2+bb+1)*sqrt(w*xt^2+bb+1)
      db = ( 1/denom - 1/2*num/denom^3*((x^2+xt^2)*w+2*bb+2) ) * 1/sqrt(1-( num/denom)^2 )  *bb    
      dw= ( x*xt/denom - 1/2*num/denom^3 * ( (w*x^2+bb+1)*xt^2 + (w*xt^2+bb+1)*x^2 ) ) * 1/sqrt(1-( num/denom)^2 )  *w
      c(dw,db)
    },

    dkdt = function (t1,t2) {
      w=self$k_par[1]; bb = self$k_par[2]
      x=t1;xt=t2
      num=(w*x*xt+bb)
      denom =  sqrt(w*x^2+bb+1)*sqrt(w*xt^2+bb+1)
      vec= xt^2*w+bb+1
      ( xt/denom - ( x*num*vec ) / denom^3 ) *w* 1/sqrt(1-( num/denom)^2 )  
    }

  )

)

