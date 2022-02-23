#' The 'RBF' class object
#'
#' This a R6 class. It inherits from 'kernel' class. It provides the rbf kernel function and the 1st order derivative of rbf kernel function. 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return an  \code{\link{R6Class}} object which can be used for the rkhs interpolation.
#' @format \code{\link{R6Class}} object.
#' @export
#' @author Mu Niu, \email{mu.niu@glasgow.ac.uk}

RBF <- R6Class("RBF",
  inherit = Kernel,
  public = list(

    greet = function() {
      cat(paste0("RBF len is", self$k_par, ".\n"))
    },

    set_k_par = function(val) {
      self$k_par <- val
    },


    kern = function (t1,t2) {
      x=t1-t2
      1/2*exp( -x^2/(2*self$k_par) )
    },

    dkd_kpar = function(t1,t2) {
      x=t1-t2
      1/2*exp( -x^2/(2*self$k_par) ) * x^2 / (2*self$k_par^2)*self$k_par
    },

   dkdt = function (t1,t2) {
     x=(t1-t2)
     #1/2*exp( -x^2/(2*self$k_par) ) * 1/self$k_par*(-x)
     1/2*exp( -x^2/(2*self$k_par) ) * 1/self$k_par*(-x)
    }

  )

)

