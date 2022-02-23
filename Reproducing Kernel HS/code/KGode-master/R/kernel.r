#' The 'Kernel' class object
#'
#' This a abstract class     provide the kernel function and the 1st order derivative of rbf kernel function. 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return an  \code{\link{R6Class}} object which can be used for the rkhs interpolation.
#' @format \code{\link{R6Class}} object.
#' @field k_par vector(of length n_hy) containing the hyper-parameter of kernel. n_hy is the length of kernel hyper parameters.
#' @section Methods:
#' \describe{
#'   \item{\code{kern(t1,t2)}}{This method is used to calculate the kernel function given two one dimensional real inputs.}   
#'   \item{\code{dkd_kpar(t1,t2)}}{This method is used to calculate the gradient of kernel function against the kernel hyper parameters given two one dimensional real inputs.}   
#'   \item{\code{dkdt(t1,t2)}}{This method is used to calculate the 1st order derivative of kernel function given two one dimensional real inputs.} }
#' @export
#'
#' @author Mu Niu, \email{mu.niu@glasgow.ac.uk}

Kernel<-R6Class("Kernel",
  public = list(
    k_par=NULL,

    initialize = function(k_par = NULL) {
      self$k_par <- k_par
      self$greet()
    },
    
    greet = function() {
    },

    kern = function (t1,t2) {
    },

    dkd_kpar = function(t1,t2) {
    },

  dkdt = function (t1,t2) {  
    }

  )
)





Sigmoid <- R6Class("sigmoid",
  inherit = Kernel,
  public = list(

    #greet = function() {
    #  cat(paste0("sigmoid len is", self$k_par, ".\n"))
    #},

    set_k_par = function(val) {
      self$k_par <- val
    },

    kern = function (t1,t2) {
      x=t1-t2
      1/( 1+exp(-x*self$k_par) )
    },

    dkd_kpar = function(t1,t2) {
      x=t1-t2
      l = self$k_par
      if( (x*l)> -20 ){
      ds=-( 1+exp(-x*l) )^(-2)* exp(-x*l) *(-l)
       }
       else {
      ds = exp(x*l+log(l))
       }
     ds
    },

   dkdt = function (t1,t2) {
     x=(t1-t2)
     l = self$k_par
     bas = exp(-x*l)
      if(is.infinite(bas)){
       dsdt = 0#( 1+exp(-x*l) )^(-2)* exp(-x*l) *l#0
        }else
       {
       dsdt = (1+bas)^(-2)*l*bas
       }
      return(dsdt)
     }

  )

)

