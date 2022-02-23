#' The 'ode' class object
#'
#' This class provide all information about odes and methods for numerically solving odes. 
#' @docType class
#' @importFrom R6 R6Class
#' @import pracma
#' @import mvtnorm
#' @import pspline
#' @export
#' @keywords data
#' @return an  \code{\link{R6Class}} object which can be used for gradient matching.
#' @format \code{\link{R6Class}} object.
#' @field ode_par vector(of length n_p) containing ode parameters. n_p is the number of ode parameters.
#' @field ode_fun function containing the ode function.
#' @field t vector(of length n_o) containing time points of observations. n_o is the length of time points.
#' @section Methods:
#' \describe{
#'   \item{\code{solve_ode(par_ode,xinit,tinterv)}}{This method is used to solve ode numerically.} 
#'   \item{\code{optim_par(par,y_p,z_p)}}{This method is used to estimate ode parameters by standard gradient matching.}
#'   \item{\code{lossNODE(par,y_p,z_p)}}{This method is used to calculate the mismatching between gradient of interpolation and gradient from ode.}
#'  }	
#' @export
#' @examples
#' noise = 0.1  ## set the variance of noise
#' SEED = 19537
#' set.seed(SEED)
#' ## Define ode function, we use lotka-volterra model in this example. 
#' ## we have two ode states x[1], x[2] and four ode parameters alpha, beta, gamma and delta.
#' LV_fun = function(t,x,par_ode){
#'   alpha=par_ode[1]
#'   beta=par_ode[2]
#'   gamma=par_ode[3]
#'   delta=par_ode[4]
#'   as.matrix( c( alpha*x[1]-beta*x[2]*x[1] , -gamma*x[2]+delta*x[1]*x[2] ) )
#' }
#' ## Define the gradient of ode function against ode parameters 
#' ## df/dalpha,  df/dbeta, df/dgamma, df/ddelta where f is the differential equation.
#' LV_grlNODE= function(par,grad_ode,y_p,z_p) { 
#' alpha = par[1]; beta= par[2]; gamma = par[3]; delta = par[4]
#' dres= c(0)
#' dres[1] = sum( -2*( z_p[1,]-grad_ode[1,])*y_p[1,]*alpha ) 
#' dres[2] = sum( 2*( z_p[1,]-grad_ode[1,])*y_p[2,]*y_p[1,]*beta)
#' dres[3] = sum( 2*( z_p[2,]-grad_ode[2,])*gamma*y_p[2,] )
#' dres[4] = sum( -2*( z_p[2,]-grad_ode[2,])*y_p[2,]*y_p[1,]*delta)
#' dres
#' }
#'
#' ## create a ode class object
#' kkk0 = ode$new(2,fun=LV_fun,grfun=LV_grlNODE)
#' ## set the initial values for each state at time zero.
#' xinit = as.matrix(c(0.5,1))
#' ## set the time interval for the ode numerical solver.
#' tinterv = c(0,6)
#' ## solve the ode numerically using predefined ode parameters. alpha=1, beta=1, gamma=4, delta=1.
#' kkk0$solve_ode(c(1,1,4,1),xinit,tinterv) 
#'
#' ## Create another ode class object by using the simulation data from the ode numerical solver.
#' ## If users have experiment data, they can replace the simulation data with the experiment data.
#' ## set initial values for ode parameters.
#' init_par = rep(c(0.1),4)
#' init_yode = kkk0$y_ode
#' init_t = kkk0$t
#' kkk = ode$new(1,fun=LV_fun,grfun=LV_grlNODE,t=init_t,ode_par= init_par, y_ode=init_yode )
#'
#' @author Mu Niu, \email{ mu.niu@glasgow.ac.uk}

ode<-R6Class("ode",
  public = list(
    sample = NULL,
    t = NULL,
    y_ode = NULL,
    ode_par=NULL,
    ode_fun=NULL,
    gr_lNODE=NULL,

    initialize = function(sample = NULL,fun = NULL,grfun=NULL,t=NULL,ode_par = NULL,y_ode=NULL) {
      self$sample <- sample
      self$ode_par <- ode_par
      self$t<- t
      self$y_ode<- y_ode
      self$ode_fun = fun
      self$gr_lNODE = grfun
      self$greet()
    },

    greet = function() {
      cat(paste0("ode is sample ", self$sample, ".\n"))
    },
## ordinary differential equation in matrix form
## ode solver
    solve_ode = function(par_ode,xinit,tinterv){
        x0 = xinit#as.matrix(c(0.5,1))
		t0 <- tinterv[1]#0; 
		tf <- tinterv[2]#6
		solt <- ode23s(self$ode_fun, t0, tf, x0, par_ode=par_ode,hmax=(tf-t0) )
		pick = seq(1,length(solt$t),self$sample)
		self$t = solt$t[pick]
		rownames(solt$y) <- NULL
		self$y_ode= t(solt$y[pick,])
    },
    
    rmsfun = function(par_ode,state,M1,true_par){
        out <- ode(y =state,times =self$t, func = M1,parms=par_ode,method="ode23")
        funlos = sqrt( sum( ( t(out[,-1]) - self$y_ode)^2 ) / length(c(self$y_ode)) )
        parlos= sqrt( sum( (par_ode - true_par)^2 ) / length(true_par)  )
        relalos= sqrt( sum( ( (par_ode - true_par)/true_par )^2 ) / length(true_par)  )
        c(funlos,parlos,relalos)
    },
## gradient produced by ode
	gradient = function(y_p,par_ode){
	    ## y_p need to be 50*2  apply to each row
        ydif = apply(y_p,2,self$ode_fun,t=self$t,par_ode)
		ydif
	},
## mismatch between ode gradient and interpolant gradient
	##################################### griadient mathcing ##################
	lossNODE= function( par,y_p,z_p) {
		par_ode= exp(par)
        grad_ode = self$gradient(y_p,par_ode)
        dim(grad_ode) = dim(z_p)
		lm = max(dim(y_p)) 
		res=0
		start = 2
		res = sum( (z_p[,start:lm]-grad_ode[,start:lm])^2 )
		#res = sum( (z_p-grad_ode)^2 )
		res
	},

	grlNODE= function( par,y_p,z_p) { 
		par_ode= exp(par)   ## ode par
		lm = max(dim(y_p))
        grad_ode = self$gradient(y_p,par_ode)
        dim(grad_ode) = dim(z_p)
        start = 2
		dres=self$gr_lNODE( par_ode,grad_ode[,start:lm],y_p[,start:lm],z_p[,start:lm] )
		#dres=self$gr_lNODE( par_ode,grad_ode,y_p,z_p )
		dres
	},
    
    loss32NODE= function( par,y_p,z_p) {
		par_ode= exp(par)
        grad_ode = self$gradient(y_p,par_ode)
		lm = max(dim(y_p)) 
		res=0
		res = sum( (z_p[,]-grad_ode[,])^2 )
		res
	},

	grl32NODE= function( par,y_p,z_p) { 
		par_ode= exp(par)   ## ode par
		lm = max(dim(y_p))
        grad_ode = self$gradient(y_p,par_ode)
		#dres= c(0)
		dres=self$gr_lNODE( par_ode,grad_ode,y_p,z_p )
		dres
	},


    optim_par = function(par,y_p,z_p){
        grlNODE = if (is.null(self$gr_lNODE)) NULL else self$grlNODE
        op = optim(log(par),self$lossNODE,grlNODE,y_p,z_p,method="L-BFGS-B",control=list(trace=3, REPORT=1))
		self$ode_par = exp(op$par)
		exp(op$par)
	}

  )

)











