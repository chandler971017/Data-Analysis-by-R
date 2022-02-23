
#' The 'warpInitLen' function
#'
#' This function is used to find the optmised initial value of the hyper parameter for the sigmoid basis function which is used for warping.
#'
#' Arguments of the 'warpfun' function are 'ode' class, 'rkhs' class, period of warped signal, uncertainty level of the period, initial values of the hyper parameters for sigmoid basis function, noisy observations and the time points that user want to warped.
#' It return the interpolation for each of the ode states. The ode parameters are estimated using gradient matching, and the results are stored in the 'ode' class as the ode_par attribute.
#' @param peod vector(of length n_s) containing the period of warped signal. n_s is the length of the ode states.
#' @param eps vector(of length n_s) containing the uncertainty level of the period. n_s is the length of the ode states.
#' @param rkgres list containing interpolation and 'rkhs' class objects for all ode states.
#' @param lens vector(of length n_l) containing a list of hyper parameters of sigmoid basis function. n_l is the length of user defined hyper parameters of the sigmoid basis functino.
#' @return return list containing :
#' \itemize{ 
#'	\item{} wres- vector(of length n_s) contaning the optimised initial hyper parameters of sigmoid basis function for each ode states.}   
#' @export
#' @examples
#'\dontshow{
#'   ##examples for checks: executable in < 5 sec together with the examples above not shown to users
#'   ### define ode 
#'   toy_fun = function(t,x,par_ode){
#'        alpha=par_ode[1]
#'       as.matrix( c( -alpha*x[1]) )
#'    }
#'
#'    toy_grlNODE= function(par,grad_ode,y_p,z_p) { 
#'        alpha = par[1]
#'        dres= c(0)
#'        dres[1] = sum( 2*( z_p-grad_ode)*y_p*alpha ) #sum( -2*( z_p[1,2:lm]-dz1)*z1*alpha ) 
#'        dres
#'    }
#'
#'   t_no = c(0.1,1,2,3,4,8)
#'   n_o = length(t_no)   
#'   y_no =  matrix( c(exp(-t_no)),ncol=1  )
#'   ######################## create and initialise ode object #########################################
#'  init_par = rep(c(0.1))
#'  init_yode = t(y_no)
#'  init_t = t_no
#'
#'  kkk = ode$new(1,fun=toy_fun,grfun=toy_grlNODE,t=init_t,ode_par= init_par, y_ode=init_yode )
#'
#'  ##### standard gradient matching
#'  ktype='rbf'
#'  rkgres = rkg(kkk,(y_no),ktype)
#'  bbb = rkgres$bbb
#'
#'  ## warping method 
#'  peod = 20
#'  eps= 1
#'
#'  fixlens=warpInitLen(peod,eps,rkgres)
#'}
#'\dontrun{
#' require(mvtnorm)
#' noise = 0.1  
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
#' ## Add noise to the numerical solution of the ode model and use it as the noisy observation.
#' n_o = max( dim( kkk0$y_ode) )
#' t_no = kkk0$t
#' y_no =  t(kkk0$y_ode) + rmvnorm(n_o,c(0,0),noise*diag(2))
#'
#' ## create a ode class object by using the simulation data we created from the Ode numerical solver.
#' ## If users have experiment data, they can replace the simulation data with the experiment data.
#' ## set initial value of Ode parameters.
#' init_par = rep(c(0.1),4)
#' init_yode = t(y_no)
#' init_t = t_no
#' kkk = ode$new(1,fun=LV_fun,grfun=LV_grlNODE,t=init_t,ode_par= init_par, y_ode=init_yode )
#'
#' ## The following examples with CPU or elapsed time > 10s
#'
#' ## Use function 'rkg' to estimate the Ode parameters.
#' ktype ='rbf'
#' rkgres = rkg(kkk,y_no,ktype)
#' bbb = rkgres$bbb
#' 
#' ###### warp all ode states
#' peod = c(6,5.3) ## the guessing period
#' eps= 1          ## the uncertainty level of period
#' 
#' ###### learn the initial value of the hyper parameters of the warping basis function
#' fixlens=warpInitLen(peod,eps,rkgres)
#'}
#' @author Mu Niu \email{mu.niu@glasgow.ac.uk}

warpInitLen= function(peod,eps,rkgres,lens)
{
 if(missing(lens)) {
        lens = c(4,5)
    } 
###### warp 1st state
lambda_t= 50    ## the weight of fixing the end of interval 
n_o = length(rkgres$intp[1,])
wres=c()
 for(i in 1:length(rkgres$bbb))
 {
  y_c = rkgres$intp[i,]
  wsigm = Sigmoid$new(1)
  bbb = Warp$new( y_c,rkgres$bbb[[i]]$t,rep(1,n_o),lambda_t,wsigm)
  wresi<- bbb$slowWarp(lens,peod[i],eps)
  wres<-c(wres,wresi$len)
 }
return(wres)  
}
