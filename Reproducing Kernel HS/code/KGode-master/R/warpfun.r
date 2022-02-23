
#' The 'warpfun' function
#'
#' This function is used to produce the warping function and learning the interpolation in the warped time domain.
#'
#' Arguments of the 'warpfun' function are 'ode' class, 'rkhs' class, period of warped signal, uncertainty level of the period, initial values of the hyper parameters for sigmoid basis function, noisy observations and the time points that user want to warped.
#' It return the interpolation for each of the ode states. The ode parameters are estimated using gradient matching, and the results are stored in the 'ode' class as the ode_par attribute.
#' @param kkkrkg 'ode' class object.
#' @param bbb list of 'rkhs' class object.
#' @param peod vector(of length n_s) containing the period of warped signal. n_s is the length of the ode states.
#' @param eps vector(of length n_s) containing the uncertainty level of the period. n_s is the length of the ode states.
#' @param fixlens vector(of length n_s) containing the initial values of the hyper parameters of sigmoid basis function.
#' @param y_no matrix(of size n_s*n_o) containing noisy observations. The row(of length n_s) represent the ode states and the column(of length n_o) represents the time points.
#' @param testData vector(of size n_x) containing user defined time points which will be warped by the warping function.
#' @param witer scale containing the number of iterations for optimising the hyper parameters of warping.
#' @return return list containing :
#' \itemize{ 
#'	\item{} dtilda - vector(of length n_x) containing the gradients of warping function at user defined time points.  
#'	\item{} bbbw - list of 'rkhs' class object containing the interpolation in warped time domain.
#'	\item{} wtime - vector(of length n_x) containing the warped time points.
#'	\item{} wfun - list of 'rkhs' class object containing information about warping function. 
#'	\item{} wkkk - 'ode' class object containing the result of parameter estimation using the warped signal and gradient matching.}   
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
#' fixlens = 4  ## the value of fixlens can be estimated using fixlens=warpInitLen(peod,eps,rkgres)
#' kkkrkg = kkk$clone()
#' www = warpfun(kkkrkg,bbb,peod,eps,fixlens,y_no,kkkrkg$t,1)
#' www$wkkk$ode_par   
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
#'
#' kkkrkg = kkk$clone() ## make a copy of ode class objects
#' ##learn the warping function, warp data points and do gradient matching in the warped time domain.
#' www = warpfun(kkkrkg,bbb,peod,eps,fixlens,y_no,kkkrkg$t)
#'
#' dtilda= www$dtilda  ## gradient of warping function
#' bbbw = www$bbbw      ## interpolation in warped time domain
#' resmtest = www$wtime  ## warped time points
#' ##display the results of parameter estimation using gradient matching in the warped time domain.
#' www$wkkk$ode_par       
#'}
#' @author Mu Niu \email{mu.niu@glasgow.ac.uk}

warpfun =function(kkkrkg,bbb,peod,eps,fixlens,y_no,testData,witer)
{
   if(missing(witer)) {
        witer = 10
    } 
   kkk= kkkrkg$clone()
   if(missing(testData)) 
   {
   	flagtest = 2
    } else {flagtest = 1}

	bbbw = c()
	dtilda = c()
	intp = c()
	grad = c()
	resmtest =c()
	wfun=c()
	nst = length(bbb)
    
	for( st in 1:nst)
	{
		###### warp 1st state
		p0=peod[st]            ## a guess of the period of warped signal
		lambda_t= 50    ## the weight of fixing the end of interval 
		y_c = bbb[[st]]$predict()$pred ##y_use[1,]

		#### fix len
		fixlen = fixlens[st]#2.9  !!!!!!!!!!!!!!!!!!!!! give a constant for testing

		wsigm = Sigmoid$new(1)
		n_o = max( dim( kkk$y_ode) )
		bbbs = Warp$new( y_c,kkk$t,rep(1,n_o),lambda_t,wsigm)
		ppp = bbbs$warpSin( fixlen,witer,p0,eps )   ## 3.9 70db

		### learnign warping function using mlp
		t_me= bbbs$tw #- mean(bbbs$tw)
		ben = MLP$new(c(5,5)) 
		rkben = rkhs$new(t(t_me),kkk$t,rep(1,n_o),1,ben)
		rkben$mkcross(c(5,5))
		resm = rkben$predict()
		if(flagtest==1) 
		{
		 resmtest = rbind(resmtest, rkben$predictT(testData)$pred)
	    } 
		### learn interpolates in warped time domain
		ann1w = RBF$new(1)
		bbb1w = rkhs$new(t(y_no)[st,],resm$pred,rep(1,n_o),1,ann1w)
		bbb1w$skcross(5)

		dtilda =rbind(dtilda,resm$grad)
		bbbw=c(bbbw,bbb1w)
	    wfun=c(wfun,rkben)
	    
		intp = rbind(intp,bbbw[[st]]$predict()$pred)
		grad = rbind(grad,bbbw[[st]]$predict()$grad*resm$grad)
	}

    inipar= rep(0.1,length(kkk$ode_par))
    kkk$optim_par( inipar, intp, grad )
  return(list("dtilda"=dtilda,"bbbw"=bbbw,"wtime"=resmtest,"wfun"=wfun,"wkkk"=kkk))
}
