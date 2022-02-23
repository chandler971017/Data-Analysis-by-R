
#' The 'crossv' function
#'
#' This function is used to estimate the weighting parameter for ode regularisation using cross validation.
#'
#' Arguments of the 'crossv' function are list of weighting parameter for ode regularisation, 'ode' class objects, 'rkhs' class objects, noisy observation, type of regularisation scheme, option of warping and the gradient of warping function. It return the interpolation for each of the ode states. The ode parameters are estimated using gradient matching, and the results are stored in the 'ode' class as the ode_par attribute.
#' @param lam vector containing different choices of the weighting parameter of ode regularisation. 
#' @param kkk 'ode' class object containing all information about the odes.
#' @param bbb list of 'rkhs' class object containing the interpolation for all ode states.
#' @param crtype character containing the optimisation scheme type. User can choose 'i' or '3'. 'i' is for fast iterative scheme and '3' for optimising the ode parameters and interpolation coefficients simultaneously.
#' @param y_no matrix(of size n_s*n_o) containing noisy observations. The row(of length n_s) represent the ode states and the column(of length n_o) represents the time points.
#' @param woption character containing the indication of using warping. If the warping scheme is done before using the ode regularisation, user can choose 'w' otherwise just leave this option empty.
#' @param resmtest vector(of length n_o) containing the warped time points. This variable is only used if user want to combine warping and the ode regularisation.
#' @param dtilda vector(of length n_o) containing the gradient of warping function. This variable is only used if user want to combine warping and the ode regularisation.
#' @param fold scalar indicating the folds of cross validation.
#' @return return list containing :
#' \itemize{ 
#'	\item{} lam - scalar containing the optimised weighting parameter. 
#'	\item{} ress -vector containing the cross validation error for all choices of weighting parameter.}   
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
#' ############ gradient matching + ode regularisation
#' crtype='i'
#' lam=c(1e-4,1e-5)
#' lamil1 = crossv(lam,kkk,bbb,crtype,y_no)
#' lambdai1=lamil1[[1]]
#' res = third(lambdai1[1],kkk,bbb,crtype)
#' ## display ode parameters
#' res$oppar
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
#' ############# gradient matching + third step
#' crtype='i'
#' ## using cross validation to estimate the weighting parameters of the ode regularisation 
#' lam=c(1e-4,1e-5)
#' lamil1 = crossv(lam,kkk,bbb,crtype,y_no)
#' lambdai1=lamil1[[1]]
#'}
#' @author Mu Niu \email{mu.niu@glasgow.ac.uk}

crossv = function(lam,kkk,bbb,crtype,y_no,woption,resmtest,dtilda,fold)
{
nst = dim(kkk$y_ode)[1]
npar = length(kkk$ode_par)
lop=length(lam)
## randomly shuffle the data
set.seed(19573)
radsp = sample( length( kkk$t ) ) 
Xdata = kkk$t[ radsp ]    
Ydata = as.matrix( y_no[radsp,] )
## create 2 equally size folds
if(missing(fold)) 
 {
fold = 2
}

folds <- cut(seq(1,length(Xdata)),breaks=fold,labels=FALSE)

if(missing(woption)) 
 {
    woption = 'nw'
 } else {    woption
 Xdata = resmtest[,radsp]
 wgrad = dtilda[,radsp]
 }

ress = array(c(0))

for( ll in 1:lop) 
 {  
   crerror = 0
   for(fdi in 1:fold)
   {
	  testIndexes = which(folds==fdi,arr.ind=TRUE)
	  y_test = t(Ydata[testIndexes,])
      y_train = as.matrix( Ydata[-testIndexes,] )
      ntrain = length(kkk$t) - length(testIndexes)

      if(woption=='nw'){
	  testData = Xdata[testIndexes]
	  trainData = Xdata[-testIndexes]
	  } else {
	  testData = Xdata[,testIndexes]
	  trainData = Xdata[,-testIndexes]
	  trainGrad = wgrad[,-testIndexes]
	  testGrad = wgrad[,testIndexes]
	  }
      n_o = ntrain 
      
      kkk1 = ode$new(1,fun=kkk$ode_fun,grfun=kkk$gr_lNODE,t=trainData,y_ode=t(y_train) )

      if(woption == 'nw') {
      ktype='rbf'
      #kkk1 = ode$new(1,fun=BP_fun,grfun=BP_grlNODE,t=trainData,y_ode=t(y_train) )
      rkgres = rkg(kkk1,y_train,ktype)
      bbb1 = rkgres$bbb
      
      ode_m = kkk$clone()
	  iterp = rkg3$new()
	  iterp$odem=ode_m
		for( st in 1:nst)
		 {
		  rk1 = bbb1[[st]]$clone()
		  iterp$add(rk1)
		 } 
      } else
      {
     ### learn interpolates in warped time domain	
      bbbw = c()
	  intp = c()
	  grad = c()
        for(wi in 1: nst)
        {
	      ann1w = RBF$new(1)
	      bbb1w = rkhs$new(t(y_train)[wi,], trainData[wi,],rep(1,ntrain),1,ann1w)
	      bbb1w$skcross(5)

	      bbbw=c(bbbw,bbb1w)
	      intp = rbind(intp,bbbw[[wi]]$predict()$pred)
	      grad = rbind(grad,bbbw[[wi]]$predict()$grad*trainGrad[wi,])
	    }
       inipar= rep(0.1,npar)
       kkk1$optim_par( inipar, intp, grad )
       
       ode_m = kkk1$clone()
	   iterp = rkg3$new()
	   iterp$odem=ode_m
		for( st in 1:nst)
		 {
		  rk1 = bbbw[[st]]$clone()
		  iterp$add(rk1)
		 } 
      }
   
	  if(woption=='nw'){
		if(crtype=='i'){	
		   	   iterp$iterate(20,3,lam[ll]) 
		   }
		else if(crtype=='i3'){ 
		   	   iterp$iterate(20,3,lam[ll])
		       oppar=iterp$opfull( lam[ll])     
		    }
		 else if( crtype=='3'){
		       oppar=iterp$opfull( lam[ll])
		   }
	    } else if(woption=='w')
	    { print('wwww')
	     	if(crtype=='i'){	
		   	   iterp$witerate(20,3,trainGrad,lam[ll])
		   }
		 else if(crtype=='i3'){ 
		   	   iterp$witerate(20,3,trainGrad,lam[ll])
		       oppar=iterp$wopfull( lam[ll],trainGrad )    
		    }
		 else if( crtype=='3'){
		       oppar=iterp$wopfull( lam[ll],trainGrad )
		   }
	    }
	
		for(st in 1:nst)
		 {
		 if(woption=='nw'){
		    crerror = crerror+sum( (iterp$rk[[st]]$predictT(testData)$pred - y_test[st,])^2 )
		   } else if(woption=='w')
		   {
		   	crerror = crerror+sum( (iterp$rk[[st]]$predictT(testData[st,])$pred - y_test[st,])^2 )
		   }
		 }
     }

   ress[ll] = crerror
 }

 lam = lam[which(ress==min(ress))]
return(list("lam"=lam,"crloss"=ress))
}

