#' The 'rkhs' class object
#'
#' This class provide the interpolation methods using reproducing kernel Hilbert space. 
#' @docType class
#' @importFrom R6 R6Class
#' @import pracma
#' @import mvtnorm
#' @import pspline
#' @export
#' @keywords data
#' @return an  \code{\link{R6Class}} object which can be used for doing interpolation using reproducing kernel Hilbert space.
#' @format \code{\link{R6Class}} object.
#' @field y matrix(of size n_s*n_o) containing observation.
#' @field t vector(of length n_o) containing time points for observation.
#' @field b vector(of length n_o) containing coefficients of kernel or basis functions.
#' @field lambda scalar containing the weighting parameter for L2 norm of the reproducing kernel Hilbert space.
#' @field ker kernel class object containing kernel.
#' @section Methods:
#' \describe{
#'   \item{\code{predict()}}{This method is used to make prediction on given time points} 	
#'   \item{\code{skcross()}}{This method is used to do cross-validation to estimate the weighting parameter lambda of L^2 norm.} }
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
#'  ##### using kernel ridge regression to learn the interpolation of the noisy observation
#'
#'  initlen = 1
#'  aker = RBF$new(initlen)
#'  bbb = rkhs$new(t(y_no)[1,],t_no,rep(1,n_o),1,aker)
#' ## optimise lambda by cross-validation
#' ## initial value of lambda
#'  initlam = 2
#'  bbb$skcross( initlam ) 
#'
#'}
#' \dontrun{
#' require(mvtnorm)
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
#' ## Add noise to the numerical solution of the ode model and use it as the noisy observation.
#' n_o = max( dim( kkk0$y_ode) )
#' t_no = kkk0$t
#' y_no =  t(kkk0$y_ode) + rmvnorm(n_o,c(0,0),noise*diag(2))
#'
#' ## Create a ode class object by using the simulation data we created from the ode numerical solver.
#' ## If users have experiment data, they can replace the simulation data with the experiment data.
#' ## Set initial value of ode parameters.
#' init_par = rep(c(0.1),4)
#' init_yode = t(y_no)
#' init_t = t_no
#' kkk = ode$new(1,fun=LV_fun,grfun=LV_grlNODE,t=init_t,ode_par= init_par, y_ode=init_yode )
#'
#' ## The following examples with CPU or elapsed time > 5s
#' ####### rkhs interpolation for the 1st state of ode using 'rbf' kernel
#' ### set initial value of length scale of rbf kernel
#' initlen = 1
#' aker = RBF$new(initlen)
#' bbb = rkhs$new(t(y_no)[1,],t_no,rep(1,n_o),1,aker)
#' ## optimise lambda by cross-validation
#' ## initial value of lambda
#' initlam = 2
#' bbb$skcross( initlam ) 
#'
#' ## make prediction using the 'predict()' method of 'rkhs' class and plot against the time.
#' plot(t_no,bbb$predict()$pred)
#' }
#' @author Mu Niu, \email{mu.niu@glasgow.ac.uk}

rkhs <- R6Class("rkhs",
  public = list(
  	
    y = NULL,
    t = NULL,
    b = NULL,
    lambda=NULL,
    ker= NULL,

    initialize = function(y = NULL, t=NULL,b=NULL,lambda=NULL,ker=NULL) {
      self$y = y
      self$t = t
      self$b = b
      self$lambda = lambda
      self$ker = ker
      self$greet()
    },

    greet = function() {
      cat(paste("RKHS ",self$ker$greet(),".\n"))
    },

    showker = function () {
      cat(paste0("ker is", self$ker$greet(), ".\n"))
    },
    
    predict = function() {
	    ###########  calculate optimised lengthscale #####################################################
	    y = scale(as.matrix(self$y), center=TRUE, scale=FALSE)
		mean_y= apply(as.matrix(self$y),2,mean)
	 	t = as.numeric(self$t)
	 	n = length(self$t)
	# ############################## make prediction for derivative ######################################################
	    y_t = array(c(0),n)#matrix(c(0),ncol = n,nrow=dimstate)
	    z_t = array(c(0),n)#matrix(c(0),ncol = n,nrow=dimstate)
	    for (i in 1:n)
	    {
	      y_t[i] = self$ker$kern(t(t),t[i])%*%self$b +mean_y
	      z_t[i] = self$ker$dkdt( t[i],t(t))%*%self$b #*usgrad[il,i]
	    }
	return(list("pred"=y_t,"grad"=z_t))#,"y_p"=y_p,"z_p"=z_p,"zt"=z_t,"zt2"=z_t2 ) )
    },

    predictT = function(testT) {
	    ###########  calculate optimised lengthscale #####################################################
	    y = scale(as.matrix(self$y), center=TRUE, scale=FALSE)
		mean_y= apply(as.matrix(self$y),2,mean)
	 	t = as.numeric(self$t)
	 	testn = length(testT)
	# ############################## make prediction for derivative ######################################################
	    y_t = array(c(0),testn)
	    z_t = array(c(0),testn)
	    for (i in 1:testn)
	    {
	      y_t[i] = self$ker$kern(t(t),testT[i])%*%self$b +mean_y
	      z_t[i] = self$ker$dkdt( testT[i],t(t))%*%self$b
	    }
	return(list("pred"=y_t,"grad"=z_t))
    },

	lossRK = function(par,tl1,y_d,jitter) {
	  self$ker$k_par = exp(par)
	  #self$ker$k_par = (par)
	  res =0
	  tl1=as.matrix(tl1)
	  n=max(dim(tl1))

	  K=matrix(c(0),nco=n, nrow=n)
	  for (i in 1:n)
	   {
	      for (j in 1:n)
	        {
	          K[i,j]=self$ker$kern(tl1[i],tl1[j])
	        }
	   }
	# calculate the coeffiecient 'b' of basis function
	  ik = solve(K+jitter[1]*diag(n))
	  b=ik%*%y_d

	  for(i in 1:n)
	  {
	  res=res+ (y_d[i]- self$ker$kern(t(tl1),tl1[i])%*%b)^2
	  }
      if(length(par)==1){
	  res = res + jitter* t(b)%*%K%*%b
	  } else
	  {
	  res = res + jitter[1]* t(b)%*%K%*%b + jitter[2]*self$ker$k_par[2]	
	  }
	  
	  res
	},

	grlossRK = function(par,tl1,y_d,jitter) {
	  self$ker$k_par = exp(par)
	  #self$ker$k_par = (par)
	  tl1=as.matrix(tl1)
	  n=max(dim(tl1))
	  np = length(par)
	  dres = array(c(0),c(1,np))
	  K=matrix(c(0),nco=n, nrow=n)
      dK= array(c(0),c(n,n,np))
      db=array(c(0),c(n,np))

	  for (i in 1:n)
	   {
	      for (j in 1:n)
	        {
	          #ts = tl1[i]-tl1[j]
	          dK[i,j,1:np]=self$ker$dkd_kpar(tl1[i],tl1[j])
	          K[i,j]=self$ker$kern(tl1[i],tl1[j])
	        }
	   }
	  ik = solve(K+jitter[1]*diag(n))
	  b=ik%*%y_d

	  for(k in 1:np){
	  db[,k]= -ik%*%dK[,,k]%*%ik%*%y_d
      }
      for(k in 1:np) 
       {
	     for(i in 1:n)
	     {
	       dres[,k]=dres[,k]+  2* (y_d[i]- self$ker$kern(t(tl1),tl1[i])%*%b) *(-1)*( dK[,i,k]%*%b + K[,i]%*%db[,k] )
	     }
	     if(k==1){
	 	 dres[,k] = dres[,k] + ( t(db[,k])%*%K%*%b + t(b)%*%dK[,,k]%*%b + t(b)%*%K%*%db[,k] ) * jitter[k]
	 	 } else
	 	 {
	 	 dres[,k] = dres[,k] + ( t(db[,k])%*%K%*%b + t(b)%*%dK[,,k]%*%b + t(b)%*%K%*%db[,k] ) * jitter[k-1] + jitter[k]*self$ker$k_par[k]
	 	 }
	   }
	dres
	},

    numgrad = function(par,tl1,y_d,jitter)
	{
	require(numDeriv)
	return( grad(self$loss11,par,,,,tl1,y_d,jitter) )
	},

	skcross = function( init,bounded ) 
	 {
	 	innerlp = 4
	    tbd = array(c(1), c(length(self$ker$k_par)) )
	 	if( missing(bounded) )
	    {  
         bounded= c(-Inf,Inf)
         lbound = bounded[1]*tbd
         ubound = bounded[2]*tbd
	    } else {
	    lbound = log(bounded[1]*tbd)
	    ubound = log(bounded[2]*tbd)
	    }

		y = scale(as.matrix(self$y), center=TRUE, scale=FALSE)
		mean_y= apply(as.matrix(self$y),2,mean)

		t_y = matrix(c(self$t),ncol=dim(y)[1])

		n_y=max(dim(t_y))
		np = length(self$ker$k_par)

		jitter=matrix(c(0),ncol=np)

		fold = 3
		oneba = seq(1,n_y,3)
		twoba = seq(2,n_y,3)
		thrba = c(1:n_y)[c(-oneba,-twoba)]
		fd<-list(oneba,twoba,thrba)

		for(index in 1:np)
		  {
		    crlambda=c(5,1,0.1,0.01,1e-03,1e-04)
		    #crlambda=c(100,10,5,1,0.1,0.01,1e-03,1e-04,1e-05,1e-06,1e-07)
		    for(iii in 1:innerlp)
		    {
		    n_l = length(crlambda)
		    cres = matrix(c(0),ncol=n_l,nrow=1)
		    ibreak=0
		     for (k in 1:n_l)
		      {
		          jitter[index] =  crlambda[k] 
		          jitter = as.numeric(jitter)
		            for (c in 1: fold)
		            {
		              t = t_y[1,-fd[[c]]]
		              y_d= y[-fd[[c]],1]
		              n = length(t)  
		              s1 = tryCatch({ optim( log(init),self$lossRK,self$grlossRK,t,y_d,jitter,method="L-BFGS-B",lower=lbound,upper=ubound )  
		       #optim( log(self$ker$k_par),self$lossRK,self$grlossRK,t,y_d,jitter,method="L-BFGS-B",lower=lbound,upper=ubound )  
		        }, warning = function(war)
		         { 
		         print(paste("MY_WARNING:  ",war))
		         },
		          error = function(err) 
		         {
		         # error handler picks up where error was generated
		         print(paste("MY_ERROR:  ",err))
		         return( optim( log(init),self$lossRK,self$grlossRK,t,y_d,jitter,method="BFGS") )
		         # optim(log(self$ker$k_par),self$lossRK,self$grlossRK,t,y_d,jitter,method="BFGS") )
		         },finally = { }  )

		           if(s1[[1]][1]==-100){
		           ibreak=100
		           break
		           }
		        
		        self$ker$k_par = exp(s1$par)
		                
		                cK=matrix(c(0),ncol=n, nrow=n)
		                  #  make new b
		                      for (i in 1:n)
		                         {
		                            for (j in 1:n)
		                              {
		                               cK[i,j]=self$ker$kern(t[i],t[j])
		                              }
		                         }
		                  # calculate the coeffiecient 'b' of basis function
		                    cik = solve(cK+ jitter[1]*diag(n))
		                    cb=cik%*%y_d

		                      for(i in 1:length(fd[[c]]))
		                      {
		                  cres[k] = cres[k]+ (y[fd[[c]],1][i]- self$ker$kern(t_y[1,fd[[c]]][i],t(t))%*%cb)^2
		                      }
		            }
		         }
		          ank = crlambda[which(cres==min(cres))]
		          pank=ank/10
		          crlambda = c( ank-2*pank,ank-pank,ank+pank,ank+2*pank)
		    }
		    jitter[index] = ank
		    }

          self$lambda = jitter
          s1 = optim( log(init),self$lossRK,self$grlossRK,as.numeric(t_y),as.numeric(y),jitter,method="L-BFGS-B",lower=lbound,upper=ubound )
		  
		  self$ker$k_par = exp(s1$par)
		  K1=matrix(c(0),ncol=n_y, nrow=n_y)
	       for (i in 1:n_y)
	       {
	        for (j in 1:n_y)
	          {
	            K1[i,j]=self$ker$kern(t_y[i],t_y[j])
	          }
	       }
	    # calculate the coeffiecient 'b' of basis function
	      ik1 = solve(K1 + self$lambda[1]*diag(n_y) )
	      self$b= as.numeric( ik1%*%as.numeric(y) )

		return(jitter)
	  },

    mkcross = function(init) 
	 	{
	 		innerlp = 2
	 		y = scale(as.matrix(self$y), center=TRUE, scale=FALSE)
	 		#mean_y= apply(as.matrix(self$y),2,mean)
	 		t_y = matrix(c(self$t),ncol=dim(y)[1])
	 		n_y=max(dim(t_y))
	 		np = length(self$ker$k_par)
	 		jitter=matrix(c(0),ncol=np)

	 		fold = 3
	 		oneba = seq(1,n_y,3)
	 		twoba = seq(2,n_y,3)
	 		thrba = c(1:n_y)[c(-oneba,-twoba)]
	 		fd<-list(oneba,twoba,thrba)

	 		crl1=c(100,10,5,1,0.3,0.1,0.01,1e-03,1e-04,1e-05,1e-06)
            crl2=c(100,10,5,1,0.1,0.01,1e-03,1e-04,1e-05,1e-06,1e-07)
            crll<-list(crl1,crl2)

	 		for(index in 1:np)
	 		  {
	 		  	crlambda=crll[[index]]
	 		    #crlambda=c(100,10,5,1,0.1,0.01,1e-03,1e-04,1e-05,1e-06,1e-07)
	 		    for(iii in 1:innerlp)
	 		    {
	 		    n_l = length(crlambda)
	 		    cres = matrix(c(0),ncol=n_l,nrow=1)
	 		    ibreak=0
	 		     for (k in 1:n_l)
	 		      {
	 		          jitter[index] =  crlambda[k] 
	 		          jitter = as.numeric(jitter)
	 		            for (c in 1: fold)
	 		            {
	 		                  t = t_y[1,-fd[[c]]]
	 		                  y_d= y[-fd[[c]],1]
	 		                  n = length(t)
	 		      			  s1 = optim( log(init),self$lossRK,self$grlossRK,t,y_d,jitter,method="L-BFGS-B",lower=log(c(0.001,0.001)),upper=log(c(1000,1000)) )   
	 		                  self$ker$k_par = exp(s1$par)

	 		                cK=matrix(c(0),ncol=n, nrow=n)
	 		                      for (i in 1:n)
	 		                         {
	 		                            for (j in 1:n)
	 		                              {
	 		                               cK[i,j]=self$ker$kern(t[i],t[j])
	 		                              }
	 		                         }
	 		                  # calculate the coeffiecient 'b' of basis function
	 		                    cik = solve(cK+ jitter[1]*diag(n))
	 		                    cb=cik%*%y_d
	 		                      for(i in 1:length(fd[[c]]))
	 		                      {
	 		                  cres[k] = cres[k]+ (y[fd[[c]],1][i]- self$ker$kern(t_y[1,fd[[c]]][i],t(t))%*%cb)^2
	 		                      }
	 		            }
	 		         }
	 		          ank = crlambda[which(cres==min(cres))]
	 		          pank=ank/10
	 		          crlambda = c( ank-2*pank,ank-pank,ank+pank,ank+2*pank)
	 		    }
	 		    jitter[index] = ank
	 		}

	        self$lambda = jitter
	 		s1 = optim( log(init),self$lossRK,self$grlossRK,self$t,self$y,jitter,method="L-BFGS-B",lower=log(c(0.001,0.001)),upper=log(c(1000,1000)) )   
	 		self$ker$k_par = exp(s1$par)
			  K1=matrix(c(0),ncol=n_y, nrow=n_y)
		       for (i in 1:n_y)
		       {
		        for (j in 1:n_y)
		          {
		            K1[i,j]=self$ker$kern(self$t[i],self$t[j])
		          }
		       }
	    # calculate the coeffiecient 'b' of basis function
	      ik1 = solve(K1 + self$lambda[1]*diag(n_y) )
	      self$b= as.numeric( ik1%*%as.numeric(y) )

	 	  return(jitter)
	 },


	 loss11 = function(par,tl1,y_d,jitter) {
	  self$ker$k_par = exp(par)
	  #self$ker$k_par = (par)
	  res =0
	  tl1=as.matrix(tl1)
	  n=max(dim(tl1))

	  K=matrix(c(0),nco=n, nrow=n)
	  for (i in 1:n)
	   {
	      for (j in 1:n)
	        {
	          K[i,j]=self$ker$kern(tl1[i],tl1[j])
	        }
	   }
	# calculate the coeffiecient 'b' of basis function
	  ik = solve(K+jitter[1]*diag(n))
	  b=ik%*%y_d

	  for(i in 1:n)
	  {
	  res=res+ (y_d[i]- self$ker$kern(t(tl1),tl1[i])%*%b)^2
	  }
      if(length(par)==1){
	  res = res + jitter* t(b)%*%K%*%b
	  } else
	  {
	  res = res + jitter[1]* t(b)%*%K%*%b + jitter[2]*self$ker$k_par[2]	
	  }
	  
	  res
	},

	grloss11 = function(par,tl1,y_d,jitter) {
	  self$ker$k_par = exp(par)
	  #self$ker$k_par = (par)
	  tl1=as.matrix(tl1)
	  n=max(dim(tl1))
	  np = length(par)
	  dres = array(c(0),c(1,np))
	  K=matrix(c(0),nco=n, nrow=n)
      dK= array(c(0),c(n,n,np))
      db=array(c(0),c(n,np))

	  for (i in 1:n)
	   {
	      for (j in 1:n)
	        {
	          #ts = tl1[i]-tl1[j]
	          dK[i,j,1:np]=self$ker$dkd_kpar(tl1[i],tl1[j])
	          K[i,j]=self$ker$kern(tl1[i],tl1[j])
	        }
	   }
	  ik = solve(K+jitter[1]*diag(n))
	  b=ik%*%y_d

	  for(k in 1:np){
	  db[,k]= -ik%*%dK[,,k]%*%ik%*%y_d
      }
      for(k in 1:np) 
       {
	     for(i in 1:n)
	     {
	       dres[,k]=dres[,k]+  2* (y_d[i]- self$ker$kern(t(tl1),tl1[i])%*%b) *(-1)*( dK[,i,k]%*%b + K[,i]%*%db[,k] )
	     }
	     if(k==1){
	 	 dres[,k] = dres[,k] + ( t(db[,k])%*%K%*%b + t(b)%*%dK[,,k]%*%b + t(b)%*%K%*%db[,k] ) * jitter[k]
	 	 } else
	 	 {
	 	 dres[,k] = dres[,k] + ( t(db[,k])%*%K%*%b + t(b)%*%dK[,,k]%*%b + t(b)%*%K%*%db[,k] ) * jitter[k-1] + jitter[k]*self$ker$k_par[k]
	 	 }
	   }
	dres
	}  

  )

)


