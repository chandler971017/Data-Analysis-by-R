
#' The 'Warp' class object
#'
#' This class provide the warping method which can be used to warp the original signal to sinusoidal like signal. 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return an  \code{\link{R6Class}} object which can be used for doing interpolation using reproducing kernel Hilbert space.
#' @format \code{\link{R6Class}} object.
#' @field y matrix(of size n_s*n_o) containing observation.
#' @field t vector(of length n_o) containing time points for observation.
#' @field b vector(of length n_o) containing coefficients of kernel or basis functions.
#' @field lambda scalar containing the weighting parameter for penalising the length of warped time span.
#' @field ker kernel class object containing sigmoid basis function.
#' @section Methods:
#' \describe{
#'   \item{\code{warpsin(len ,lop,p0,eps)}}{This method is used to warp the initial interpolation into a sinusoidal shape.}   
#'   \item{\code{slowWarp(lens,peod,eps)}}{This method is used to find the optimised initial hyper parameters for the sigmoid basis function for each ode states.}
#'   \item{\code{ warpLossLen(par,lam,p0,eps)}}{This method is used to implement the loss function for warping. It is called by the 'warpSin' function.} }
#' @export
#' @author Mu Niu, \email{mu.niu@glasgow.ac.uk}


Warp <- R6Class("Warp",
  public = list(
    y = NULL,
    t = NULL,
    b = NULL,
    lambda=NULL,
    ker= NULL,
    tw = NULL,

    initialize = function(y = NULL, t=NULL,b=NULL,lambda=NULL,ker=NULL) {
      self$y = y
      self$t = t
      self$b = b
      self$lambda = lambda
      self$ker = ker
      self$greet()
    },

    greet = function() {
      cat(paste("warp ",self$ker$greet(),".\n"))
    },

    showker = function () {
      cat(paste0("ker is", self$ker$greet(), ".\n"))
    },

    warpLoss = function( par,len,p0,eps ) {
     tor = as.matrix(self$t)
     n = max(dim(tor))
     lam=par[1]
     self$ker$k_par=exp(len)
     lambda_t = self$lambda
     y_r = self$y

     self$b = par[2:(n+1)]
     kw=(2*pi/ (p0 + eps*tanh(lam) )  )^2

      tw = matrix(c(0),ncol = n,nrow=1)
      for (i in 1:n)
      {
        tw[1,i] = self$ker$kern(tor[i,1],t(tor[,1]))%*%self$b^2 
      }
      t=t(tw)
      
      h=diff(t)
      z_t1 = matrix(c(0),ncol = n,nrow=1)
      z_t2 = matrix(c(0),ncol = n,nrow=2)
      y_t = matrix(c(0),ncol = n,nrow=2)

      ### calculate Euler 1st and 2nd order, if 1st order is enornouse  do not do spline
      for(i in 2:(n-1) )
      {
       z_t1[1,i] = (y_r[i+1]-y_r[i-1])/ (h[i]+h[i-1])
      }
      z_t1[1,1]=(y_r[2]-y_r[1])/h[1]
      z_t1[1,n]=(y_r[n]-y_r[n-1])/h[n-1]

      for(i in 2:(n-1) )
      {
       z_t2[1,i] = (z_t1[i+1]-z_t1[i-1])/ (h[i]+h[i-1])
      }
      z_t2[1,1]=(z_t1[2]-z_t1[1])/h[1]
      z_t2[1,n]=(z_t1[n]-z_t1[n-1])/h[n-1]

      y_t[1,]= y_r
      res= sum( (z_t2[1,] + kw*y_t[1,])^2 ) + lambda_t*( (tor[1,1]- t[1,1])^2+ (tor[n,1]- t[n,1])^2 )

      if( is.nan( sum(z_t1) ) )
      {
        print(len)
        return(res= 100000)
      }

      if( sum(abs(z_t1) )<1000 )
      {
        z_t2[1,] = tryCatch( { 
          predict(sm.spline(t, y_r,df=n-5),t, 2)
        }, warning = function(war)
                { 
                print(paste("MY_WARNING:  ",war))
                },
                 error = function(err) 
                {
                # error handler picks up where error was generated
                print(paste("warp_ERROR:  ",err,i))
                return(list(-10000,0) )
                #return(optim(par1,lossWarpTruelenFtanh,gr=NULL,pick,lambda_t,y_use[1,],len,p0,eps,method="L-BFGS-B") )
                },finally = { }  )
        if(z_t2[[1]][1]==-10000){ 
            return(res= 100000)
            }

        y_t[1,] = predict(sm.spline(t, y_r,df=n-5),t, 0)
        res= sum( (z_t2[1,] + kw*y_t[1,])^2 ) + lambda_t*( (tor[1,1]- t[1,1])^2+ (tor[n,1]- t[n,1])^2 )
      }

     res
	  },

    warpLossLen=function(par,lam,p0,eps ){
     tor = as.matrix(self$t)
     n = max(dim(tor))
     lam = lam#par[1]
     self$ker$k_par=exp(par)
     lambda_t = self$lambda
     y_r = self$y

     #bw1= self$b 
     kw=(2*pi/ (p0 + eps*tanh(lam) )  )^2
     tw = matrix(c(0),ncol = n,nrow=1)
      for (i in 1:n)
      {
        tw[1,i] = self$ker$kern(tor[i,1],t(tor[,1]))%*%self$b^2 
      }
      t=t(tw)
      h=diff(t)
      z_t1 = matrix(c(0),ncol = n,nrow=1)
      z_t2 = matrix(c(0),ncol = n,nrow=2)
      y_t = matrix(c(0),ncol = n,nrow=2)
        ### calculate Euler 1st and 2nd order, if 1st order is enornouse  do not do spline
      for(i in 2:(n-1) )
      {
       z_t1[1,i] = (y_r[i+1]-y_r[i-1])/ (h[i]+h[i-1])
      }
      z_t1[1,1]=(y_r[2]-y_r[1])/h[1]
      z_t1[1,n]=(y_r[n]-y_r[n-1])/h[n-1]

      for(i in 2:(n-1) )
      {
       z_t2[1,i] = (z_t1[i+1]-z_t1[i-1])/ (h[i]+h[i-1])
      }
      z_t2[1,1]=(z_t1[2]-z_t1[1])/h[1]
      z_t2[1,n]=(z_t1[n]-z_t1[n-1])/h[n-1]

      y_t[1,]= y_r
      res= sum( (z_t2[1,] + kw*y_t[1,])^2 ) + lambda_t*( (tor[1,1]- t[1,1])^2+ (tor[n,1]- t[n,1])^2 )

      if( is.nan( sum(z_t1) ) )
      {
        print(len)
        return(res= 100000)
      }

      if( sum(abs(z_t1) )<1000 ){
        z_t2[1,] = predict(sm.spline(t, y_r),t, 2)
        y_t[1,] = predict(sm.spline(t, y_r),t, 0)
        res= sum( (z_t2[1,] + kw*y_t[1,])^2 ) + lambda_t*( (tor[1,1]- t[1,1])^2+ (tor[n,1]- t[n,1])^2 )
      }

     res
    },


	  warpSin = function( len ,lop,p0,eps ) {
      kw1= 1
      #lop=3
      lout = length(self$t)
      bw = self$b

      for(i in 1:lop)
      {
        par<-c(kw1,bw)
        test <- tryCatch({ optim(par,self$warpLoss,gr=NULL,len,p0,eps,method="BFGS")
        }, warning = function(war)
                { 
                print(paste("MY_WARNING:  ",war))
                },
                 error = function(err) 
                {
                # error handler picks up where error was generated
                print(paste("warp_ERROR:  ",err,i))
                return(list(-10000,0) )
                #return(optim(par1,lossWarpTruelenFtanh,gr=NULL,pick,lambda_t,y_c,len,p0,eps,method="L-BFGS-B") )
                },finally = { }  )

        if(test[[1]][1]==-10000)
         { 
           wscore=100000
           break
          }else
          {
            self$b= test$par[2:(lout+1)]
            kw1 = test$par[1]
            par2=len
            bw= self$b
            #bbbl$warpLossLen(par2,kw1,p0,eps)
            test2<-tryCatch({ optim(par2,self$warpLossLen,gr=NULL,kw1,p0,eps,method="L-BFGS-B")
            }, warning = function(war)
                  { 
                  print(paste("MY_WARNING:  ",war))
                  },
                   error = function(err) 
                  {
                  # error handler picks up where error was generated
                  print(paste("warp_ERROR:  ",err,i))
                  return(list(-10000,0) )
                  #return(optim(par1,lossWarpTruelenFtanh,gr=NULL,pick,lambda_t,y_c,len,p0,eps,method="L-BFGS-B") )
                  },finally = { }  )
            if(test2[[1]][1]==-10000){ 
              wscore= 100000
              break }

            wscore=test2$value
            len=test2$par
          }
      }

      self$ker$k_par = exp(len)
      n = length(self$t)
      tw = matrix(c(0),ncol = n,nrow=1)
      for (i in 1:n)
      {
        tw[1,i] = self$ker$kern(self$t[i],t(self$t))%*%self$b^2 
      }
      self$tw = tw

      return( list( "rescore"=wscore,"len"=len) )  
    },


    slowWarp = function(lens,p0,eps) 
    {
      bw = rep(c(1),length(self$t))
      kw = 1
      loscore=c(0)
      #lens=#seq(str,end,0.1)  #seq(4.7,6.5,0.1)
      for(iii in 1:length(lens))  
      {
      par<-c(kw,bw)
      len=lens[iii]
      ptm <- proc.time()
      test<-optim(par,self$warpLoss,gr=NULL,len,p0,eps,method="BFGS")#optim(par,lossWarpTruelenFtanh,gr=NULL,pick,lambda_t,y_c,len,p0,eps,method="BFGS")
      proc.time() - ptm
      loscore[iii]= test$value
     }
      len=lens[which(loscore==min(loscore) )][1]
      return(list("len"=len,"loscore"=loscore))
    }

  )

)



