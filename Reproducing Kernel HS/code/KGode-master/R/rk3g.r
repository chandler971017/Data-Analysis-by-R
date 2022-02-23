#' The 'rkg3' class object
#'
#' This class provides advanced gradient matching method by using the ode as a regularizer. 
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return an  \code{\link{R6Class}} object which can be used for improving ode parameters estimation by using ode as a regularizer.
#' @format \code{\link{R6Class}} object.
#' @field rk the 'rkhs' class object containing the interpolation information for each state of the ode.
#' @field ode_m the 'ode' class object containing the information about the odes.
#' @section Methods:
#' \describe{
#'   \item{\code{iterate(iter,innerloop,lamb)}}{Iteratively updating ode parameters and interpolation regression coefficients.} 
#'   \item{\code{witerate(iter,innerloop,dtilda,lamb)}}{Iteratively updating ode parameters and the warped interpolation regression coefficients.}  
#'   \item{\code{full(par,lam)}}{Updating ode parameters and rkhs interpolation regression coefficients simultaneously. This method is slow but guarantee convergence.} }
#' @export
#' @author Mu Niu, \email{mu.niu@glasgow.ac.uk}

rkg3 <- R6Class("rkg3",
  public = list(
  	
    rk= list(),
    odem = NULL,
    initialize = function(rk=NULL,odem=NULL) {
      self$rk = rk
      self$odem = odem
      self$greet()
    },

    greet = function() {
      cat(paste("RK3G ",".\n"))
    },

    add = function(x) {
      self$rk <- c(self$rk, x)
      #invisible(self)
    },

    iterate= function( iter,innerloop,lamb ) 
    {  
      t = as.numeric( self$rk[[1]]$t)
      n = length(t)
      nd = length(self$rk) 

      #lamb = 1
     ## first step fix b in nonlinear part loss fun and work out the optimised b
      y_pkl = array(c(0),c(n,n,nd))
      z_tkl = array(c(0),c(n,n,nd))
      SKl = array(c(0),c(n,n,nd))
      fstl = array(c(0),c(1,n,nd))

      for(j in 1:nd)
      {
        for (i in 1:n) ## through data point
        {
         y_pkl[i,,j] = self$rk[[j]]$ker$kern(t(t),t[i])
         z_tkl[i,,j] = self$rk[[j]]$ker$dkdt(t[i],t(t))
        }

        SKl[,,j] = tryCatch({ solve(t(y_pkl[,,j])%*%y_pkl[,,j]+ lamb*t(z_tkl[,,j])%*%z_tkl[,,j])
          }, warning = function(war)
        { 
          print(paste("MY_WARNING:  ",war))
        },error = function(err) 
        {# error handler picks up where error was generated
          print(paste("MY_ERROR:  ",err))
          return( solve(t(y_pkl[,,j])%*%y_pkl[,,j]+ lamb*t(z_tkl[,,j])%*%z_tkl[,,j] +1e-10*diag(n) ) )
        },finally = {} )
       
       fstl[,,j] = c( scale(self$rk[[j]]$y, center=TRUE, scale=FALSE) ) %*% y_pkl[,,j]
      }

      lbl = array(c(0),c(nd,n))
      y_p =  array(c(0),c(nd,n))  
      z_p =  array(c(0),c(nd,n)) 

      for (it in 1:iter)
        {
          for(inlp in 1:innerloop)
          {  ## linear B function in old code
             for(j in 1:nd) {
                y_p[j,] =  self$rk[[j]]$predict()$pred 
              }
             dzl=self$odem$gradient(y_p,self$odem$ode_par)
             dim(dzl) = dim(y_p)  
             for(j in 1:nd) {
                lbl[j,]=( fstl[,,j] + lamb*dzl[j,]%*%(z_tkl[,,j]) ) %*% SKl[,,j]
                self$rk[[j]]$b = lbl[j,] 
              }
          }

          for(j in 1:nd) 
          {
            reslll = self$rk[[j]]$predict()
            y_p[j,] = reslll$pred
            z_p[j,] = reslll$grad
          }

          grlNODE = if (is.null(self$odem$gr_lNODE)) NULL else self$odem$grlNODE
          s32 <- tryCatch({ optim(log(self$odem$ode_par),self$odem$lossNODE,gr=grlNODE,y_p,z_p,method="L-BFGS-B")
            }, warning = function(war)
            { 
            print(paste("MY_WARNING:  ",war))
            },
             error = function(err) 
            {
            print(paste("MY_ERROR:  ",err))
            return( optim(log(self$odem$ode_par),self$odem$lossNODE,gr=grlNODE,y_p,z_p,method="BFGS") )
            },finally = { }  )
    
          par_rbf = exp(s32$par)
          self$odem$ode_par = par_rbf
          cat(par_rbf,"\n")
        }

     },

   witerate= function( iter,innerloop,dtilda,lamb) 
    {  

      n = dim(dtilda)[2]#length( self$odem$y_ode[1,])
      nd = length(self$rk) 
      #lamb = 1
     ## first step fix b in nonlinear part loss fun and work out the optimised b
      y_pkl = array(c(0),c(n,n,nd))
      z_tkl = array(c(0),c(n,n,nd))
      SKl = array(c(0),c(n,n,nd))
      fstl = array(c(0),c(1,n,nd))
      

      for(j in 1:nd)
      {
        t = as.numeric( self$rk[[j]]$t)

        for (i in 1:n) ## through data point
        {
         y_pkl[i,,j] = self$rk[[j]]$ker$kern(t(t),t[i])
         z_tkl[i,,j] = self$rk[[j]]$ker$dkdt(t[i],t(t))
        }

        oo = dtilda[j,]%*%ones(1,n)
        wkdot =  t( t(z_tkl[,,j])*(oo) )
        twkdot= ( t(z_tkl[,,j])*(oo) )

        SKl[,,j] = tryCatch({  solve( t(y_pkl[,,j])%*%y_pkl[,,j]+ lamb*twkdot%*%wkdot +1e-10*diag(n)  ) 
          }, warning = function(war)
        { 
          print(paste("MY_WARNING:  ",war))
        },error = function(err) 
        {# error handler picks up where error was generated
          print(paste("MY_ERROR:  ",err))
          return( solve(t(y_pkl[,,j])%*%y_pkl[,,j]+ lamb*twkdot%*%wkdot +1e-5*diag(n) ) )
        },finally = {} )
       
       fstl[,,j] = c( scale(self$rk[[j]]$y, center=TRUE, scale=FALSE) ) %*% y_pkl[,,j]
      }

      lbl = array(c(0),c(nd,n))
      y_p =  array(c(0),c(nd,n))  
      z_p =  array(c(0),c(nd,n)) 

      for (it in 1:iter)
        {
          for(inlp in 1:innerloop)
          {  ## linear B function in old code
             for(j in 1:nd) {
                y_p[j,] =  self$rk[[j]]$predict()$pred 
              }
             dzl=self$odem$gradient(y_p,self$odem$ode_par) 
             dim(dzl) = dim(y_p) 
             for(j in 1:nd) {
                #wkdot = (z_tkl[,,j])*(oo) 
                lbl[j,] = ( fstl[,,j] + lamb*dzl[j,]%*%z_tkl[,,j]*dtilda[j,] ) %*% SKl[,,j]
                self$rk[[j]]$b = lbl[j,] 
              }
          }

          for(j in 1:nd) 
          {
            reslll = self$rk[[j]]$predict()
            y_p[j,] = reslll$pred
            z_p[j,] = reslll$grad*dtilda[j,]
          }

          grlNODE = if (is.null(self$odem$gr_lNODE)) NULL else self$odem$grlNODE
          #s32 <- tryCatch({ optim(log(c(0.1,0.1,0.1,0.1)),self$odem$lossNODE,gr=self$odem$grlNODE,y_p,z_p,method="L-BFGS-B")
          s32 <- tryCatch({ optim(log(self$odem$ode_par),self$odem$lossNODE,gr=grlNODE,y_p,z_p,method="L-BFGS-B")
            }, warning = function(war)
            { 
            print(paste("MY_WARNING:  ",war))
            },
             error = function(err) 
            {
            print(paste("MY_ERROR:  ",err))
            return( optim(log(self$odem$ode_par),self$odem$lossNODE,gr=grlNODE,y_p,z_p,method="BFGS") )
            },finally = { }  )
    
          par_rbf = exp(s32$par)
          self$odem$ode_par = par_rbf
          cat(par_rbf,"\n")
        }

     },

    full= function( par,lam ) 
    { #lam=1
      t = as.numeric( self$rk[[1]]$t)
      n = length(t)
      nd = length(self$rk)  
      par_ode = exp( par[(nd*n+1):length(par)] )

      lbl = array(c(0),c(nd,n))
      y_t =  array(c(0),c(nd,n))  
      z_t =  array(c(0),c(nd,n)) 

      res= 0 
      for(j in 1:nd) {
      self$rk[[j]]$b = par[ ((j-1)*n+1):(j*n) ]
      reslll = self$rk[[j]]$predict()
      y_t[j,] = reslll$pred
      z_t[j,] = reslll$grad
      res =res+ sum( (self$rk[[j]]$y-y_t[j,])^2 )
      }
      dzl=self$odem$gradient(y_t,par_ode) 
      dim(dzl) = dim(y_t) 
      res = res  + lam*sum( (z_t- dzl)^2 )
      res
    },

    wfull= function( par,lam,dtilda ) 
    { #lam=1
      t = as.numeric( self$rk[[1]]$t)
      n = length(t)
      nd = length(self$rk)  
      par_ode = exp( par[(nd*n+1):length(par)] )

      lbl = array(c(0),c(nd,n))
      y_t =  array(c(0),c(nd,n))  
      z_t =  array(c(0),c(nd,n)) 

      res= 0 
      for(j in 1:nd) {
      self$rk[[j]]$b = par[ ((j-1)*n+1):(j*n) ]
      reslll = self$rk[[j]]$predict()
      y_t[j,] = reslll$pred
      z_t[j,] = reslll$grad*dtilda[j,]
      res =res+ sum( (self$rk[[j]]$y-y_t[j,])^2 )
      }
      dzl=self$odem$gradient(y_t,par_ode) 
      dim(dzl) = dim(y_t) 
      res = res  + lam*sum( (z_t- dzl)^2 )
      res
    },

    opfull= function(lam) 
    {  
      nd = length(self$rk)
      par=c()
      for(j in 1:nd) {  
      par=c( par,self$rk[[j]]$b)
      }

      #par=rep(1,length(par))
      par=c(par, log(self$odem$ode_par) )
      baseloss = self$full(par,lam)
      op = optim(par,self$full,,lam,method="BFGS",control=list(trace=3))

      np= length(self$odem$ode_par)
      plist = c( head(op$par, -np) , exp(tail(op$par,np)) )
      list( plist,op$value,baseloss)
    },

    wopfull= function(lam,dtilda) 
    {  
      nd = length(self$rk)
      par=c()
      for(j in 1:nd) {  
      par=c( par,self$rk[[j]]$b)
      }

      #par=rep(1,length(par))
      par=c(par, log(self$odem$ode_par) )
      baseloss = self$full(par,lam)
      op = optim(par,self$wfull,,lam,dtilda,method="BFGS")

      np= length(self$odem$ode_par)
      plist = c( head(op$par, -np) , exp(tail(op$par,np)) )
      list( plist,op$value,baseloss)
    },

    cross = function(lam,testX,testY)
    {
      nd = length(self$rk)
      par=c()
      for(j in 1:nd) {  
      par=c( par,self$rk[[j]]$b)
      }
      #par=rep(1,length(par))
      par=c(par, log(self$odem$ode_par) )
      baseloss = self$full(par,lam)
      op = optim(par,self$full,,lam,method="BFGS")


      y_t =  array(c(0),c(nd,ntest))  
      res = 0
      for(j in 1:nd) 
      {
       mean_y = apply(as.matrix(iterp$rk[[j]]$y),2,mean)
        for(jj in 1:ntest)
        { 
          y_t[j,jj] = iterp$rk[[j]]$ker$kern(t(trainData),testData[jj]) %*%iterp$rk[[j]]$b + mean_y
        }
       res =res+ sum( (y_test_me-y_t[j,])^2 )
      }
      
    },

    fullos= function( par ) 
    {  
      t = as.numeric( self$rk[[1]]$t)
      n = length(t)
      nd = length(self$rk)  
      par_ode = par[(nd*n+1):length(par)]
      
      lbl = array(c(0),c(nd,n))
      y_t =  array(c(0),c(nd,n))  
      z_t =  array(c(0),c(nd,n)) 
      
      res= 0 
      for(j in 1:nd) 
      {
       b = par[ ((j-1)*n+1):(j*n) ]
       mean_y = apply(as.matrix(self$rk[[j]]$y),2,mean)
        for(jj in 1:n)
        { 
          y_t[j,jj] = self$rk[[j]]$ker$kern(t(t),t[jj]) %*%b + mean_y
          z_t[j,jj] = self$rk[[j]]$ker$dkdt(t[jj],t(t)) %*%b
        }
       res =res+ sum( (self$rk[[j]]$y-y_t[j,])^2 )
      }
      dzl=self$odem$gradient(y_t,par_ode) 
      dim(dzl) = dim(y_t) 
      res = res  + sum( (z_t- dzl)^2 )
      res
    }

  )

)

##  par=c( rk1$b,rk2$b ,kkk$ode_par)



