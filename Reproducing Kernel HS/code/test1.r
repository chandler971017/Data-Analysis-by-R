<<<<<<< HEAD
if(!require(mvtnorm))install.packages("mvtnorm")
if(!require(KGode))install.packages("KGode")
library(mvtnorm)
library(KGode)
library(tidyverse)



noise = 0.2  ## set the variance of noise
SEED = 19537
set.seed(SEED)

## Define ode function, we use lotka-volterra model in this example.
## we have two ode states x[1], x[2] and four ode parameters alpha, beta, gamma and delta. LV_fun = function(t,x,par_ode){
LV_fun = function(t,x,par_ode){
  alpha=par_ode[1]
  beta=par_ode[2]
  gamma=par_ode[3]
  delta=par_ode[4]
  as.matrix( c( alpha*x[1]-beta*x[2]*x[1] , -gamma*x[2]+delta*x[1]*x[2] ) )
}


## create a ode class object
solveOde = ode$new(sample=2,fun=LV_fun)            
## set the initial values for each state at time zero.
xinit = as.matrix(c(0.5,1))
## set the time interval for the ode numerical solver.
tinterv = c(0,6)
## solve the ode numerically using predefined ode parameters. alpha=1, beta=1, gamma=4, delta=1. kkk0$solve_ode(c(1,1,4,1),xinit,tinterv)
solveOde$solve_ode(par_ode=c(1,1,4,1),xinit,tinterv)
## Add noise to the numerical solution of the ode model and use it as the noisy observation. n_o = max( dim( kkk0$y_ode) )
y_true= t(solveOde$y_ode) #解出的值，由于是两个state，是一个2列的矩阵
n_o = max(dim( y_true))

t_no = solveOde$t        #时间，为什么是这种不均匀的，我也不知道 solve_ode 是怎么操作的
y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))




# 
# ## define a rbf kernel object --RBF radial bass function kernel 
# initlen =1 #length scale
# ker_rbf = RBF$new(initlen)
# 
# ## define a rkhs regression object
# rkhs_obj = rkhs$new(y_no,t=t_no,rep(1,n_o),1,ker_rbf)
# 
# ## optimise by cross-validation 
# initlam = 2   ## initial value of lambda    
# rkhs_obj$skcross(initlam) 
# 
# y_predict = rkhs_obj$predict()$pred  ## prediction at t_no



odem= ode$new(fun=LV_fun,grfun=NULL,
              t=t_no,
              ode_par = rep(1,4),
              y_ode=t(y_no))
ode=rkg(odem,y_no,ktype = "rbf")
odem$ode_par

cbind(t_no,y_true,y_no,t(ode$intp))%>%
  as.data.frame()%>%
  rename(t=t_no,x1=V2,x2=V3,x1_obs=V4,x2_obs=V5,x1_inp=V6,x2_inp=V7)%>%
  gather(key="ID",value="value",-t)->data
data$type=factor(rep(c("true_value","noisy_obs","interpolants"),each=nrow(data)/3))

ggplot(data,aes(x=t,y=value,color=ID,shape=ID,size=2))+
  geom_point()
diagnostic(ode,type="rkg",index=1,qq_plot=T)

    


# points(t_no,y_true[,1],col='red',pch=16)
# points(t_no,y_no[,1],col='blue',pch=16)
#How to get estimated ode parameters.
#How to extract basis function with optimised length scales
predictT = function(testT) {
  ###########  calculate optimised lengthscale #####################################################
  y = scale(as.matrix(self$y), center=TRUE, scale=FALSE)
  mean_y= apply(as.matrix(self$y),2,mean)
  t = as.numeric(self$t)
  testn = length(testT)
  ############################### make prediction for derivative ######################################################
  y_t = array(c(0),testn)
  z_t = array(c(0),testn)
  for (i in 1:testn)
  {
    y_t[i] = self$ker$kern(t(t),testT[i])%*%self$b +mean_y
    z_t[i] = self$ker$dkdt( testT[i],t(t))%*%self$b
  }
  return(list("pred"=y_t,"grad"=z_t))
}                                      #predictT

#gradient matching的参数返回去哪了
#调用 basis 列


=======
if(!require(mvtnorm))install.packages("mvtnorm")
if(!require(KGode))install.packages("KGode")
library(mvtnorm)
library(KGode)
library(tidyverse)



noise = 0.2  ## set the variance of noise
SEED = 19537
set.seed(SEED)

## Define ode function, we use lotka-volterra model in this example.
## we have two ode states x[1], x[2] and four ode parameters alpha, beta, gamma and delta. LV_fun = function(t,x,par_ode){
LV_fun = function(t,x,par_ode){
  alpha=par_ode[1]
  beta=par_ode[2]
  gamma=par_ode[3]
  delta=par_ode[4]
  as.matrix( c( alpha*x[1]-beta*x[2]*x[1] , -gamma*x[2]+delta*x[1]*x[2] ) )
}


## create a ode class object
solveOde = ode$new(sample=2,fun=LV_fun)            
## set the initial values for each state at time zero.
xinit = as.matrix(c(0.5,1))
## set the time interval for the ode numerical solver.
tinterv = c(0,6)
## solve the ode numerically using predefined ode parameters. alpha=1, beta=1, gamma=4, delta=1. kkk0$solve_ode(c(1,1,4,1),xinit,tinterv)
solveOde$solve_ode(par_ode=c(1,1,4,1),xinit,tinterv)
## Add noise to the numerical solution of the ode model and use it as the noisy observation. n_o = max( dim( kkk0$y_ode) )
y_true= t(solveOde$y_ode) #解出的值，由于是两个state，是一个2列的矩阵
n_o = max(dim( y_true))

t_no = solveOde$t        #时间，为什么是这种不均匀的，我也不知道 solve_ode 是怎么操作的
y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))




# 
# ## define a rbf kernel object --RBF radial bass function kernel 
# initlen =1 #length scale
# ker_rbf = RBF$new(initlen)
# 
# ## define a rkhs regression object
# rkhs_obj = rkhs$new(y_no,t=t_no,rep(1,n_o),1,ker_rbf)
# 
# ## optimise by cross-validation 
# initlam = 2   ## initial value of lambda    
# rkhs_obj$skcross(initlam) 
# 
# y_predict = rkhs_obj$predict()$pred  ## prediction at t_no



odem= ode$new(fun=LV_fun,grfun=NULL,
              t=t_no,
              ode_par = rep(1,4),
              y_ode=t(y_no))
ode=rkg(odem,y_no,ktype = "rbf")
odem$ode_par

cbind(t_no,y_true,y_no,t(ode$intp))%>%
  as.data.frame()%>%
  rename(t=t_no,x1=V2,x2=V3,x1_obs=V4,x2_obs=V5,x1_inp=V6,x2_inp=V7)%>%
  gather(key="ID",value="value",-t)->data
data$type=factor(rep(c("true_value","noisy_obs","interpolants"),each=nrow(data)/3))

ggplot(data,aes(x=t,y=value,color=ID,shape=ID,size=2))+
  geom_point()
diagnostic(ode,type="rkg",index=1,qq_plot=T)

    


# points(t_no,y_true[,1],col='red',pch=16)
# points(t_no,y_no[,1],col='blue',pch=16)
#How to get estimated ode parameters.
#How to extract basis function with optimised length scales
predictT = function(testT) {
  ###########  calculate optimised lengthscale #####################################################
  y = scale(as.matrix(self$y), center=TRUE, scale=FALSE)
  mean_y= apply(as.matrix(self$y),2,mean)
  t = as.numeric(self$t)
  testn = length(testT)
  ############################### make prediction for derivative ######################################################
  y_t = array(c(0),testn)
  z_t = array(c(0),testn)
  for (i in 1:testn)
  {
    y_t[i] = self$ker$kern(t(t),testT[i])%*%self$b +mean_y
    z_t[i] = self$ker$dkdt( testT[i],t(t))%*%self$b
  }
  return(list("pred"=y_t,"grad"=z_t))
}                                      #predictT

#gradient matching的参数返回去哪了
#调用 basis 列


>>>>>>> 8da0e641601be3da65f7603035b565a8c5442c8c
