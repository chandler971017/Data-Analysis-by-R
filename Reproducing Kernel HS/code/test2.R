<<<<<<< HEAD


if(!require(mvtnorm))install.packages("mvtnorm");library(mvtnorm)
if(!require(KGode))install.packages("KGode");library(KGode)
library(tidyverse)



diagnostic=function (infer_list, index, type, qq_plot) 
{
  if (type == "rkg") {
    interp = infer_list$intp[index, ]
    residual = infer_list$bbb[[index]]$y - interp
  }
  else if (type == "third") {
    interp = infer_list$rk3$rk[[index]]$predict()$pred
    residual = infer_list$rk3$rk[[index]]$y - interp
  }
  else if (type == "warp") {
    warp_inter = infer_list$bbbw
    wfun = infer_list$wfun
    tgrid = infer_list$wtime[index, ]
    interp = warp_inter[[index]]$predictT(tgrid)$pred
    residual = infer_list$bbb[[index]]$y - interp
  }
  if (qq_plot) {
    par(mfrow = c(2, 1))
    qqnorm(residual);qqline(residual,lty=3,col="red")
    plot(interp, residual, main = "Residual vs interpolation")
    abline(h = 0, lty = 3)
  }
  return(list(residual = residual, interp = interp))
}

noise = 0.25  ## set the variance of noise
SEED = 19000
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


kkk0 = ode$new(1,fun=LV_fun) 
## set the initial values for each stat           at time zero.
xinit = as.matrix(c(1,2))
## set the time interval for the ode numerical solver.
tinterv = c(0,30)
## solve the ode numerically using predefined ode parameters. alpha=1, beta=1, gamma=4, delta=1. kkk0$solve_ode(c(1,1,4,1),xinit,tinterv)
kkk0$solve_ode(c(0.2,0.35,0.7,0.4),xinit,tinterv)
## Add noise to the numerical solution of the ode model and use it as the noisy observation. n_o = max( dim( kkk0$y_ode) )
y_true= t(kkk0$y_ode)
n_o = max(dim( y_true) )
t_no = kkk0$t
y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))


##########################
# Inference on 2 state #
##########################
init_par=rep(1,4)
init_yode=t(y_no)
odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
rkgres=rkg(odem,y_no,ktype="rbf")
odem$ode_par
 # plot(odem$t,rkgres$intp[1,])
# lines(odem$t,rkgres$intp[1,],col="blue",lwd=2,lty=3)
diagnostic(rkgres,type="rkg",index=2,qq_plot=T)

 

#################################
# Interpolate at any time point #                   #
#################################
interp_rkhs=rkgres$bbb    # 2个states 2 sets of kernel basis functions.
nst =length(interp_rkhs)
interp_rkhs[[1]]$predictT()$pred

rmvnorm(n_o,c(0,0),noise*diag(2))
################################################
# 100 initializations for observation_vector 
################################################
#parallel 用599s, CPU 负载》62%，这个方法才16%，用时超过30 mins
start.time=proc.time()
seed.list=as.list(.Random.seed[1:100])
par.list=lapply(seed.list,FUN=function(x){
  set.seed(x)
  init_par=rep(1,4)
  #different seed gets different y_no
  y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="rbf")
  return(odem$ode_par)})
stop.time=proc.time()
print(stop.time-start.time)

par.variation=do.call(rbind,par.list)
save(par.list,"par.list")

write.csv(par.variation,"par.variation.csv")
par.variation%>%
  as.data.frame()%>%
  rename(alpha=V1,beta=V2,gamma=V3,delta=V4)%>%
gather(key="parameters",value = "estimates")->par.variation

par.variation$true=rep(c(0.2,0.35,0.7,0.4),each=100)
par.variation%>%
  ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012)->p
  p+geom_crossbar(aes(x=parameters,y=true,ymin=true-0.025,ymax=true+0.025),width=0.5,color="red")+
    labs(title="The Dot plot of parameter estimates for the Lotka–Volterra model for 100 different
random initializations. The true values for the parameters are alpha=0.2, beta=0.35, gamma=0.7,
delta=0.4.The crossbar covers the 0.025 range around the true parameter values.")
  
  
    
=======


if(!require(mvtnorm))install.packages("mvtnorm");library(mvtnorm)
if(!require(KGode))install.packages("KGode");library(KGode)
library(tidyverse)



diagnostic=function (infer_list, index, type, qq_plot) 
{
  if (type == "rkg") {
    interp = infer_list$intp[index, ]
    residual = infer_list$bbb[[index]]$y - interp
  }
  else if (type == "third") {
    interp = infer_list$rk3$rk[[index]]$predict()$pred
    residual = infer_list$rk3$rk[[index]]$y - interp
  }
  else if (type == "warp") {
    warp_inter = infer_list$bbbw
    wfun = infer_list$wfun
    tgrid = infer_list$wtime[index, ]
    interp = warp_inter[[index]]$predictT(tgrid)$pred
    residual = infer_list$bbb[[index]]$y - interp
  }
  if (qq_plot) {
    par(mfrow = c(2, 1))
    qqnorm(residual);qqline(residual,lty=3,col="red")
    plot(interp, residual, main = "Residual vs interpolation")
    abline(h = 0, lty = 3)
  }
  return(list(residual = residual, interp = interp))
}

noise = 0.25  ## set the variance of noise
SEED = 19000
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


kkk0 = ode$new(1,fun=LV_fun) 
## set the initial values for each stat           at time zero.
xinit = as.matrix(c(1,2))
## set the time interval for the ode numerical solver.
tinterv = c(0,30)
## solve the ode numerically using predefined ode parameters. alpha=1, beta=1, gamma=4, delta=1. kkk0$solve_ode(c(1,1,4,1),xinit,tinterv)
kkk0$solve_ode(c(0.2,0.35,0.7,0.4),xinit,tinterv)
## Add noise to the numerical solution of the ode model and use it as the noisy observation. n_o = max( dim( kkk0$y_ode) )
y_true= t(kkk0$y_ode)
n_o = max(dim( y_true) )
t_no = kkk0$t
y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))


##########################
# Inference on 2 state #
##########################
init_par=rep(1,4)
init_yode=t(y_no)
odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
rkgres=rkg(odem,y_no,ktype="rbf")
odem$ode_par
 # plot(odem$t,rkgres$intp[1,])
# lines(odem$t,rkgres$intp[1,],col="blue",lwd=2,lty=3)
diagnostic(rkgres,type="rkg",index=2,qq_plot=T)

 

#################################
# Interpolate at any time point #                   #
#################################
interp_rkhs=rkgres$bbb    # 2个states 2 sets of kernel basis functions.
nst =length(interp_rkhs)
interp_rkhs[[1]]$predictT()$pred

rmvnorm(n_o,c(0,0),noise*diag(2))
################################################
# 100 initializations for observation_vector 
################################################
#parallel 用599s, CPU 负载》62%，这个方法才16%，用时超过30 mins
start.time=proc.time()
seed.list=as.list(.Random.seed[1:100])
par.list=lapply(seed.list,FUN=function(x){
  set.seed(x)
  init_par=rep(1,4)
  #different seed gets different y_no
  y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="rbf")
  return(odem$ode_par)})
stop.time=proc.time()
print(stop.time-start.time)

par.variation=do.call(rbind,par.list)
save(par.list,"par.list")

write.csv(par.variation,"par.variation.csv")
par.variation%>%
  as.data.frame()%>%
  rename(alpha=V1,beta=V2,gamma=V3,delta=V4)%>%
gather(key="parameters",value = "estimates")->par.variation

par.variation$true=rep(c(0.2,0.35,0.7,0.4),each=100)
par.variation%>%
  ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012)->p
  p+geom_crossbar(aes(x=parameters,y=true,ymin=true-0.025,ymax=true+0.025),width=0.5,color="red")+
    labs(title="The Dot plot of parameter estimates for the Lotka–Volterra model for 100 different
random initializations. The true values for the parameters are alpha=0.2, beta=0.35, gamma=0.7,
delta=0.4.The crossbar covers the 0.025 range around the true parameter values.")
  
  
    
>>>>>>> 8da0e641601be3da65f7603035b565a8c5442c8c
