<<<<<<< HEAD
if(!require(parallel))install.packages("parallel");library(parallel)
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
    qqnorm(residual);qqline(residual,col="red")
    plot(interp, residual, main = "Residual vs interpolation")
    abline(h = 0, lty = 3)
  }
  return(list(residual = residual, interp = interp))
}

noise = 0.8  ## set the variance of noise
SEED = 19000
set.seed(SEED)

Lorenz_fun=function(t,x,par_ode){
  sigma=par_ode[1]
  r=par_ode[2]
  beta=par_ode[3]
  d_x=sigma*(x[2]-x[1])
  d_y=r*x[1]-x[1]*x[3]-x[2]
  d_z=-beta*x[3]+x[1]*x[2]
  as.matrix(c(d_x,d_y,d_z))
}


kkk0 = ode$new(2,fun=Lorenz_fun) 
## set the initial values for each stat           at time zero.
xinit = as.matrix(c(1,2,3))
## set the time interval for the ode numerical solver.
tinterv = c(0,20)

kkk0$solve_ode(par_ode=c(3,28,8/3),xinit,tinterv)

## Add noise to the numerical solution of the ode model and use it as the noisy observation. n_o = max( dim( kkk0$y_ode) )
y_true= t(kkk0$y_ode)
n_o = max(dim( y_true))
t_no = kkk0$t

y_no = y_true + rmvnorm(n_o,c(0,0,0),noise*diag(3))


##########################
# Inference on 3 state #
##########################

init_par=rep(1,3)
init_yode=t(y_no)
odem= ode$new(sample=2,fun=Lorenz_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
rkgres=rkg(odem,y_no,ktype="rbf")

par(mfrow=c(3,1))
for(i in 1:3){
plot(odem$t,y_true[,i],type="n",xlab="Time",ylab=paste("State",i))
points(odem$t,y_no[,i],col="black",pch=4)      #observations
lines(odem$t,rkgres$intp[i,],col=c("purple","blue","green")[i],lwd=2,lty=2) #Interpolated smooth line using kernel method
lines(odem$t,y_true[,i],col="red")     #True
legend("topright",legend=c("True","RBF kernel"),col=c(c("purple","blue","green")[i],"red"),lty=c(2,1),text.width = 2)
}


diagnostic(rkgres,type="rkg",index=2,qq_plot=T)
odem$ode_par


loss.function=function(x,ODE,y_no){
  solt <- ode45(ODE, t0=0, tfinal=20, y0=x[c(1,2,3)], par_ode=x[-c(1,2,3)])
  y_solve=deval(solt$t,solt$y,t_no)
  y_gap=y_solve-y_no
  loss=sum(y_gap[-1,]^2)
  if(is.na(loss)) loss=Inf
  return(loss)
}

DE=DEoptim(loss.function,lower=c(-3,-3,-3,0,0,0),upper=c(3,3,3,40,40,40),
           ODE=Lorenz_fun,y_no=y_no,
           control=list(itermax=3000,storepopfrom=1, trace=T, parallelType=1,packages=c("pracma"),parVar=list("t_no","Lorenz_fun","y_no")))


summary(DE)

=======
if(!require(parallel))install.packages("parallel");library(parallel)
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
    qqnorm(residual);qqline(residual,col="red")
    plot(interp, residual, main = "Residual vs interpolation")
    abline(h = 0, lty = 3)
  }
  return(list(residual = residual, interp = interp))
}

noise = 0.8  ## set the variance of noise
SEED = 19000
set.seed(SEED)

Lorenz_fun=function(t,x,par_ode){
  sigma=par_ode[1]
  r=par_ode[2]
  beta=par_ode[3]
  d_x=sigma*(x[2]-x[1])
  d_y=r*x[1]-x[1]*x[3]-x[2]
  d_z=-beta*x[3]+x[1]*x[2]
  as.matrix(c(d_x,d_y,d_z))
}


kkk0 = ode$new(2,fun=Lorenz_fun) 
## set the initial values for each stat           at time zero.
xinit = as.matrix(c(1,2,3))
## set the time interval for the ode numerical solver.
tinterv = c(0,20)

kkk0$solve_ode(par_ode=c(3,28,8/3),xinit,tinterv)

## Add noise to the numerical solution of the ode model and use it as the noisy observation. n_o = max( dim( kkk0$y_ode) )
y_true= t(kkk0$y_ode)
n_o = max(dim( y_true))
t_no = kkk0$t

y_no = y_true + rmvnorm(n_o,c(0,0,0),noise*diag(3))


##########################
# Inference on 3 state #
##########################

init_par=rep(1,3)
init_yode=t(y_no)
odem= ode$new(sample=2,fun=Lorenz_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
rkgres=rkg(odem,y_no,ktype="rbf")

par(mfrow=c(3,1))
for(i in 1:3){
plot(odem$t,y_true[,i],type="n",xlab="Time",ylab=paste("State",i))
points(odem$t,y_no[,i],col="black",pch=4)      #observations
lines(odem$t,rkgres$intp[i,],col=c("purple","blue","green")[i],lwd=2,lty=2) #Interpolated smooth line using kernel method
lines(odem$t,y_true[,i],col="red")     #True
legend("topright",legend=c("True","RBF kernel"),col=c(c("purple","blue","green")[i],"red"),lty=c(2,1),text.width = 2)
}


diagnostic(rkgres,type="rkg",index=2,qq_plot=T)
odem$ode_par


loss.function=function(x,ODE,y_no){
  solt <- ode45(ODE, t0=0, tfinal=20, y0=x[c(1,2,3)], par_ode=x[-c(1,2,3)])
  y_solve=deval(solt$t,solt$y,t_no)
  y_gap=y_solve-y_no
  loss=sum(y_gap[-1,]^2)
  if(is.na(loss)) loss=Inf
  return(loss)
}

DE=DEoptim(loss.function,lower=c(-3,-3,-3,0,0,0),upper=c(3,3,3,40,40,40),
           ODE=Lorenz_fun,y_no=y_no,
           control=list(itermax=3000,storepopfrom=1, trace=T, parallelType=1,packages=c("pracma"),parVar=list("t_no","Lorenz_fun","y_no")))


summary(DE)

>>>>>>> 8da0e641601be3da65f7603035b565a8c5442c8c
