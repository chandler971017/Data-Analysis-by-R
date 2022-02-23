<<<<<<< HEAD
if(!require(parallel))install.packages("parallel");library(parallel)
if(!require(mvtnorm))install.packages("mvtnorm");library(mvtnorm)
if(!require(KGode))install.packages("KGode");library(KGode)
library(tidyverse)

#################################
# Modify the functions in KGode #
#################################
#we need the mean values of the estimates in diagnostic
#we want a qqline in diagnostic
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
    par(mfrow = c(1, 2))
    qqnorm(residual);qqline(residual,lty=3,col="red")
    plot(interp, residual, main = "Residual vs interpolation")
    abline(h = 0, lty = 3)
  }
  return(list(residual = residual, interp = interp))
}

bootstrap <- function(kkk, y_no, ktype, K, ode_par, intp_data, www=NULL) {
  intp = do.call(rbind, intp_data) # convert from list of lists to array
  ode_pars=c()
  for ( i in 1:K )
  {
    # compute residuals
    residuals = kkk$y_ode - intp
    
    # sample with replacement
    resampled_residuals = t(apply(residuals, 1, function(row) sample(row, length(row), replace=TRUE)))
    
    # add to interpolation
    resampled_data = intp + resampled_residuals
    
    # make a new kkk object with the resampled data
    new_kkk = ode$new(1, fun=kkk$ode_fun, grfun=kkk$gr_lNODE, t=kkk$t,
                      ode_par=ode_par, y_ode=resampled_data)
    
    # run gradient matching again
    if(is.null(www)) {
      x = rkg(new_kkk, t(resampled_data), ktype)
    } else {
      ### learn interpolates in warped time domain
      intpl = c()
      gradl = c()
      nst = nrow(kkk$y_ode)
      n_o = max(dim(kkk$y_ode))
      for( st in 1:nst) {
        new_rbf= RBF$new(1)
        wk = rkhs$new(resampled_data[st,], www$wtime[st,], rep(1, n_o), 1, new_rbf)
        wk$skcross(5)
        intpl = rbind(intp, wk$predict()$pred)
        gradl = rbind(gradl, wk$predict()$grad*www$dtilda[st,])
      }
      inipar = rep(0.1, length(new_kkk$ode_par))
      new_kkk$optim_par(inipar, intpl, gradl)
    }
    
    new_ode_par = new_kkk$ode_par
    ode_pars = rbind(ode_pars, new_ode_par)
  }
  
  # compute median absolute standard deviation from the bootstrap replicates
  mads = apply(ode_pars, 2, stats::mad)
  return(list(mads=mads,ode_pars=ode_pars))
}

noise = 0.20  ## set the variance of noise
SEED = 19000
set.seed(SEED)

## Define ode function, we use lotka-volterra model in this example.
## we have two ode states x[1], x[2] and four ode parameters alpha, beta, gamma and delta. LV_fun = function(t,x,par_ode){
LV_fun = function(t,x,par_ode){
  alpha=par_ode[1]
  beta=par_ode[2]
  gamma=par_ode[3]
  delta=par_ode[4]
  as.matrix(c( alpha*x[1]-beta*x[2]*x[1] , -gamma*x[2]+delta*x[1]*x[2] ))
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


########################
# Inference on 2 state #
########################
init_par=rep(1,4)
init_yode=t(y_no)
odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
start.rkg=proc.time()
rkgres=rkg(odem,y_no,ktype="rbf")
stop.rkg=proc.time()
stop.rkg-start.rkg
odem$ode_par
# plot(odem$t,rkgres$intp[1,])
# lines(odem$t,rkgres$intp[1,],col="blue",lwd=2,lty=3)

diagnostic(rkgres,type="rkg",index=2,qq_plot=T)
diagnostic(rkgres_mlp,type="rkg",index=2,qq_plot=T)


#################################
# Interpolate at any time point #                   
#################################
interp_rkhs=rkgres$bbb    # 2 tates 2 sets of kernel basis functions.
nst =length(interp_rkhs)
interp_rkhs[[1]]$predictT()$pred

rmvnorm(n_o,c(0,0),noise*diag(2))
################################################
# 100 initializations for observation_vector 
################################################
start.time=proc.time()
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:100])


clusterExport(cl=cl, varlist=c("y_true", "noise", "n_o", "LV_fun","kkk0"))
par.list=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,4)
  #different seed gets different y_no
  y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="rbf")
  return(odem$ode_par)})
#bell
stopCluster(cl)
stop.time=proc.time()

print(stop.time-start.time)
par.variation=do.call(rbind,par.list)
par.variation%>%
ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012)+
  geom_point(aes(x=parameters,y=true),color="red",size=4)+
  facet_wrap(~Kernel)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=14))

################################################
# 100 initializations for MLP
################################################
start.time=proc.time()
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:100])



clusterExport(cl=cl, varlist=c("y_true", "noise", "n_o", "LV_fun","kkk0"))
par.list.mlp=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,4)
  #different seed gets different y_no
  y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="mlp")
  return(odem$ode_par)})
#bell
stopCluster(cl)
stop.time=proc.time()

print(stop.time-start.time)
par.variation.mlp=do.call(rbind,par.list.mlp)

par.variation.mlp%>%
  as.data.frame()%>%
  rename(alpha=V1,beta=V2,gamma=V3,delta=V4)%>%
  gather(key="parameters",value = "estimates")->par.variation.mlp
par.variation.mlp$Kernel="MLP"
par.variation$Kernel="RBF"

pooled=rbind(par.variation,par.variation.mlp)

pooled%>%
  
  ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012)+
  geom_point(aes(x=parameters,y=true),color="red",size=4)+
  facet_wrap(~Kernel)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=14))
# labs(title="The Dot plot of parameter estimates for the Lotka-Volterra model for 100 different
# random initializations. The true values for the parameters are alpha=0.2, beta=0.35, gamma=0.7,
# delta=0.4.The crossbar covers the 0.025 range around the true parameter values.")

# obj_names=list.files(pattern="Principles of Probability and Statistics 2020-21")
# file.rename(obj_names,to=paste(gsub("Principles of Probability and Statistics 2020-21","",x=obj_names)))


##########################
# Inference on 2 state #
##########################

init_par=rep(1,4)
init_yode=t(y_no)
odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
rkgres=rkg(odem,y_no,ktype="rbf")
odem$ode_par
par(mfrow=c(2,1),mar=c(0.5,4.5,3,3))
for(i in 1:2){
  plot(odem$t,y_true[,i],type="n",xlab="Time",ylab=paste("State",i),cex.lab=1.5)
  points(odem$t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),rkgres$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,col=c('blue',"green")[i],lwd=2,lty=2)
  lines(odem$t,rkgres$intp[i,],col=c("blue","green")[i],lwd=2,lty=2) #Interpolated smooth line using kernel method
  lines(odem$t,y_true[,i],col="red")     #True
  legend("topright",legend=c("Observation","RBF kernel","True"),inset = c(0.06,0),bty = "n",col=c("black",c("blue","green")[i],"red"),pch=c(4,NA,NA),lty=c(NA,2,1),lwd=c(NA,2,2),cex=1.2,pt.cex = 1,text.width = 2,seg.len=1.5)
}
title(outer=T,xlab="Time",line=2,cex.lab=1.5)

############
# MLP
############
rkgres_mlp=rkg(odem,y_no,ktype="mlp")

par(mfrow=c(2,1),mar=c(0.5,4.5,3,3),oma = c(3,0.5,0,0) + 0.1)
for(i in 1:2){
  plot(odem$t,y_true[,i],type="n",xlab="",ylab=paste("State",i),cex.lab=1.5)
  points(odem$t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),rkgres$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,col=c('blue',"green")[i],lwd=2,lty=2)
  lines(odem$t,rkgres_mlp$intp[i,],col=c("blue","green")[i],lwd=2,lty=2) #Interpolated smooth line using kernel method
  lines(odem$t,y_true[,i],col="red")     #True
  legend("topright",legend=c("Observation","MLP kernel","True"),inset = c(0.06,0),bty = "n",col=c("black",c("blue","green")[i],"red"),pch=c(4,NA,NA),lty=c(NA,2,1),lwd=c(NA,2,2),cex=1.2,pt.cex = 1,text.width = 2,seg.len=1.5)
}
title(outer=T,xlab="Time",line=2,cex.lab=1.5)

par.variation%>%
  as.data.frame()%>%
  rename(alpha=V1,beta=V2,gamma=V3,delta=V4)%>%
  gather(key="parameters",value = "estimates")->par.variation

# save(par.list,par.variation,file ="data.RData")

#########
# MAD
##########

interp_rkhs = rkgres$bbb
nst = length(interp_rkhs)
ode_par = odem$ode_par
# intp_data = list()
# for( i in 1:nst) {
#    intp_data[[i]] = interp_rkhs[[i]]$predictT(
#    interp_rkhs[[i]]$t)$pred
#    }
intp_data= as.data.frame(t(rkgres$intp))
K = 20
mads = bootstrap(odem,y_no,ktype="rbf",K,ode_par,
                      intp_data)




ode_par
apply(mads$ode_pars,2,mean)

mads$mads


############
# RKG3     #
############
start_rkg3=proc.time()
crtype="i"
lam = c(10,1,0.1,0.01)
lambdalist = crossv(lam,odem,rkgres$bbb,crtype,y_no)
res=third(lambdalist[[1]],odem,bbb=rkgres$bbb,crtype)
stop_rkg3=proc.time()
stop_rkg3-start_rkg3
res$oppar
par(mfrow=c(2,1),mar=c(0.5,4.5,3,3),oma = c(3,0.5,0,0) + 0.1)
for(i in 1:2){
  plot(odem$t,y_true[,i],type="n",xlab="Time",ylab=paste("State",i),cex.lab=1.5)
  points(odem$t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),rkgres$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,col=c('blue',"green")[i],lwd=2,lty=2)
  lines(seq(0,100,by=0.01),res$rk3$rk[[i]]$predictT(seq(0,100,by=0.01))$pred,col="purple",lwd=3,lty=3) #Interpolated smooth line using kernel method
  lines(odem$t,y_true[,i],col="red")     #True
  legend("topleft",legend=c("Observation","RBF kernel","ODE regularized","True"),inset = c(0.06,0),bty = "n",col=c("black",c("blue","green")[i],"purple","red"),pch=c(4,NA,NA,NA),lty=c(NA,2,3,1),lwd=c(NA,2,2,2),cex=1.2,pt.cex = 1,text.width = 2,seg.len=1.5)
}
title(outer=T,xlab="Time",line=2,cex.lab=1.5)


start.time=proc.time()
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:100])



clusterExport(cl=cl, varlist=c("y_true", "noise", "n_o", "LV_fun","kkk0"))
par.list.rkg3=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,4)
  #different seed gets different y_no
  y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="rbf")
  lam = c(10,1,0.1,0.01)
  crtype="i"
  lambdalist = crossv(lam,odem,rkgres$bbb,crtype,y_no)
  res=third(lambdalist[[1]],odem,bbb=rkgres$bbb,crtype)
  return(res$oppar)})

par.variation.rkg3 =do.call(rbind,par.list.rkg3)
par.variation.rkg3%>%
  as.data.frame()%>%
  rename(alpha=V1,beta=V2,gamma=V3,delta=V4)%>%
  gather(key="parameters",value = "estimates")->par.variation.rkg3
par.variation.rkg3$Kernel="ODE regularized"
par.variation$Kernel="RBF"
par.variation.rkg3$true=par.variation$true

pooled_rkg3=rbind(par.variation,par.variation.rkg3)

pooled_rkg3%>%
  
  ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012)+
  geom_point(aes(x=parameters,y=true),color="red",size=4)+
  facet_wrap(~Kernel)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=14))

#bell
stopCluster(cl)
stop.time=proc.time()
-start.time+stop.time
################
# THird bootstrap
################
y_rkg3=cbind(res$rk3$rk[[1]]$predict()$pred,res$rk3$rk[[2]]$predict()$pred)
estimates_pooled=c()
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:21])



clusterExport(cl=cl, varlist=c("estimates_pooled","y_no", "noise", "n_o", "odem","kkk0","y_rkg3"))

estimates_pooled=parLapply(cl,X=as.list(1:20),fun =function(x){
  # compute residuals
  residuals = y_no - y_rkg3
  
  # sample with replacement
  resampled_residuals = apply(residuals, 2, function(column) sample(column, length(column), replace=TRUE))
 
  # add to interpolation
  resampled_data = y_rkg3 + resampled_residuals
  rkgres=rkg(odem,resampled_data,ktype="rbf")
  crtype="i"
  res=third(10,odem,bbb=rkgres$bbb,crtype)
  estimates_pooled=rbind(estimates_pooled,res$oppar)
  return(estimates_pooled)})

do.call(rbind,estimates_pooled)->estimates_pooled

mads_rkg3 = apply(estimates_pooled, 2, stats::mad)
mads_rkg3
apply(estimates_pooled, 2, mean)
##################

?round
=======
if(!require(parallel))install.packages("parallel");library(parallel)
if(!require(mvtnorm))install.packages("mvtnorm");library(mvtnorm)
if(!require(KGode))install.packages("KGode");library(KGode)
library(tidyverse)

#################################
# Modify the functions in KGode #
#################################
#we need the mean values of the estimates in diagnostic
#we want a qqline in diagnostic
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
    par(mfrow = c(1, 2))
    qqnorm(residual);qqline(residual,lty=3,col="red")
    plot(interp, residual, main = "Residual vs interpolation")
    abline(h = 0, lty = 3)
  }
  return(list(residual = residual, interp = interp))
}

bootstrap <- function(kkk, y_no, ktype, K, ode_par, intp_data, www=NULL) {
  intp = do.call(rbind, intp_data) # convert from list of lists to array
  ode_pars=c()
  for ( i in 1:K )
  {
    # compute residuals
    residuals = kkk$y_ode - intp
    
    # sample with replacement
    resampled_residuals = t(apply(residuals, 1, function(row) sample(row, length(row), replace=TRUE)))
    
    # add to interpolation
    resampled_data = intp + resampled_residuals
    
    # make a new kkk object with the resampled data
    new_kkk = ode$new(1, fun=kkk$ode_fun, grfun=kkk$gr_lNODE, t=kkk$t,
                      ode_par=ode_par, y_ode=resampled_data)
    
    # run gradient matching again
    if(is.null(www)) {
      x = rkg(new_kkk, t(resampled_data), ktype)
    } else {
      ### learn interpolates in warped time domain
      intpl = c()
      gradl = c()
      nst = nrow(kkk$y_ode)
      n_o = max(dim(kkk$y_ode))
      for( st in 1:nst) {
        new_rbf= RBF$new(1)
        wk = rkhs$new(resampled_data[st,], www$wtime[st,], rep(1, n_o), 1, new_rbf)
        wk$skcross(5)
        intpl = rbind(intp, wk$predict()$pred)
        gradl = rbind(gradl, wk$predict()$grad*www$dtilda[st,])
      }
      inipar = rep(0.1, length(new_kkk$ode_par))
      new_kkk$optim_par(inipar, intpl, gradl)
    }
    
    new_ode_par = new_kkk$ode_par
    ode_pars = rbind(ode_pars, new_ode_par)
  }
  
  # compute median absolute standard deviation from the bootstrap replicates
  mads = apply(ode_pars, 2, stats::mad)
  return(list(mads=mads,ode_pars=ode_pars))
}

noise = 0.20  ## set the variance of noise
SEED = 19000
set.seed(SEED)

## Define ode function, we use lotka-volterra model in this example.
## we have two ode states x[1], x[2] and four ode parameters alpha, beta, gamma and delta. LV_fun = function(t,x,par_ode){
LV_fun = function(t,x,par_ode){
  alpha=par_ode[1]
  beta=par_ode[2]
  gamma=par_ode[3]
  delta=par_ode[4]
  as.matrix(c( alpha*x[1]-beta*x[2]*x[1] , -gamma*x[2]+delta*x[1]*x[2] ))
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


########################
# Inference on 2 state #
########################
init_par=rep(1,4)
init_yode=t(y_no)
odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
start.rkg=proc.time()
rkgres=rkg(odem,y_no,ktype="rbf")
stop.rkg=proc.time()
stop.rkg-start.rkg
odem$ode_par
# plot(odem$t,rkgres$intp[1,])
# lines(odem$t,rkgres$intp[1,],col="blue",lwd=2,lty=3)

diagnostic(rkgres,type="rkg",index=2,qq_plot=T)
diagnostic(rkgres_mlp,type="rkg",index=2,qq_plot=T)


#################################
# Interpolate at any time point #                   
#################################
interp_rkhs=rkgres$bbb    # 2 tates 2 sets of kernel basis functions.
nst =length(interp_rkhs)
interp_rkhs[[1]]$predictT()$pred

rmvnorm(n_o,c(0,0),noise*diag(2))
################################################
# 100 initializations for observation_vector 
################################################
start.time=proc.time()
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:100])


clusterExport(cl=cl, varlist=c("y_true", "noise", "n_o", "LV_fun","kkk0"))
par.list=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,4)
  #different seed gets different y_no
  y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="rbf")
  return(odem$ode_par)})
#bell
stopCluster(cl)
stop.time=proc.time()

print(stop.time-start.time)
par.variation=do.call(rbind,par.list)
par.variation%>%
ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012)+
  geom_point(aes(x=parameters,y=true),color="red",size=4)+
  facet_wrap(~Kernel)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=14))

################################################
# 100 initializations for MLP
################################################
start.time=proc.time()
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:100])



clusterExport(cl=cl, varlist=c("y_true", "noise", "n_o", "LV_fun","kkk0"))
par.list.mlp=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,4)
  #different seed gets different y_no
  y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="mlp")
  return(odem$ode_par)})
#bell
stopCluster(cl)
stop.time=proc.time()

print(stop.time-start.time)
par.variation.mlp=do.call(rbind,par.list.mlp)

par.variation.mlp%>%
  as.data.frame()%>%
  rename(alpha=V1,beta=V2,gamma=V3,delta=V4)%>%
  gather(key="parameters",value = "estimates")->par.variation.mlp
par.variation.mlp$Kernel="MLP"
par.variation$Kernel="RBF"

pooled=rbind(par.variation,par.variation.mlp)

pooled%>%
  
  ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012)+
  geom_point(aes(x=parameters,y=true),color="red",size=4)+
  facet_wrap(~Kernel)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=14))
# labs(title="The Dot plot of parameter estimates for the Lotka-Volterra model for 100 different
# random initializations. The true values for the parameters are alpha=0.2, beta=0.35, gamma=0.7,
# delta=0.4.The crossbar covers the 0.025 range around the true parameter values.")

# obj_names=list.files(pattern="Principles of Probability and Statistics 2020-21")
# file.rename(obj_names,to=paste(gsub("Principles of Probability and Statistics 2020-21","",x=obj_names)))


##########################
# Inference on 2 state #
##########################

init_par=rep(1,4)
init_yode=t(y_no)
odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
rkgres=rkg(odem,y_no,ktype="rbf")
odem$ode_par
par(mfrow=c(2,1),mar=c(0.5,4.5,3,3))
for(i in 1:2){
  plot(odem$t,y_true[,i],type="n",xlab="Time",ylab=paste("State",i),cex.lab=1.5)
  points(odem$t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),rkgres$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,col=c('blue',"green")[i],lwd=2,lty=2)
  lines(odem$t,rkgres$intp[i,],col=c("blue","green")[i],lwd=2,lty=2) #Interpolated smooth line using kernel method
  lines(odem$t,y_true[,i],col="red")     #True
  legend("topright",legend=c("Observation","RBF kernel","True"),inset = c(0.06,0),bty = "n",col=c("black",c("blue","green")[i],"red"),pch=c(4,NA,NA),lty=c(NA,2,1),lwd=c(NA,2,2),cex=1.2,pt.cex = 1,text.width = 2,seg.len=1.5)
}
title(outer=T,xlab="Time",line=2,cex.lab=1.5)

############
# MLP
############
rkgres_mlp=rkg(odem,y_no,ktype="mlp")

par(mfrow=c(2,1),mar=c(0.5,4.5,3,3),oma = c(3,0.5,0,0) + 0.1)
for(i in 1:2){
  plot(odem$t,y_true[,i],type="n",xlab="",ylab=paste("State",i),cex.lab=1.5)
  points(odem$t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),rkgres$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,col=c('blue',"green")[i],lwd=2,lty=2)
  lines(odem$t,rkgres_mlp$intp[i,],col=c("blue","green")[i],lwd=2,lty=2) #Interpolated smooth line using kernel method
  lines(odem$t,y_true[,i],col="red")     #True
  legend("topright",legend=c("Observation","MLP kernel","True"),inset = c(0.06,0),bty = "n",col=c("black",c("blue","green")[i],"red"),pch=c(4,NA,NA),lty=c(NA,2,1),lwd=c(NA,2,2),cex=1.2,pt.cex = 1,text.width = 2,seg.len=1.5)
}
title(outer=T,xlab="Time",line=2,cex.lab=1.5)

par.variation%>%
  as.data.frame()%>%
  rename(alpha=V1,beta=V2,gamma=V3,delta=V4)%>%
  gather(key="parameters",value = "estimates")->par.variation

# save(par.list,par.variation,file ="data.RData")

#########
# MAD
##########

interp_rkhs = rkgres$bbb
nst = length(interp_rkhs)
ode_par = odem$ode_par
# intp_data = list()
# for( i in 1:nst) {
#    intp_data[[i]] = interp_rkhs[[i]]$predictT(
#    interp_rkhs[[i]]$t)$pred
#    }
intp_data= as.data.frame(t(rkgres$intp))
K = 20
mads = bootstrap(odem,y_no,ktype="rbf",K,ode_par,
                      intp_data)




ode_par
apply(mads$ode_pars,2,mean)

mads$mads


############
# RKG3     #
############
start_rkg3=proc.time()
crtype="i"
lam = c(10,1,0.1,0.01)
lambdalist = crossv(lam,odem,rkgres$bbb,crtype,y_no)
res=third(lambdalist[[1]],odem,bbb=rkgres$bbb,crtype)
stop_rkg3=proc.time()
stop_rkg3-start_rkg3
res$oppar
par(mfrow=c(2,1),mar=c(0.5,4.5,3,3),oma = c(3,0.5,0,0) + 0.1)
for(i in 1:2){
  plot(odem$t,y_true[,i],type="n",xlab="Time",ylab=paste("State",i),cex.lab=1.5)
  points(odem$t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),rkgres$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,col=c('blue',"green")[i],lwd=2,lty=2)
  lines(seq(0,100,by=0.01),res$rk3$rk[[i]]$predictT(seq(0,100,by=0.01))$pred,col="purple",lwd=3,lty=3) #Interpolated smooth line using kernel method
  lines(odem$t,y_true[,i],col="red")     #True
  legend("topleft",legend=c("Observation","RBF kernel","ODE regularized","True"),inset = c(0.06,0),bty = "n",col=c("black",c("blue","green")[i],"purple","red"),pch=c(4,NA,NA,NA),lty=c(NA,2,3,1),lwd=c(NA,2,2,2),cex=1.2,pt.cex = 1,text.width = 2,seg.len=1.5)
}
title(outer=T,xlab="Time",line=2,cex.lab=1.5)


start.time=proc.time()
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:100])



clusterExport(cl=cl, varlist=c("y_true", "noise", "n_o", "LV_fun","kkk0"))
par.list.rkg3=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,4)
  #different seed gets different y_no
  y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=LV_fun,grfun=NULL,t=kkk0$t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="rbf")
  lam = c(10,1,0.1,0.01)
  crtype="i"
  lambdalist = crossv(lam,odem,rkgres$bbb,crtype,y_no)
  res=third(lambdalist[[1]],odem,bbb=rkgres$bbb,crtype)
  return(res$oppar)})

par.variation.rkg3 =do.call(rbind,par.list.rkg3)
par.variation.rkg3%>%
  as.data.frame()%>%
  rename(alpha=V1,beta=V2,gamma=V3,delta=V4)%>%
  gather(key="parameters",value = "estimates")->par.variation.rkg3
par.variation.rkg3$Kernel="ODE regularized"
par.variation$Kernel="RBF"
par.variation.rkg3$true=par.variation$true

pooled_rkg3=rbind(par.variation,par.variation.rkg3)

pooled_rkg3%>%
  
  ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012)+
  geom_point(aes(x=parameters,y=true),color="red",size=4)+
  facet_wrap(~Kernel)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=14))

#bell
stopCluster(cl)
stop.time=proc.time()
-start.time+stop.time
################
# THird bootstrap
################
y_rkg3=cbind(res$rk3$rk[[1]]$predict()$pred,res$rk3$rk[[2]]$predict()$pred)
estimates_pooled=c()
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:21])



clusterExport(cl=cl, varlist=c("estimates_pooled","y_no", "noise", "n_o", "odem","kkk0","y_rkg3"))

estimates_pooled=parLapply(cl,X=as.list(1:20),fun =function(x){
  # compute residuals
  residuals = y_no - y_rkg3
  
  # sample with replacement
  resampled_residuals = apply(residuals, 2, function(column) sample(column, length(column), replace=TRUE))
 
  # add to interpolation
  resampled_data = y_rkg3 + resampled_residuals
  rkgres=rkg(odem,resampled_data,ktype="rbf")
  crtype="i"
  res=third(10,odem,bbb=rkgres$bbb,crtype)
  estimates_pooled=rbind(estimates_pooled,res$oppar)
  return(estimates_pooled)})

do.call(rbind,estimates_pooled)->estimates_pooled

mads_rkg3 = apply(estimates_pooled, 2, stats::mad)
mads_rkg3
apply(estimates_pooled, 2, mean)
##################

?round
>>>>>>> 8da0e641601be3da65f7603035b565a8c5442c8c
