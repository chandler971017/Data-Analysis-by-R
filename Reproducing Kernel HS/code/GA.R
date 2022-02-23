<<<<<<< HEAD
if(!require(parallel))install.packages("parallel");library(parallel)
if(!require(mvtnorm))install.packages("mvtnorm");library(mvtnorm)
if(!require(KGode))install.packages("KGode");library(KGode)

if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

if(!require(pracma))install.packages("pracma");library(pracma) #ode23
if(!require(DEoptim))install.packages("DEoptim");library(DEoptim)
LV_fun = function(t,x,par_ode){
  alpha=par_ode[1]
  beta=par_ode[2]
  gamma=par_ode[3]
  delta=par_ode[4]
  as.matrix(c( alpha*x[1]-beta*x[2]*x[1] , -gamma*x[2]+delta*x[1]*x[2] ))
}
##############
# Simulation #
##############
# To solve the ODE, we will need initial values, however our initial values are noisy. So initial values will be included in population and in the evaluation of loss function
# In the first version, t=0 is excluded from the loss evaluation, in the second version, it is incorporated but regularised, we can use a cross validation to determine the strength.
noise = 0.20  ## set the variance of noise
SEED = 19000
set.seed(SEED)
kkk0 = ode$new(1,fun=LV_fun) 
## set the initial values for each stat at time zero.
xinit = as.matrix(c(1,2))
## set the time interval for the ode numerical solver.
tinterv = c(0,30)
## solve the ode numerically using predefined ode parameters. alpha=1, beta=1, gamma=4, delta=1. kkk0$solve_ode(c(1,1,4,1),xinit,tinterv)
kkk0$solve_ode(c(0.2,0.35,0.7,0.4),xinit,tinterv)
## Add noise to the numerical solution of the ode model and use it as the noisy observation. n_o = max( dim( kkk0$y_ode) )
y_true= t(kkk0$y_ode)
n_o = max(dim( y_true))
t_no = kkk0$t
y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
t=kkk0$t

# the first few positions are left for xinit
### record the time lapse for one single GA


loss.function=function(x,ODE,y_no){
  solt <- ode45(ODE, t0=0, tfinal=30, y0=x[c(1,2)], par_ode=x[-c(1,2)])
  y_solve=deval(solt$t,solt$y,t)
  y_gap=y_solve-y_no
  loss=sum(y_gap[-1,]^2)
  if(is.na(loss)) loss=Inf
    return(loss)
}
DE=DEoptim(loss.function,lower=c(0.5*y_no[1,1],0.5*y_no[1,2],0,0,0,0),upper=c(1.5*y_no[1,1],1.5*y_no[1,2],5,5,5,5),
           ODE=LV_fun,y_no=y_no,
           control=list(itermax=2000,storepopfrom=1, trace=T, parallelType=1,packages=c("pracma"),parVar=list("t","LV_fun","y_no")))
summary(DE)




###################
# Evaluate the fitness
################

###Estimate on the initial value is vital
## want to test the quality of the estimate- within 2 sigma range

est_par=unlist(DE$optim)
#state 1
result <- ode45(LV_fun, t0=0, tfinal=30, y0=est_par[c(1,2)], par_ode=est_par[3:6])
y_solve=deval(result$t,result$y,t)
resid_de=y_no-y_solve
qqnorm(resid_de[,1])
qqline(resid_de[,1],col="red")
qqnorm(resid_de[,2])
qqline(resid_de[,2],col="red")




par(mfrow=c(2,1),mar=c(0.5,4.5,3,3),oma = c(3,0.5,0,0) + 0.1)
for(i in 1:2){
plot(t,y_solve[,i],type="n",ylab=paste("State",i),xlab = "Time",cex.lab=1.5)
lines(t,y_solve[,i],pch=16,col=c("blue","green")[i],lty=2)
points(t,y_true[,i],,type="n")
points(t,y_no[,i],col="black",pch=4)
lines(t,y_true[,i],pch=16,col="red")
legend("topright",legend=c("Observation","Solved ODEs","True"),inset = c(0.06,0),bty = "n",col=c("black",c("blue","green")[i],"red"),pch=c(4,NA,NA),lty=c(NA,2,1),lwd=c(NA,2,2),cex=1.2,pt.cex = 1,text.width = 2,seg.len=1.5)
}
title(outer=T,xlab="Time",line=2,cex.lab=1.5)

#MAD
#residuals
start.time=proc.time()
estimates_pooled=c()
for(i in 1:21){
  # compute residuals
  residuals = y_no - y_solve
  
  # sample with replacement
  resampled_residuals = apply(residuals, 2, function(column) sample(column, length(column), replace=TRUE))
  
  # add to interpolation
  resampled_data = y_solve + resampled_residuals
  #we believe the initial value is within the range of 0.5 to 1.5 observed value
  DE=tryCatch(DEoptim(loss.function,lower=c(0.5*resampled_data[1,1],0.5*resampled_data[1,2],0,0,0,0),upper=c(1.5*resampled_data[1,1],1.5*resampled_data[1,2],5,5,5,5), 
                      ODE=LV_fun,y_no= resampled_data,
             control=list(itermax=1500,storepopfrom=1, trace=T, parallelType=1,packages=c("pracma"),parVar=list("t","LV_fun","y_no"))),
             error=function(e) NA)
  if (is.na(DE)){
    next
  }else{par_estimates=DE$optim$bestmem
  estimates_pooled=rbind(estimates_pooled,par_estimates)}}
end.time=proc.time()
lapse=end.time-start.time
lapse

mads_GA = apply(estimates_pooled, 2, stats::mad)
mads_GA[-c(1,2)]
apply(estimates_pooled, 2, mean)


loss.function(c(1,2,0.2,0.35,0.7,0.4),ODE=LV_fun,y_no)

DE$optim$bestval
par(mfrow=c(1,1),mar=c(2,4,2,2))
plot(DE, plot.type = "bestvalit",pch=16,cex=0.1)
title(xlab="Generation",outer = T,line=0.5)


process=data.frame(`Objective function value`=DE$member$bestvalit,`Generation`=1:2000)
process%>%
  ggplot(aes(x=Generation,y=Objective.function.value))+
  geom_line()+
  labs(title="Convergence plot")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
  title = element_text(size=16))

=======
if(!require(parallel))install.packages("parallel");library(parallel)
if(!require(mvtnorm))install.packages("mvtnorm");library(mvtnorm)
if(!require(KGode))install.packages("KGode");library(KGode)

if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

if(!require(pracma))install.packages("pracma");library(pracma) #ode23
if(!require(DEoptim))install.packages("DEoptim");library(DEoptim)
LV_fun = function(t,x,par_ode){
  alpha=par_ode[1]
  beta=par_ode[2]
  gamma=par_ode[3]
  delta=par_ode[4]
  as.matrix(c( alpha*x[1]-beta*x[2]*x[1] , -gamma*x[2]+delta*x[1]*x[2] ))
}
##############
# Simulation #
##############
# To solve the ODE, we will need initial values, however our initial values are noisy. So initial values will be included in population and in the evaluation of loss function
# In the first version, t=0 is excluded from the loss evaluation, in the second version, it is incorporated but regularised, we can use a cross validation to determine the strength.
noise = 0.20  ## set the variance of noise
SEED = 19000
set.seed(SEED)
kkk0 = ode$new(1,fun=LV_fun) 
## set the initial values for each stat at time zero.
xinit = as.matrix(c(1,2))
## set the time interval for the ode numerical solver.
tinterv = c(0,30)
## solve the ode numerically using predefined ode parameters. alpha=1, beta=1, gamma=4, delta=1. kkk0$solve_ode(c(1,1,4,1),xinit,tinterv)
kkk0$solve_ode(c(0.2,0.35,0.7,0.4),xinit,tinterv)
## Add noise to the numerical solution of the ode model and use it as the noisy observation. n_o = max( dim( kkk0$y_ode) )
y_true= t(kkk0$y_ode)
n_o = max(dim( y_true))
t_no = kkk0$t
y_no = y_true + rmvnorm(n_o,c(0,0),noise*diag(2))
t=kkk0$t

# the first few positions are left for xinit
### record the time lapse for one single GA


loss.function=function(x,ODE,y_no){
  solt <- ode45(ODE, t0=0, tfinal=30, y0=x[c(1,2)], par_ode=x[-c(1,2)])
  y_solve=deval(solt$t,solt$y,t)
  y_gap=y_solve-y_no
  loss=sum(y_gap[-1,]^2)
  if(is.na(loss)) loss=Inf
    return(loss)
}
DE=DEoptim(loss.function,lower=c(0.5*y_no[1,1],0.5*y_no[1,2],0,0,0,0),upper=c(1.5*y_no[1,1],1.5*y_no[1,2],5,5,5,5),
           ODE=LV_fun,y_no=y_no,
           control=list(itermax=2000,storepopfrom=1, trace=T, parallelType=1,packages=c("pracma"),parVar=list("t","LV_fun","y_no")))
summary(DE)




###################
# Evaluate the fitness
################

###Estimate on the initial value is vital
## want to test the quality of the estimate- within 2 sigma range

est_par=unlist(DE$optim)
#state 1
result <- ode45(LV_fun, t0=0, tfinal=30, y0=est_par[c(1,2)], par_ode=est_par[3:6])
y_solve=deval(result$t,result$y,t)
resid_de=y_no-y_solve
qqnorm(resid_de[,1])
qqline(resid_de[,1],col="red")
qqnorm(resid_de[,2])
qqline(resid_de[,2],col="red")




par(mfrow=c(2,1),mar=c(0.5,4.5,3,3),oma = c(3,0.5,0,0) + 0.1)
for(i in 1:2){
plot(t,y_solve[,i],type="n",ylab=paste("State",i),xlab = "Time",cex.lab=1.5)
lines(t,y_solve[,i],pch=16,col=c("blue","green")[i],lty=2)
points(t,y_true[,i],,type="n")
points(t,y_no[,i],col="black",pch=4)
lines(t,y_true[,i],pch=16,col="red")
legend("topright",legend=c("Observation","Solved ODEs","True"),inset = c(0.06,0),bty = "n",col=c("black",c("blue","green")[i],"red"),pch=c(4,NA,NA),lty=c(NA,2,1),lwd=c(NA,2,2),cex=1.2,pt.cex = 1,text.width = 2,seg.len=1.5)
}
title(outer=T,xlab="Time",line=2,cex.lab=1.5)

#MAD
#residuals
start.time=proc.time()
estimates_pooled=c()
for(i in 1:21){
  # compute residuals
  residuals = y_no - y_solve
  
  # sample with replacement
  resampled_residuals = apply(residuals, 2, function(column) sample(column, length(column), replace=TRUE))
  
  # add to interpolation
  resampled_data = y_solve + resampled_residuals
  #we believe the initial value is within the range of 0.5 to 1.5 observed value
  DE=tryCatch(DEoptim(loss.function,lower=c(0.5*resampled_data[1,1],0.5*resampled_data[1,2],0,0,0,0),upper=c(1.5*resampled_data[1,1],1.5*resampled_data[1,2],5,5,5,5), 
                      ODE=LV_fun,y_no= resampled_data,
             control=list(itermax=1500,storepopfrom=1, trace=T, parallelType=1,packages=c("pracma"),parVar=list("t","LV_fun","y_no"))),
             error=function(e) NA)
  if (is.na(DE)){
    next
  }else{par_estimates=DE$optim$bestmem
  estimates_pooled=rbind(estimates_pooled,par_estimates)}}
end.time=proc.time()
lapse=end.time-start.time
lapse

mads_GA = apply(estimates_pooled, 2, stats::mad)
mads_GA[-c(1,2)]
apply(estimates_pooled, 2, mean)


loss.function(c(1,2,0.2,0.35,0.7,0.4),ODE=LV_fun,y_no)

DE$optim$bestval
par(mfrow=c(1,1),mar=c(2,4,2,2))
plot(DE, plot.type = "bestvalit",pch=16,cex=0.1)
title(xlab="Generation",outer = T,line=0.5)


process=data.frame(`Objective function value`=DE$member$bestvalit,`Generation`=1:2000)
process%>%
  ggplot(aes(x=Generation,y=Objective.function.value))+
  geom_line()+
  labs(title="Convergence plot")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
  title = element_text(size=16))

>>>>>>> 8da0e641601be3da65f7603035b565a8c5442c8c
