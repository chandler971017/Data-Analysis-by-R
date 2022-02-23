<<<<<<< HEAD
if(!require(parallel))install.packages("parallel");library(parallel)
if(!require(mvtnorm))install.packages("mvtnorm");library(mvtnorm)
if(!require(KGode))install.packages("KGode");library(KGode)
library(tidyverse)
set.seed(19000)
 BP_fun <- function(t, x, par_ode) {
     k1 = par_ode[1]
     k2 = par_ode[2]
     k3 = par_ode[3]
     k4 = par_ode[4]
     k5 = par_ode[5]
     k6 = par_ode[6]
  as.matrix( c( -k1*x[1]-k2*x[1]*x[3]+k3*x[4],
                    k1*x[1],
                    -k2*x[1]*x[3]+k3*x[4]+k5*x[5]/(k6+x[5]),
                    k2*x[1]*x[3]-k3*x[4]-k4*x[4],
                k4*x[4]-k5*x[5]/(k6+x[5])) )
 }
solveOde = ode$new(sample=1,fun=BP_fun)
xinit = as.matrix(c(1,0,1,0,0))
tinterv = c(0,100)

solveOde$solve_ode(par_ode=c(0.07,0.6,0.05,0.3,
                                   0.017,0.3),
                          xinit,tinterv)
n_o = max(dim(solveOde$y_ode))

y_true=t(solveOde$y_ode)
data_std=apply(y_true,2,sd)
y_no = t(solveOde$y_ode) + rmvnorm(n_o,c(0,0,0,0,0),
                                       + (0.3*data_std)^2*diag(5))
t=solveOde$t
par(mfrow=c(3,2))
for(i in 1:5){
plot(c(t,t),c(y_no[,i],y_true[,i]),col=c(rep(1,length(t)),rep(2,length(t))),pch=c(rep(4,length(t)),rep(16,length(t))))
}
biopathway= ode$new(fun=BP_fun,ode_par = rep(0.1,6),grfun=NULL,t=t,y_ode = t(y_no))
biopathway.RBF=rkg(biopathway,y_no,ktype="rbf")
par(mfrow=c(3,2),
    oma = c(3,0.5,0,0) + 0.1,
    mar=c(4, 5, 1, 1) )

for(i in 1:5){
  plot(t,y_no[,i],type="n",xlab="",ylab=paste("State",i),cex.lab=2,cex.axis=1.5)
  points(t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),biopathway.RBF$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,,col=c("green","orange","blue","purple","yellow")[i],lwd=2,lty=2)
  lines(t,y_true[,i],col="red")     #True
  legend("bottom",horiz=T,xpd = TRUE,
         x.intersp=0, xjust=0,inset = c(0,-0.45),bty = "n",legend=c("Observation","RBF kernel","True"),col=c("black",c("green","orange","blue","purple","yellow")[i],"red"),pch=c(4,NA,NA),lty=c(NA,2,1),lwd = c(NA,2,2),cex=1.5,pt.cex = 1.5,seg.len=1.5)
}
title(xlab = "Time",outer = T,cex.lab=2,line=0.5)

biopathway= ode$new(sample=1,fun=BP_fun,ode_par = rep(0.1,6),grfun=NULL,t=t,y_ode = t(y_no))
biopathway.mlp=rkg(biopathway,y_no,ktype="mlp")
par(mfrow=c(3,2),
    oma = c(3,0.5,0,0) + 0.1,
    mar=c(4, 5, 1, 1) )
for(i in 1:5){
  plot(t,y_no[,i],type="n",xlab="",ylab=paste("State",i),cex.lab=2,cex.axis=1.5)
  points(t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),biopathway.mlp$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,,col=c("green","orange","blue","purple","yellow")[i],lwd=2,lty=2)
  lines(t,y_true[,i],col="red")     #True
  legend("bottom",horiz=T,xpd = TRUE,
         x.intersp=0, xjust=0,inset = c(0,-0.45),bty = "n",legend=c("Observation","MLP kernel","True"),col=c("black",c("green","orange","blue","purple","yellow")[i],"red"),pch=c(4,NA,NA),,lty=c(NA,2,1),lwd = c(NA,2,2),cex=1.5,pt.cex = 1.5,seg.len=1.5)
}
title(xlab = "Time",outer = T,cex.lab=2,line=0.5)

#################
# 100 INITIALISATION
#################
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:100])



clusterExport(cl=cl, varlist=c("data_std","noise", "n_o", "BP_fun","solveOde","t"))
par.list.BP_rbf=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,6)
  #different seed gets different y_no
  y_no = t(solveOde$y_ode) + rmvnorm(n_o,c(0,0,0,0,0),
                                     + (0.3*data_std)^2*diag(5))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=BP_fun,grfun=NULL,t=t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="rbf")
  return(tryCatch(odem$ode_par,error=function(e) NA))})

par.list.BP_mlp=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,6)
  #different seed gets different y_no
  y_no = t(solveOde$y_ode) + rmvnorm(n_o,c(0,0,0,0,0),
                                     + (0.3*data_std)^2*diag(5))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=BP_fun,grfun=NULL,t=t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="mlp")
  return(tryCatch(odem$ode_par,error=function(e) NA))})
stopCluster(cl)

save.image("G:/OneDrive - University of Glasgow/Project/code/Biopathway.RData")

BP.rbf.variation=do.call(rbind,par.list.BP_rbf)%>%
  as.data.frame()
BP.mlp.variation=do.call(rbind,par.list.BP_mlp)%>%
  as.data.frame()
BP.rbf.variation$ratio=BP.rbf.variation$V5/BP.rbf.variation$V6
BP.mlp.variation$ratio=BP.mlp.variation$V5/BP.mlp.variation$V6
BP.mlp.variation%>%
  rename(k1=V1,k2=V2,k3=V3,k4=V4,k5=V5,k6=V6)%>%
  gather(key="parameters",value = "estimates")->BP.mlp.variation
BP.rbf.variation%>%
  rename(k1=V1,k2=V2,k3=V3,k4=V4,k5=V5,k6=V6)%>%
  gather(key="parameters",value = "estimates")->BP.rbf.variation
BP.rbf.variation$Kernel="RBF"

BP.mlp.variation$Kernel="MLP"

pooled=rbind(BP.rbf.variation,BP.mlp.variation)
pooled$true=rep(c(0.07,0.6,0.05,0.3,
             0.017,0.3,0.017/0.3),each=100)

pooled%>%
  filter(!parameters%in%c("k5","k6"))%>%
  ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012,stackratio = 0.5,size=3)+
  ylim(c(0,1))+
  geom_point(aes(x=parameters,y=true),color="red",size=3)+
  facet_wrap(~Kernel)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=17,face="bold"),
        strip.text = element_text(size=14))



plot(solveOde$t,y_no[,1],pch=4,xlab = "Time",ylab="Kernel function",ylim = c(0,0.5))
#lines(seq(0,100,by=0.1),biopathway.RBF$bbb[[1]]$predictT(seq(0,100,by=0.1))$pred)
for(i in 15:28){
  lines(seq(0,100,by=0.01),biopathway.RBF$bbb[[1]]$ker$kern(seq(0,100,by=0.01),t[i]))}


plot(solveOde$t,biopathway.mlp$intp[1,],pch=16)
#lines(seq(0,100,by=0.01),biopathway.mlp$bbb[[1]]$predictT(seq(0,100,by=0.01))$pred)
for(i in 1:25){
lines(seq(0,100,by=0.01),biopathway.mlp$bbb[[1]]$ker$kern(seq(0,100,by=0.01),t[i]))}


# #Handling error
# error.list=list(1,1,1,"d")
# trial.fun=function(x){
#  x+1
# }
# 
# lapply(error.list[1:3],trial.fun)
# a=c()
# e <- simpleError("test error")
# tryCatch(stop(e), finally = print("Hello"))
# tryCatch(stop(e),error=function(e)NA)
# 
# for(i in 1:4){
#   a[i]=restart(trial.fun(error.list[[i]]))}

=======
if(!require(parallel))install.packages("parallel");library(parallel)
if(!require(mvtnorm))install.packages("mvtnorm");library(mvtnorm)
if(!require(KGode))install.packages("KGode");library(KGode)
library(tidyverse)
set.seed(19000)
 BP_fun <- function(t, x, par_ode) {
     k1 = par_ode[1]
     k2 = par_ode[2]
     k3 = par_ode[3]
     k4 = par_ode[4]
     k5 = par_ode[5]
     k6 = par_ode[6]
  as.matrix( c( -k1*x[1]-k2*x[1]*x[3]+k3*x[4],
                    k1*x[1],
                    -k2*x[1]*x[3]+k3*x[4]+k5*x[5]/(k6+x[5]),
                    k2*x[1]*x[3]-k3*x[4]-k4*x[4],
                k4*x[4]-k5*x[5]/(k6+x[5])) )
 }
solveOde = ode$new(sample=1,fun=BP_fun)
xinit = as.matrix(c(1,0,1,0,0))
tinterv = c(0,100)

solveOde$solve_ode(par_ode=c(0.07,0.6,0.05,0.3,
                                   0.017,0.3),
                          xinit,tinterv)
n_o = max(dim(solveOde$y_ode))

y_true=t(solveOde$y_ode)
data_std=apply(y_true,2,sd)
y_no = t(solveOde$y_ode) + rmvnorm(n_o,c(0,0,0,0,0),
                                       + (0.3*data_std)^2*diag(5))
t=solveOde$t
par(mfrow=c(3,2))
for(i in 1:5){
plot(c(t,t),c(y_no[,i],y_true[,i]),col=c(rep(1,length(t)),rep(2,length(t))),pch=c(rep(4,length(t)),rep(16,length(t))))
}
biopathway= ode$new(fun=BP_fun,ode_par = rep(0.1,6),grfun=NULL,t=t,y_ode = t(y_no))
biopathway.RBF=rkg(biopathway,y_no,ktype="rbf")
par(mfrow=c(3,2),
    oma = c(3,0.5,0,0) + 0.1,
    mar=c(4, 5, 1, 1) )

for(i in 1:5){
  plot(t,y_no[,i],type="n",xlab="",ylab=paste("State",i),cex.lab=2,cex.axis=1.5)
  points(t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),biopathway.RBF$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,,col=c("green","orange","blue","purple","yellow")[i],lwd=2,lty=2)
  lines(t,y_true[,i],col="red")     #True
  legend("bottom",horiz=T,xpd = TRUE,
         x.intersp=0, xjust=0,inset = c(0,-0.45),bty = "n",legend=c("Observation","RBF kernel","True"),col=c("black",c("green","orange","blue","purple","yellow")[i],"red"),pch=c(4,NA,NA),lty=c(NA,2,1),lwd = c(NA,2,2),cex=1.5,pt.cex = 1.5,seg.len=1.5)
}
title(xlab = "Time",outer = T,cex.lab=2,line=0.5)

biopathway= ode$new(sample=1,fun=BP_fun,ode_par = rep(0.1,6),grfun=NULL,t=t,y_ode = t(y_no))
biopathway.mlp=rkg(biopathway,y_no,ktype="mlp")
par(mfrow=c(3,2),
    oma = c(3,0.5,0,0) + 0.1,
    mar=c(4, 5, 1, 1) )
for(i in 1:5){
  plot(t,y_no[,i],type="n",xlab="",ylab=paste("State",i),cex.lab=2,cex.axis=1.5)
  points(t,y_no[,i],col="black",pch=4)      #observations
  lines(seq(0,100,by=0.01),biopathway.mlp$bbb[[i]]$predictT(seq(0,100,by=0.01))$pred,,col=c("green","orange","blue","purple","yellow")[i],lwd=2,lty=2)
  lines(t,y_true[,i],col="red")     #True
  legend("bottom",horiz=T,xpd = TRUE,
         x.intersp=0, xjust=0,inset = c(0,-0.45),bty = "n",legend=c("Observation","MLP kernel","True"),col=c("black",c("green","orange","blue","purple","yellow")[i],"red"),pch=c(4,NA,NA),,lty=c(NA,2,1),lwd = c(NA,2,2),cex=1.5,pt.cex = 1.5,seg.len=1.5)
}
title(xlab = "Time",outer = T,cex.lab=2,line=0.5)

#################
# 100 INITIALISATION
#################
ncores=detectCores(logical = F)
cl=makeCluster(ncores)

junk <- clusterEvalQ(cl, 
                     {library(mvtnorm)
                       library(KGode)}) ## Discard result
seed.list=as.list(.Random.seed[1:100])



clusterExport(cl=cl, varlist=c("data_std","noise", "n_o", "BP_fun","solveOde","t"))
par.list.BP_rbf=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,6)
  #different seed gets different y_no
  y_no = t(solveOde$y_ode) + rmvnorm(n_o,c(0,0,0,0,0),
                                     + (0.3*data_std)^2*diag(5))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=BP_fun,grfun=NULL,t=t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="rbf")
  return(tryCatch(odem$ode_par,error=function(e) NA))})

par.list.BP_mlp=parLapply(cl,X=seed.list,fun =function(x){
  set.seed(x)
  init_par=rep(1,6)
  #different seed gets different y_no
  y_no = t(solveOde$y_ode) + rmvnorm(n_o,c(0,0,0,0,0),
                                     + (0.3*data_std)^2*diag(5))
  init_yode=t(y_no)
  odem= ode$new(sample=1,fun=BP_fun,grfun=NULL,t=t,ode_par=init_par,y_ode = init_yode)
  rkgres=rkg(odem,y_no,ktype="mlp")
  return(tryCatch(odem$ode_par,error=function(e) NA))})
stopCluster(cl)

save.image("G:/OneDrive - University of Glasgow/Project/code/Biopathway.RData")

BP.rbf.variation=do.call(rbind,par.list.BP_rbf)%>%
  as.data.frame()
BP.mlp.variation=do.call(rbind,par.list.BP_mlp)%>%
  as.data.frame()
BP.rbf.variation$ratio=BP.rbf.variation$V5/BP.rbf.variation$V6
BP.mlp.variation$ratio=BP.mlp.variation$V5/BP.mlp.variation$V6
BP.mlp.variation%>%
  rename(k1=V1,k2=V2,k3=V3,k4=V4,k5=V5,k6=V6)%>%
  gather(key="parameters",value = "estimates")->BP.mlp.variation
BP.rbf.variation%>%
  rename(k1=V1,k2=V2,k3=V3,k4=V4,k5=V5,k6=V6)%>%
  gather(key="parameters",value = "estimates")->BP.rbf.variation
BP.rbf.variation$Kernel="RBF"

BP.mlp.variation$Kernel="MLP"

pooled=rbind(BP.rbf.variation,BP.mlp.variation)
pooled$true=rep(c(0.07,0.6,0.05,0.3,
             0.017,0.3,0.017/0.3),each=100)

pooled%>%
  filter(!parameters%in%c("k5","k6"))%>%
  ggplot(aes(x=parameters,y=estimates))+
  geom_dotplot(binaxis = "y", stackdir='center',binwidth = 0.012,stackratio = 0.5,size=3)+
  ylim(c(0,1))+
  geom_point(aes(x=parameters,y=true),color="red",size=3)+
  facet_wrap(~Kernel)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=17,face="bold"),
        strip.text = element_text(size=14))



plot(solveOde$t,y_no[,1],pch=4,xlab = "Time",ylab="Kernel function",ylim = c(0,0.5))
#lines(seq(0,100,by=0.1),biopathway.RBF$bbb[[1]]$predictT(seq(0,100,by=0.1))$pred)
for(i in 15:28){
  lines(seq(0,100,by=0.01),biopathway.RBF$bbb[[1]]$ker$kern(seq(0,100,by=0.01),t[i]))}


plot(solveOde$t,biopathway.mlp$intp[1,],pch=16)
#lines(seq(0,100,by=0.01),biopathway.mlp$bbb[[1]]$predictT(seq(0,100,by=0.01))$pred)
for(i in 1:25){
lines(seq(0,100,by=0.01),biopathway.mlp$bbb[[1]]$ker$kern(seq(0,100,by=0.01),t[i]))}


# #Handling error
# error.list=list(1,1,1,"d")
# trial.fun=function(x){
#  x+1
# }
# 
# lapply(error.list[1:3],trial.fun)
# a=c()
# e <- simpleError("test error")
# tryCatch(stop(e), finally = print("Hello"))
# tryCatch(stop(e),error=function(e)NA)
# 
# for(i in 1:4){
#   a[i]=restart(trial.fun(error.list[[i]]))}

>>>>>>> 8da0e641601be3da65f7603035b565a8c5442c8c
           