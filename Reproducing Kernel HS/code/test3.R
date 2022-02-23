<<<<<<< HEAD

require(mvtnorm)
library(KGode)
SEED = 19537
set.seed(SEED)

FN_fun <- function(t, x, par_ode) {
  a = par_ode[1]
  b = par_ode[2]
  c = par_ode[3]
  as.matrix(c(c*(x[1]-x[1]^3/3 + x[2]),-1/c*(x[1]-a+b*x[2])))
}

solveOde = ode$new(2,fun=FN_fun)

xinit = as.matrix(c(-1,-1))

tinterv = c(0,10)

solveOde$solve_ode(par_ode=c(0.2,0.2,3),xinit,tinterv)

n_o = max(dim(solveOde$y_ode))

noise = 0.01 

y_no = t(solveOde$y_ode)+rmvnorm(n_o,c(0,0),noise*diag(2))

t_no = solveOde$t

init_par = rep(c(0.1),3)
init_yode = t(y_no)
init_t = t_no

odem = ode$new(fun=FN_fun,grfun=NULL,t=init_t,ode_par=init_par,y_ode=init_yode)
ktype = 'rbf'
rkgres = rkg(odem,y_no,ktype)

odem$ode_par

=======

require(mvtnorm)
library(KGode)
SEED = 19537
set.seed(SEED)

FN_fun <- function(t, x, par_ode) {
  a = par_ode[1]
  b = par_ode[2]
  c = par_ode[3]
  as.matrix(c(c*(x[1]-x[1]^3/3 + x[2]),-1/c*(x[1]-a+b*x[2])))
}

solveOde = ode$new(2,fun=FN_fun)

xinit = as.matrix(c(-1,-1))

tinterv = c(0,10)

solveOde$solve_ode(par_ode=c(0.2,0.2,3),xinit,tinterv)

n_o = max(dim(solveOde$y_ode))

noise = 0.01 

y_no = t(solveOde$y_ode)+rmvnorm(n_o,c(0,0),noise*diag(2))

t_no = solveOde$t

init_par = rep(c(0.1),3)
init_yode = t(y_no)
init_t = t_no

odem = ode$new(fun=FN_fun,grfun=NULL,t=init_t,ode_par=init_par,y_ode=init_yode)
ktype = 'rbf'
rkgres = rkg(odem,y_no,ktype)

odem$ode_par

>>>>>>> 8da0e641601be3da65f7603035b565a8c5442c8c
