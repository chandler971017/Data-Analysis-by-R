
#' The 'diagnostic' function
#'
#' This function is used to perform diagnostic procedure to compute the residual and make diagnostic plots.
#'
#' Arguments of the 'diagnostic' function are inference list , inference type, a list of interpolations for each of the ode state from gradient matching, and . It returns a vector of the median absolute standard deviations for each ode state.
#' @import stats
#' @param infer_list a list of inference results including ode objects and inference objects.
#' @param index the index of the ode states which the user want to do the diagnostic analysis.
#' @param type character containing the type of inference methods. User can choose 'rkg', 'third', or 'warp'.
#' @param qq_plot boolean variable, enable or disable the plotting function.
#' @return return list containing :
#' \itemize{ 
#'  \item{} residual - vector containing residual.  
#'  \item{} interp - vector containing interpolation. }  
#' @importFrom graphics abline
#' @importFrom graphics par
#' @importFrom graphics plot
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
#'  toy_grlNODE= function(par,grad_ode,y_p,z_p) {
#'      alpha = par[1]
#'      dres= c(0)
#'       dres[1] = sum( 2*( z_p-grad_ode)*y_p*alpha ) #sum( -2*( z_p[1,2:lm]-dz1)*z1*alpha )
#'       dres
#'    }
#'
#'   t_no = c(0.1,1,2,3,4,8)
#'   n_o = length(t_no)
#'   y_no =  matrix( c(exp(-t_no)),ncol=1  )
#'   ######################## create and initialise ode object #########################################
#'   init_par = rep(c(0.1))
#'  init_yode = t(y_no)
#'  init_t = t_no
#'
#'  kkk = ode$new(1,fun=toy_fun,grfun=toy_grlNODE,t=init_t,ode_par= init_par, y_ode=init_yode )
#'
#'  ##### standard gradient matching
#'  ktype='rbf'
#'  rkgres = rkg(kkk,(y_no),ktype)
#'  rkgdiag = diagnostic( rkgres,1,'rkg',qq_plot=FALSE )
#'}
#'\dontrun{
#' require(mvtnorm)
#' set.seed(SEED);  SEED = 19537
#' FN_fun <- function(t, x, par_ode) {
#' a = par_ode[1]
#' b = par_ode[2]
#' c = par_ode[3]
#' as.matrix(c(c*(x[1]-x[1]^3/3 + x[2]),-1/c*(x[1]-a+b*x[2])))
#' }
#'
#' solveOde = ode$new(sample=2,fun=FN_fun)
#' xinit = as.matrix(c(-1,-1))
#' tinterv = c(0,10)
#' solveOde$solve_ode(par_ode=c(0.2,0.2,3),xinit,tinterv)
#'
#' n_o = max(dim(solveOde$y_ode))
#' noise = 0.01 
#' y_no = t(solveOde$y_ode)+rmvnorm(n_o,c(0,0),noise*diag(2))
#' t_no = solveOde$t
#'
#' odem = ode$new(fun=FN_fun,grfun=NULL,t=t_no,ode_par=rep(c(0.1),3),y_ode=t(y_no))
#' ktype = 'rbf'
#' rkgres = rkg(odem,y_no,ktype)
#' rkgdiag = diagnostic( rkgres,1,'rkg',qq_plot=FALSE )
#'}
#' @author Mu Niu \email{mu.niu@glasgow.ac.uk}
diagnostic<-function(  infer_list , index,type, qq_plot)
{

    if(type=='rkg')
    {
      interp = infer_list$intp[index,]
      residual =  infer_list$bbb[[index]]$y - interp 

    } else if(type=='third') {

      interp = infer_list$rk3$rk[[index]]$predict()$pred
      residual = infer_list$rk3$rk[[index]]$y - interp 

    }else if(type=='warp') {

      warp_inter= infer_list$bbbw
      wfun = infer_list$wfun
      tgrid = infer_list$wtime[index,]
      interp = warp_inter[[index]]$predictT(tgrid)$pred
      residual =  infer_list$bbb[[index]]$y - interp 

    }

    if(qq_plot)
    {
      par(mfrow=c(2,1))
      qqnorm(residual)
      plot(interp,residual,main='Residual vs interpolation')
      abline(h=0,lty=3)
    }

    return( list( 'residual'=residual,'interp'=interp ) )
}

