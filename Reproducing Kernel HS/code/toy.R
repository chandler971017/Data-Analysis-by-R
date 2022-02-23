<<<<<<< HEAD
toy_example=function(x){
  -0.1*x^2+0.1*x+0.5*cos(x)
}
derivative=function(x){
  -0.1*2*x+0.1-0.5*sin(x)
}
t=runif(15,-2,5)
toy_example(t)->y
y_no=y+rnorm(t,0,0.7^2)
# draw a smooth line through a scatter plot
data=cbind(t,y_no)
plot(t,y_no,xlab = "Time",ylab="Observed value",pch=4,lwd=2)

t_evaluated=seq(from=min(t)+0.2,to=max(t)-0.2,by=0.1)
lines(t_evaluated,toy_example(t_evaluated),lwd=2,col="blue")

segments(x0=t[15],x1=t[15],y0=y_no[15],y1=toy_example(t[15]),lty=2,col="red",lwd=2)
points(t[15],toy_example(t[15]),pch=16,col="red")
arrows(t[15],toy_example(t[15]),t[15]+1,toy_example(t[15])+derivative(t[15]),col="red",angle = 15)

5+40*exp(-0.03)-43
mat=matrix(c(44,43,40,27,22,20,1,1,1),byrow = T,nrow=3,ncol = 3)
solve(mat,as.matrix(c(42*1.03,23*1.03,1)))

0.5*(33-33/1.015)-33+34/1.015


=======
toy_example=function(x){
  -0.1*x^2+0.1*x+0.5*cos(x)
}
derivative=function(x){
  -0.1*2*x+0.1-0.5*sin(x)
}
t=runif(15,-2,5)
toy_example(t)->y
y_no=y+rnorm(t,0,0.7^2)
# draw a smooth line through a scatter plot
data=cbind(t,y_no)
plot(t,y_no,xlab = "Time",ylab="Observed value",pch=4,lwd=2)

t_evaluated=seq(from=min(t)+0.2,to=max(t)-0.2,by=0.1)
lines(t_evaluated,toy_example(t_evaluated),lwd=2,col="blue")

segments(x0=t[15],x1=t[15],y0=y_no[15],y1=toy_example(t[15]),lty=2,col="red",lwd=2)
points(t[15],toy_example(t[15]),pch=16,col="red")
arrows(t[15],toy_example(t[15]),t[15]+1,toy_example(t[15])+derivative(t[15]),col="red",angle = 15)

5+40*exp(-0.03)-43
mat=matrix(c(44,43,40,27,22,20,1,1,1),byrow = T,nrow=3,ncol = 3)
solve(mat,as.matrix(c(42*1.03,23*1.03,1)))

0.5*(33-33/1.015)-33+34/1.015


>>>>>>> 8da0e641601be3da65f7603035b565a8c5442c8c
