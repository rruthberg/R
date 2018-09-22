#Simple AR example: X_t = 0.9*X_n-1 + N(0,1)
x=rnorm(1)
for (t in 2:10^3){
	x=c(x, 0.5*x[t-1]+rnorm(1))
	}
par(mfrow=c(1,2))
plot(x,type="l",main="Simulated AR(1) with a=0.5",xlab="time",ylab="X",lwd=1,lty=1,col="steelblue",ylim=range(cumsum(x)))
lines(cumsum(x),lwd=1,col="orange3")
hist(x,col="grey",main="Distribution of dX")