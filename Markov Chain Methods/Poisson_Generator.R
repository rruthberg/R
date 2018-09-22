#Poisson Generator

N=10^3; lambda=10
spread=3*sqrt(lambda)
t=round(seq(max(0,lambda-spread),lambda+spread,1))
prob=ppois(t,lambda)
X=rep(0,N)
for (i in 1:N){
	u=runif(1)
	X[i]=t[1]+sum(prob<u)
	}
plot(X)
hist(X)
