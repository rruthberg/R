#Inverse Transform 
#Recall if X has df F(x), and U has df U(0,1), then F_inv(U)~X
#equivalently: F(X)~U <-> X~F_inv(U)

#Simulating a exp(1) by uniform
N=10^4
U=runif(N)
X=-log(U)
Y=rexp(N)
par(mfrow=c(1,2))
hist(X,freq=F,main="Inverse Transform")
hist(Y,freq=F,main="Exp(1)")

#Simulating chi-square
#Recal chi-squared is sum of idp standard normals, which can be likened to
#sum of idp 2*exp(1)

v=3
U=runif(v*N)
U=matrix(data=U,nrow=v)
X=-log(U)
X=2*apply(X,2,sum)
par(mfrow=c(1,2))
Y=rchisq(N,df=2*v)
hist(X,freq=F,main="Inverse Transform")
hist(Y,freq=F,main="Chisq(df=6)")