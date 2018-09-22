#Some basic and not so basic statistics and prob distr.
#install.packages("MASS")
#ex. 1.6; a linear model of the form y = a + b*x
x=rnorm(20)
y=3*x+5+rnorm(20,sd=0.3)
reslm=lm(y~x)
summary(reslm)

#variance differs from vector vs matrix!
var(X) #gives a covariance matrix
var(x) #gives the variance in the series
covX=var(X)
cov(x,y)
m = chol(covX)
m

#t-test:
x=rnorm(25) # gives a N(0,1) sample 25 size
t.test(x)
#Pearsons corr-test:
attach(faithful) #some dataset
cor.test(faithful[,1],faithful[,2])
#kolmogorov-smirnov:
ks.test(jitter(faithful[,1]),pnorm) #jitter is a function that removes ties in a dataset
#shapiro normality test
shapiro.test(faithful[,2])
#wilcoxon rank test
wilcox.test(faithful[,1])

#some tests with plots, splines etc
a = c(0,0,0,0,1,1,1,1,2,2,2,2,2,3,3,4,4,4,4,4,5,5,6,6,6,6)
b = c(2,3,4,2,1,5,6,7,8,6,5,4,6,4,3,6,7,8,3,4,1,2,1,8,9,7)
m=tapply(b,a,mean) #means
n=tapply(a,a,mean) #means
plot(a,b,xlim=c(0,6),ylim=c(min(b),max(b)),pch=19)
plot(smooth.spline(a,b)) #natural spline
#bootstrapping
y = rnorm(100)
ystar=sample(y,replace=5)

#ex 1.1 linear regression and bootstrap
x=seq(-3,3,le=5) #equidispersed regressor
y=2+4*x+rnorm(5) #simulated dependent variable
lm(y~x)
#generate residuals used for a bootstrap sample
fit=lm(y~x)
Rdata=fit$residuals #get resids
nBoot=2000 #number of bstrap samplex
B=array(0,dim=c(nBoot,2)) #bstrap array
for(i in 1:nBoot){
	ystar=y+sample(Rdata,replace=T)
	Bfit=lm(ystar~x)
	B[i,]=Bfit$coefficients
}
par(mfrow=c(1,2))
y = c(0:15)
y=y/5
hist(B[,1],breaks=y,col="grey",main="Intercept")
y = c(30:50)
y=y/10
hist(B[,2],breaks=y,col="grey",main="Slope")
#ACF
plot(acf(ldeaths))