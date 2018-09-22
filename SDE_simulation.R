###########################################################################################################
###################### SIMULATION by SDE ##################################################################
###########################################################################################################
#clear all
rm(list=ls(all=TRUE))
#Load packages
library(zoo)
library(sde)

#Load data
data_profile = read.csv("Profile_data2.csv",header=TRUE,sep=",")
data_prices = read.csv("Prices_SEK_13_15_T_Temp_hydro.csv",header=TRUE,sep=";")
attach(data_profile)
M <- data_profile
S <- data_prices
colSr = 12
testvec = M[1:4000,colSr]

for (i in 1:length(S[1,])) {
    S[,i] = na.locf(S[,i])
    S[,i] = as.numeric(S[,i])
}

len = length(testvec)-1

initVal     = 1
drift       = 0.005
vol         = 0.1

#Proxy for input drift, format: premium over GA

#drift2=runif(len+1,min=0.99,max=1.01)
#drift2 = diff(testvec)
drift2 = log(testvec[2:(len+1)]/testvec[1:len]) #approx log-returns
ret    = testvec[2:(len+1)]/testvec[1:len]-1
retTrue = S[2:(len+1),colSr-1]/S[1:len,colSr-1]-1
std      = sqrt(var(retTrue))
rVol = 1.56846/100 #estimated realized volatility per hour, on average

vol2 = sqrt(var(drift2))

tvec = 1:len
Bvec = rnorm(len)


#DISCRETE GEOMETRIC BROWNIAN MOTION
BM=c(testvec[1])
BMd = BM
for (i in 2:len) {
    #dX          = ret[i]*BMd[i-1]+sd*BMd[i-1]*rnorm(1)
    dX          = (ret[i-1]+rVol*rnorm(1))*BMd[i-1]
    BMd[i]     = BMd[i-1]+dX
}


mean(drift2)
vol2
mu=mean(M[2:len,8])
matplot(tvec,cbind(M[2:len,8], mu*testvec[1:len], mu*BMd),type="l",col=c("grey","red","black"),lty="solid")

drift = log(testvec[2:(len+1)]/testvec[1:len])
cumdrift = cumsum(drift)
#vol = sqrt(var(drift))
vol=rVol



#SEMI-CONTINUOUS GEOMETRIC BROWNIAN MOTION
BM = testvec[1]*exp((cumdrift/tvec-(vol^2)/2)*tvec+vol*cumsum(rnorm(len)-rnorm(len)))

matplot(tvec,cbind(M[2:len,8],mu*testvec[2:len],mu*BM),type="l",col=c("grey","red","black"),lty="solid")

ks.test(M[2:len,8],mu*BMd)

# Ornstein-Uhlenbeck process
nlen=1000
Y=S[1:nlen,colSr-1]
Profile=mean(S[,colSr-1])*M[1:nlen,colSr]
X=log(Y)
mu=mean(X)
vol=sqrt(var(X))

#Use EM algorithm (?) /MC to obtain optimal theta-coefficient
#Stochastic volatility inclusion
#What if we do a O-U model of prices with stochastic mean and volatility?
#This can be done by adjusting x in the first expression d with an alpha-parameter (?)
colSr=12
co=40
d <- expression(co*(mu-x))
s <- expression(vol*sqrt(2*co))
X<-sde.sim(X0=mu,drift=d, sigma=s,N=nlen-1)
X=exp(X)
#plot(X,main="Ornstein-Uhlenbeck")

Y=Y
matplot(1:nlen,cbind(Y,X,Profile),col=c("red","black","blue"),type="l",lty="solid")

#plotdist(X,histo=TRUE,demp=TRUE)
hist(Y,main="Price")
ks.test(X,Y)

#O-U works for Wind but with seasonal volatility and mean, maybe

#Simulate an Ornstein-uhlenbeck process using SDE package
#Compare it with actual fit
#Make sure to load package sde first

###########################################################################################################
#10. SDE
fSDE <- function(S,colSr=11,cof=40,nlen=8760,initVal=0,stats=TRUE,bounded=TRUE,bCapacity = 0,logT = TRUE) {
    if(nlen==0) { nlen = length(S[,1])}
    Head = names(S)
    colName = Head[colSr]
    Y=S[1:nlen,colSr]
    Ymax = as.numeric(bCapacity)
    if (bCapacity == 0) { Ymax = max(Y)}
    X = Y
    if (logT == TRUE) { X=log(Y)}
    mu=mean(X)
    if (initVal==0) {initVal=mu}
    vol=sqrt(var(X))
    k=cof
    d = as.expression(bquote(.(k)*(.(mu)-x)))
    s = as.expression(bquote(.(vol)*sqrt(2*.(k))))
    X = sde.sim(X0=mu,drift=d,sigma=s,N=nlen-1)
    if ( logT==TRUE) {X=exp(X)}
    if(bounded==TRUE) {
        X=ifelse(X>1.1*Ymax,1.1*Ymax-sample(0:(Ymax/5),size=nlen,replace=TRUE),X)
    }
    if (stats==TRUE) {
        matplot(1:nlen,cbind(Y,X),col=c("grey","black"),type="l",lty="solid")
        hist(X,seq(min(X),max(X),len=50),main=paste(colName,"simulated O-U process"))
        hist(Y,seq(min(Y),max(Y),len=50),main=paste(colName,"actual"))
        ks.test(X,Y)
    }
    return(X)
}

#Plausible that max capacity bounds the process

fSDE(S,cof=150)

g <- expression( sin(x+t) )

g[[1]]
# sin(x)

f <- function(x,t){ eval( g[[1]] ) }