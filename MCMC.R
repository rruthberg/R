###########################################
### MCMC Empirical Distribution Sampler ###
###########################################
rm(list=ls(all=TRUE)) #Clear all past values
#Random vector of returns
startprice=309
init = 0
vol = 100
data = rnorm(10000,init,vol) #assume data is the % increase day on day
#data = log(data/100)*100

#FORTUM DATA
data_prices = read.csv("Data_11_16.csv",header=TRUE,sep=";")    #Load relevant data.
M <- data_prices
for (i in 1:length(M[1,])) {
    if (class(M[,i]) == "factor") { M[,i] = as.numeric(levels(M[,i]))[M[,i]]}
    M[,i] = as.numeric(M[,i])
    M[,i] = na.locf(M[,i])
    M[,i] = as.numeric(M[,i])
}
colPr   = 8     #Define price used, 8 = SE3
Ret = M[2:length(M$Hydro),]
Ret[,colPr]  = M[,colPr][2:length(Ret[,colPr])]/M[,colPr][1:(length(Ret[,colPr]))] #Returns
Ret[,colPr] = ifelse(Ret[,colPr]==Inf,0,Ret[,colPr])
Ret[,colPr] = ifelse(Ret[,colPr]==-Inf,0,Ret[,colPr])
data=Ret[,colPr]*100



#fdata = (floor(data)+ceiling(data))/2
#fdata = c(floor(data),ceiling(data)) #make the normalized data symmetric
fdata = ceiling(data)
Nsim = 40000

tab = table(fdata)

X = (1:Nsim)*0
X[1]=init
M=X
Z =X
Z[1] = startprice

for(i in 2:Nsim){
	Y = sample(tab,1,replace=TRUE)#Random generator = uniform selection of the value is Y
	fproxy = Y[[1]]
	qproxy = tab[[paste(X[i-1])]]
	fproxy
	qproxy
	rho=min(c(fproxy/qproxy,1)) #This is the key that makes it a markov chain with the chosen properties - beautiful!
	u=runif(1)
	if(u<rho) {
		X[i] = as.integer(names(Y))
	} else {
		X[i] = X[i-1]
	}
	M[i] = X[i]-0.5+runif(1) #generate decimal noise uniformly
	#M[i] = X[i] #generate decimal noise uniformly
	Z[i] = Z[i-1]*(1+M[i]/100)
}

plotPrices=FALSE
if(plotPrices==TRUE){
	M=Z
	data=data_prices[,colPr]
}

par(mfrow=c(2,3))
plot(data,type="l")
plot(fdata,type="l")
plot(M,type="l")
hist(data)
hist(fdata)
hist(M)

par(mfrow=c(2,3))
plot(data[30000:30500],type="l")
plot(fdata[30000:30500],type="l")
plot(M[30000:30500],type="l")
hist(data[30000:30500])
hist(fdata[30000:30500])
hist(M[30000:30500])

par(mfrow=c(2,1))
plot(M[400:600],type="l")
plot(M[3400:3600],type="l")