####################################################################################################
####################### Volatility Analysis ########################################################
####################################################################################################
#clear all
rm(list=ls(all=TRUE))

#Load packages
library(zoo)
library(gam)
library(MASS)
library(fitdistrplus)
library(gPdtest)
library(msm)

#Load created functions
source("functions.R")

#Load data
data_prices = read.csv("Prices_SEK_13_15_T_Temp_hydro.csv",header=TRUE,sep=";")
attach(data_prices)
colPr=8
#Headers in dataframe
#1      2       3   4       5   6   7   8   9   10      11      12      13      14
#Date	Month	Day	Hour	SYS	SE1	SE2	SE3	SE4	DEMAND	Wind	Hydro	Nuclear	Gas_diesel	
#15         16      17  18      19
#Heat_other	Unspec	Sun	SUPPLY	IMPORT
####################################################################################################
#Define a new dataframe M and adjust data for missing values/NA
M <- data_prices

for (i in 1:length(M[1,])) {
    M[,i] = na.locf(M[,i])
    M[,i] = as.numeric(M[,i])
    #M[,i] = abs(M[,i]) #Make all values absolute
}
#Save original matrix
OrgM = M

####################################################################################################
#Volatility analysis. Using the function fReasVol in function library.
#Done "manually" for illustrative- and runtime purposes...

#Interval setting
a   = 1
b   = length(M[,1])
M   = M[a:b,]

#fRealVol <- function(M,rPer=24,rHour=23,colSr=8,colHr=4,colD=3,colM=2,fullData=FALSE,plotz=FALSE,stdev=FALSE) {

SumPrRealVol <- fRealVol(M,fullData=TRUE,plotz=TRUE,stdev=FALSE)
MPrVol = SumPrRealVol[[4]]

for (i in 1:length(MPrVol[1,])) {
    MPrVol[,i] = na.locf(MPrVol[,i])
    MPrVol[,i] = as.numeric(MPrVol[,i])
    #M[,i] = abs(M[,i]) #Make all values absolute
}

realVol = MPrVol[,colPr]

#Realized vol
#RvolProfile = fProfile(MPrVol,exp=TRUE,plotz=TRUE,colPr=8)
SvolProfile = fProfile(MPrVol,exp=TRUE,plotz=TRUE,colPr=8)
####################################################################################################
#Creating proxy for the supply stack, variables as % of total production

X1 	= M$Hydro
X2	= M$Wind
X3	= M$Nuclear
X4	= M$Heat_other
X5  = M$Gas_diesel
X6  = M$Sun
X7  = M$Unspec
corDF  = data.frame(X1,X2,X3,X4,X5,X6,X7)
covDF   = corDF[2:length(corDF[,1]),]

for (i in 1:7) {
    covDF[,i] = corDF[2:length(corDF[,i]),i]/corDF[1:(length(corDF[,i])-1),i]-1
}

is.na(covDF) <- do.call(cbind,lapply(covDF, is.infinite))


Mcov = cov(covDF,use="complete")*sqrt(length(a:b))


TOT                 = MPrVol$Hydro+MPrVol$Wind+MPrVol$Heat_other + MPrVol$Nuclear + MPrVol$Gas_diesel + MPrVol$Sun + MPrVol$Unspec
MPrVol$Hydro        = MPrVol$Hydro/TOT
MPrVol$Wind         = MPrVol$Wind/TOT
MPrVol$Heat_other   = MPrVol$Heat_other/TOT
MPrVol$Nuclear      = MPrVol$Nuclear/TOT
MPrVol$Gas_diesel   = MPrVol$Gas_diesel/TOT
MPrVol$Unspec       = MPrVol$Unspec/TOT
MPrVol$Sun          = MPrVol$Sun/TOT

X1 	= MPrVol$Hydro
X2	= MPrVol$Wind
X3	= MPrVol$Nuclear
X4	= MPrVol$Heat_other
X5  = MPrVol$Gas_diesel
X6  = MPrVol$Sun
X7  = MPrVol$Unspec
X8  = MPrVol$Temp
X9  = MPrVol$SE_hydro

rvolDF              = data.frame(X1,X2,X3,X4,X5,X6,X7)
DF =  data.frame(X1,X2,X3,X4,X5,X6,X7,X8,X9)

predVol = realVol

Dmat = data.matrix(rvolDF)
for (i in 1:length(predVol)) {
    x           =  crossprod(Dmat[i,],Mcov)
    predVol[i]  = crossprod(x,Dmat[i,])
}


par(mfrow=c(1,1))
matplot(1:length(realVol),cbind(realVol,predVol),type="l",col=c("black","red"))

volModel <- lm(log(realVol) ~ predVol,data.frame(realVol,predVol))
predVol2 = exp(predict(volModel,data.frame(realVol,predVol)))
volModel2 <- glm(log(realVol)~poly(X1,10)+poly(X2,10)+poly(X3,10)+poly(X4,10)+poly(X5,10)+poly(X6,10)+poly(X7,10)+poly(X8,10)+poly(X9,10),data=DF)
predVol3 = exp(predict(volModel2,DF))


matplot(1:length(realVol),cbind(realVol,predVol2,predVol3),type="l",
        col=c("black","blue","green"),lty="solid",
        ylab = "Volatility,% variation", xlab ="Day i")
        
        

###########################################################################################################
#8. SCATTER. 
#Scatter plot
#Function for creating scatter plot between XX and YY, color of dots decidd by value of ZZ
#ColorPlot(vector, string, vector, string, vector, string)
#The strings are explanations of the vector
#Example: ColorPlot(N$Wind,"Wind",N$Hydro,"Hydro",M$SE3,"Price")
###########################################################################################################

#fColorPlot <- function(XX,x_is,YY,y_is,ZZ,ZZ_is){

fColorPlot(M$Temp,"Temperature",M$Heat_other,"Heat Power production",M$SE3,"Price")
fColorPlot(M$Temp,"Temperature",abs(M$DEMAND),"Demand",M$SE3,"Price")
fColorPlot(M$Wind,"Wind Production",M$Heat_other,"Heat Power production",M$SE3,"Price")
fColorPlot(M$Hydro,"Hydro Production",M$Heat_other,"Heat Power production",M$SE3,"Price")
fColorPlot(M$Nuclear,"Nuclear Production",M$Heat_other,"Heat Power production",M$SE3,"Price")
fColorPlot(M$Gas_diesel,"Gas and Diesel Production",M$Heat_other,"Heat Power production",M$SE3,"Price")