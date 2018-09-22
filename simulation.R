####################################################################################################
###################### SIMULATION ##################################################################
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
library(sde)

#Load created functions
source("functions.R")

#Load data
data_profile = read.csv("Profile_data2.csv",header=TRUE,sep=",")
attach(data_profile)

#Load data
data_prices = read.csv("Prices_SEK_13_15_T_Temp_hydro.csv",header=TRUE,sep=";")
attach(data_prices)



#Headers in dataframe
#1      2       3   4       5   6   7   8   9   10      11      12      13      14
#Date	Month	Day	Hour	SYS	SE1	SE2	SE3	SE4	DEMAND	Wind	Hydro	Nuclear	Gas_diesel	
#15         16      17  18      19      20  21          22          23          24
#Heat_other	Unspec	Sun	SUPPLY	IMPORT Temp	No_hydro	SE_hydro	FI_hydro	Week

####################################################################################################
#Define a new dataframe M and adjust data for missing values/NA
M <- data_prices
N <- data_profile
N[,1]<-NULL

for (i in 1:length(M[1,])) {
    M[,i] = as.numeric(M[,i])
    M[,i] = na.locf(M[,i])
    M[,i] = as.numeric(M[,i])
    N[,i] = as.numeric(N[,i])
    #M[,i] = abs(M[,i])*mean(N[,i],na.rm=TRUE) #Create real series
    N[,i] = na.locf(N[,i])
    N[,i] = as.numeric(N[,i])
}

#Save original matrix
OrgM = M
OrgN = N

#Clean for import
colD = 10
colI = 19

M[,colD] = abs(M[,colD])-M[,colI]

colT = 20
colN = 13
colW = 11
colS = 17
colHr = 22
colH = 12
colHe = 15
colG = 14
colU = 16

nLen = length(M[,colD])

D <- fSinFit(M, colSr=colD,plots=FALSE,st=500,OU=FALSE)
Temp <- fSinFit(M, colSr=colT,plots=FALSE,st=500,OU=TRUE,cof=50)
W <- fSDE(M,colW,nlen=0,cof=5)
#S <- fSinFit(M, colSr=colS,plots=TRUE,st=500,OU=FALSE,cof=1,yrd=TRUE)
S <- mean(M[,colS])*N[,colS]
Hr <- mean(M[,colHr])*N[,colHr]

Nuc <- mean(M[,colN])*N[,colN]

resDemand = M[,colD]-M[,colW]-M[,colS]-M[,colN]
estResD = D - W - S - Nuc
uw = 0.000
gw = 0.00

x=0.0 +Temp*0 #-34*Temp/estResD
#x=ifelse(x<0,0,x)
#x=ifelse(x>1,0.2,x)
plot(x,type="l")

rws = cbind(1-x-gw-uw,x,gw+0*x,uw+0*x) #Percentage of residual demand taken, Hydro, Heat, Gas, Unspec
resHydro = rws[1:nLen,1]*estResD
resHeat = rws[1:nLen,2]*estResD
resGas = rws[1:nLen,3]*estResD
resUnspec = rws[1:nLen,4]*estResD


NewM = M
NewM[,colT] = Temp
NewM[,colN] = Nuc
NewM[,colW] = W
NewM[,colS] = S
NewM[,colHr] = Hr
NewM[,colD] = D
NewM[,colH] = resHydro
NewM[,colHe] = resHeat
NewM[,colG] = resGas
NewM[,colU] = resUnspec


write.csv(NewM,"Simulation_data5.csv")

fSimulate <- function(M,P,wCSV=FALSE,SDEcol=c(11), SDEcf=c(10), PRcol=c(17,13,22),
                        SINcol=c(10,20),sinOU=c(FALSE,TRUE),sinOUcf=c(0,10),
                        resDcol=c(12,15,14,16)) {
                        
                        
                        
                        
}