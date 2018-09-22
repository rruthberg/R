####################################################################################################
###################### Seasonality Analysis ########################################################
####################################################################################################
#clear all
rm(list=ls(all=TRUE))

#Load packages
library(zoo)

library(MASS)
library(fitdistrplus)



#Load created functions
source("functions.R")

#Load data
#data_prices = read.csv("Prices_SEK_13_15_T_Temp_hydro.csv",header=TRUE,sep=";")
data_prices = read.csv("Data_11_16.csv",header=TRUE,sep=";")
attach(data_prices)

data_sim = read.csv("Sim_Profile.csv",header=TRUE,sep=";")


#Headers in dataframe
#1      2       3   4       5   6   7   8   9   10      11      12      13      14
#Date	Month	Day	Hour	SYS	SE1	SE2	SE3	SE4	DEMAND	Wind	Hydro	Nuclear	Gas_diesel	
#15         16      17  18      19      20  21          22          23          24
#Heat_other	Unspec	Sun	SUPPLY	IMPORT Temp	No_hydro	SE_hydro	FI_hydro	Week
####################################################################################################
#Define a new dataframe M and adjust data for missing values/NA
M <- data_prices
N <- data_sim

for (i in 1:length(M[1,])) {
    if (class(M[,i]) == "factor") { M[,i] = as.numeric(levels(M[,i]))[M[,i]]}
    M[,i] = na.locf(M[,i])
    M[,i] = as.numeric(M[,i])
    M[,i] = abs(M[,i]) #Make all values absolute
}

for (i in 1:length(N[1,])) {
    if (class(N[,i]) == "factor") { N[,i] = as.numeric(levels(N[,i]))[N[,i]]}
    N[,i] = na.locf(N[,i])
    N[,i] = as.numeric(N[,i])
}


#Save original matrix
OrgM = M
OrgN = N

####################################################################################################
#Profile generation below. Using the function fProfile in function library.
#Done "manually" for illustrative- and runtime purposes...

#Interval setting
a   = 1
b   = length(M[,1])
M   = M[a:b,]

#Price, SE3.
PriceProfile = fProfile(M,exp=TRUE,plotz=TRUE,colPr=8)
PriceProfile[[1]]
PriceProfile[[2]]
PriceProfile[[3]]

#DEMAND
DemandProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=10)
DemandProfile[[1]]
DemandProfile[[2]]
DemandProfile[[3]]

#Wind
WindProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=11)
WindProfile[[1]]
WindProfile[[2]]
WindProfile[[3]]

#Hydro
HydroProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=12)
HydroProfile[[1]]
HydroProfile[[2]]
HydroProfile[[3]]

#Nuclear
NuclearProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=13)
NuclearProfile[[1]]
NuclearProfile[[2]]
NuclearProfile[[3]]

#Heat power
Heat_otherProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=15)
Heat_otherProfile[[1]]
Heat_otherProfile[[2]]
Heat_otherProfile[[3]]

#Gas power
GasProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=14)
GasProfile[[1]]
GasProfile[[2]]
GasProfile[[3]]

#Sun
SunProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=17)
SunProfile[[1]]
SunProfile[[2]]
SunProfile[[3]]

#Unspecified
UnspecProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=16)
UnspecProfile[[1]]
UnspecProfile[[2]]
UnspecProfile[[3]]

#Temperature
TempProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=20)
TempProfile[[1]]
TempProfile[[2]]
TempProfile[[3]]

#Hydro Reservoir Level
HydResProfile = fProfile(M,exp=TRUE,plotz=FALSE,colPr=22)
HydResProfile[[1]]
HydResProfile[[2]]
HydResProfile[[3]]


####################################################################################################
#fFwdProfile <- function(N,HPr,DPr,MPr,colHr=4,colD=3,colM=2) {


fwdPriceProfile = fFwdProfile(N,PriceProfile[[1]],PriceProfile[[2]],PriceProfile[[3]])
fwdDemandProfile = fFwdProfile(N,DemandProfile[[1]],DemandProfile[[2]],DemandProfile[[3]])
fwdWindProfile = fFwdProfile(N,WindProfile[[1]],WindProfile[[2]],WindProfile[[3]])
fwdHydroProfile = fFwdProfile(N,HydroProfile[[1]],HydroProfile[[2]],HydroProfile[[3]])
fwdNuclearProfile = fFwdProfile(N,NuclearProfile[[1]],NuclearProfile[[2]],NuclearProfile[[3]])
fwdHeat_otherProfile = fFwdProfile(N,Heat_otherProfile[[1]],Heat_otherProfile[[2]],Heat_otherProfile[[3]])
fwdGasProfile = fFwdProfile(N,GasProfile[[1]],GasProfile[[2]],GasProfile[[3]])
fwdSunProfile = fFwdProfile(N,SunProfile[[1]],SunProfile[[2]],SunProfile[[3]])
fwdUnspecProfile = fFwdProfile(N,UnspecProfile[[1]],UnspecProfile[[2]],UnspecProfile[[3]])
fwdTempProfile = fFwdProfile(N,TempProfile[[1]],TempProfile[[2]],TempProfile[[3]])
fwdHydResProfile = fFwdProfile(N,HydResProfile[[1]],HydResProfile[[2]],HydResProfile[[3]])

N[,8]   = fwdPriceProfile
N[,10]  = fwdDemandProfile
N[,11]  = fwdWindProfile
N[,12]  = fwdHydroProfile
N[,13]  = fwdNuclearProfile
N[,15]  = fwdHeat_otherProfile
N[,14]  = fwdGasProfile
N[,17]  = fwdSunProfile
N[,16]  = fwdUnspecProfile
N[,20]  = fwdTempProfile
N[,22]  = fwdHydResProfile
write.csv(N,"Sim_Profile.csv")