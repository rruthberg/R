#Accept-Reject method (rejection sampling)
#Algorithm:
#   1) Generate Y~g and U~U(0,1)
#   2) Accept X=Y if U<=f(Y)/(M*g(Y))
#   3) Return to step 1) otherwise // repeat
##########################
#Implementation:
#u=runif(1)*M
#y=randg(1)
#while (u>f(y)/g(y)){
#	u=runif(1)*M
#	y=randg(1)
#	}
###########################################

