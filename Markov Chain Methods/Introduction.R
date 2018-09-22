#Just some simple examples...

#This is a vector:
a = c(1,2,3,4)
a
#see!
#now, c is a function for concatenation...
b=c(5,6,7,8)
c=c(a,b)
c
#see!
#to assign values to a variable, simply use:
x <- c
x
#nice!
#Here are some examples of vector operations:
z=a[1:5]
z
sum(a)
a[3]
x=log(x)
x
length(x)
names(x)
#8x8 matrix of x:
M=matrix(x,nrow=length(x),ncol=length(x))
M
diag(M)
X=matrix(1:9,nrow=3) #fills a matrix with 3 rows with numbers 1 to 9
X
Y=matrix(0:8,nrow=3)
Y
Y=t(Y)#transpose of Y
Y
dim(Y) #dimension of a matrix
l =X%*%Y #matrix product
l
l=X*Y #term by term product
l
Z=rbind(X,Y) #vertical bind of vectors
Z
Z=cbind(X,Y) #horizontal bind of vector
Z
choX = chol(X) #cholesky decomposition of X
choX
invX = solve(X)
invX