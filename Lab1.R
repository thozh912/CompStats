library(XLConnect)
wb = loadWorkbook("tecator.xls")
data = readWorksheet(wb,sheet = "data" ,header = TRUE)

#Assignments 1 through 4

x1 <- 1/3
x2 <- 1/4
if( (x1 - x2) == 1/12 ){
  print("Teacher was true")
} else{
  print("Teacher was lied")
}
#Outcome is lied.
#I imagine this is due to the finite nature of the significand of floating point numbers.
.Machine #Check

#Outcome is true if we round the compared floating numbers to < 16 decimal places.
x1 <- 1/3
x2 <- 1/4
if( round((x1 - x2),10) == round(1/12,10) ){
  print("Teacher was true")
} else{
  print("Teacher was lied")
}

derivative <- function(x,epsilon){
  res <- ((x + epsilon) - x) / epsilon 
  return(res)
}

derivative(x = 100000, epsilon = 10^-15)
#Obtained value is zero
#Real value is one
#Reason for difference is cancellation in the numerator. try a larger epsilon.
derivative(x = 100000, epsilon = 10^-5)
#Yeah its ok, starts numerically to break down at order diff of 16

myvar <- function(x){
  n <- length(x)
  sumsqrd <- sum(x^2)
  sqrdsum <- sum(x)^2
  res <- 1/(n - 1) * (sumsqrd - 1/n * sqrdsum)
  return(res)
}

testvec <- rnorm(10000,10^8,1)
diff <- rep(0,10000)
for(i in 1:10000){
  diff[i] <- myvar(testvec[1:i]) - var(testvec[1:i])
}
plot(1:10000,diff,pch="+",main="Dependence of Y_i on i",xlab="i",ylab="Y_i")

#Myvar seems to be smaller than var. 

#head(data)
X <- as.matrix(data[,-c(1,102)])
#head(X)
Y <- as.matrix(data[,102,drop=FALSE])
A <- t(X) %*% X
b <- t(X) %*% Y
solveres <- solve(A,b) # can not solve the system is ill-conditioned
rcond(A)

datascaled <- scale(data)

#head(data)
Xscaled <- as.matrix(datascaled[,-c(1,102)])
#head(X)
Yscaled<- as.matrix(datascaled[,102,drop=FALSE])
A <- t(Xscaled) %*% Xscaled
b <- t(Xscaled) %*% Yscaled
solveres <- solve(A,b) #now it works. scaling appears to improve rcond a little
rcond(A)
