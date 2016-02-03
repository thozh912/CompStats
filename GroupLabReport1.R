x1 <- 1/3
x2 <- 1/4
if(x1-x2 == 1/12){
  print("Teacher said true")
}else{
  print("Teacher lied")
}
x1 <- 1/3
x2 <- 1/4
if(round(x1-x2, 4)==round(1/12, 4)){
  print("Teacher said true")
}else{
  print("Teacher lied")
}
derivative <- function(x,epsilon){
  res <- ((x + epsilon) - x) / epsilon 
  return(res)
}

derivative(x = 100000, epsilon = 10^-15)
derivative(x = 100000, epsilon = 10^-5)
set.seed(12345)
x <- rnorm(10000, mean = 10^8, sd = 1)
myvar <- function(x){
  v <- 1/(length(x) - 1) * (sum(x^2) - 1/length(x) * (sum(x)^2))
  return(v)
}

y <- c()
for(i in 1:10000){
  sub <- x[1:i]
  y[i] <- myvar(sub) - var(sub)
}

plot(1:10000, y, xlab = "Dimension of x", ylab = "Y")
for(i in 2:10){
print(myvar(x[1:i]))
}
for(i in 2:10){
print(var(x[1:i]))
}  
## X <- as.matrix(data[,-c(1,102)])
## #head(X)
## Y <- as.matrix(data[,102,drop=FALSE])
## A <- t(X) %*% X
## b <- t(X) %*% Y
## solveres <- solve(A,b) # can not solve the system is ill-conditioned
## rcond(A) # rcond returns 7.25 * 10^(-17)
## datascaled <- scale(data)
## 
## #head(data)
## Xscaled <- as.matrix(datascaled[,-c(1,102)])
## #head(X)
## Yscaled<- as.matrix(datascaled[,102,drop=FALSE])
## A <- t(Xscaled) %*% Xscaled
## b <- t(Xscaled) %*% Yscaled
## solveres <- solve(A,b) #now it works. scaling appears to improve rcond a little
## rcond(A) # 7.0 * 10^(-14)
## solveres
## NA
