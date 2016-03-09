target <- function(x){
  result <- x^5 * exp(-x)
  return(result)
}

g <- seq(1, 100, 0.2)
plot(g,target(g), type = "l", ylab = "y", xlab = "x",xlim=c(-1,20))
library(coda)
x <- rep(0, 600)

mhlnor <- function(start){
  x[1] <- start
for(i in 1:length(x)){
  Y <- rlnorm(1, meanlog = x[i], sdlog = 1)
  u <- runif(1,0,1)
  c <- (target(Y)*dlnorm(x[i], Y, 1)) / (target(x[i])*dlnorm(Y, x[i], 1))
  alpha <- min(1, c)
  if(u <= alpha){
    x[i + 1] <- Y
  }else{
    x[i + 1] <- x[i]
  }
}
  return(x)
}

lndist <- mhlnor(3)
plot(lndist, type = "l")
hist(lndist, main = "Histogram of the samples obtained with the log-normal", xlab = "",breaks=20)
mhchi <- function(start){
  x[1] <- start
for(i in 1:length(x)){
  Y <- rchisq(1, floor(x[i] + 1))
  u <- runif(1,0,1)
  c <- (target(Y)*dchisq(x[i], floor(Y + 1))) / (target(x[i])*dchisq(Y, floor(x[i] + 1)))
  alpha <- min(1, c)
  if(u <= alpha){
    x[i + 1] <- Y
  }else{
    x[i + 1] <- x[i]
  }
}
  return(x)
}

chdist <- mhchi(30)
plot(chdist, type = "l")
hist(chdist, main = "Histogram of the samples obtained with the Chi-square", xlab = "")
startpoints <- seq(1, 10, 1)
X <- data.frame(mhchi(3))
for(i in 1:10){
  X[,i] <- mhchi(startpoints[i])
}


library(coda)
f=mcmc.list()
for (i in 1:10) f[[i]]=as.mcmc(X[,i])
gelman.diag(f)
round(sum(lndist) / length(lndist),3)
round(sum(chdist) / length(chdist),3)
load("chemical.RData")
plot(X,Y,main="measured concentration of chemical vs day",xlab="day",ylab="measured conc")
mu0 <- rep(0,length(Y))

gibbs_sampler <- function(start,length = 1000){
  n <- length(start)
  result <- matrix(data = 0, nrow = (length + 1), ncol = n)
  result[1,] <- start
  turn <- 2
  repeat{
    if(turn > (length + 1)){
      break
    }
    result[turn,1] <- rnorm(1, mean = (Y[1] + result[(turn - 1),2]) / 2, sd = sqrt(0.2 / 2))
    for(j in 2:(n - 1)){
      result[turn,j] <- rnorm(1, 
          mean = ( Y[j] + result[turn,(j - 1)] + result[(turn - 1),(j + 1)]) / 3,
          sd = sqrt(0.2 / 3))
    }
    result[turn,n] <- rnorm(1, mean = (Y[n] + result[turn,(n - 1)]) / 2, sd = sqrt(0.2 / 2))
    turn <- turn + 1
  }
  return(result)
}
expctedvalu <- function(matr){
  output <- rep(0,ncol(matr))
  for(j in 1:ncol(matr)){
    output[j] <- mean(matr[,j])
  }
  return(output)
}
res <- gibbs_sampler(start = mu0)

res2 <- expctedvalu(res)
plot(X,Y,pch="+",col = "red",main= expression(paste("Y values and Expected value of ", mu)))
lines(X,res2,col="darkgreen")
legend("bottomright",c("Y values",expression(paste("E[",mu,"]"))),pch = c("+",""),lty = c(NA,1),
       col = c("red","darkgreen"))
plot(1:1001,res[,50],type="l",main=expression(paste("Traceplot of ",mu[50])),
     xlab = "no. of iteration",ylab = expression(mu[50]))
## NA
