library(coda)
phi <- function(x){
  result <- x^5 * exp(-x)
  return(result)
}
xes <- seq(from=0, to=100,by=0.01)
plot(xes,1/100 * phi(xes),xlim=c(-1,30),type="l")
#lines(dlnorm(xes,),col=2)
#lines(dchisq(xes, df= 3),col=6)

lognormMCMC <- function(length,start,stddev){
  X <- c(start)
  for(i in 1:(length-1)){
    Y <- rlnorm(1,meanlog = X[i],sdlog = stddev)
    U <- runif(1)
    ratio <- phi(Y)/phi(X[i]) * 
      dlnorm(X[i], meanlog = Y)/dlnorm(Y, meanlog = X[i])
    alpha <- min(1,ratio)
    if(U <= alpha){
      X[i + 1] <- Y
    }else{
      X[i + 1] <- X[i]
    }
  }
  return(X)
}

plot(1:10000,lognormMCMC(10000,3,1),type="l",
     main ="Chain of M-H alg. with log normal proposal distribution") # You can pick any start as long as its not too large above 5
hist(lognormMCMC(10000,3,1),breaks=100, main=c("Histogram of M-H alg. output","log normal proposal distribution"))
paste("Mean value of lognormMCMC chain:",round(mean(lognormMCMC(10000,3,1)),3)) # not good.

MCMC <- function(length,start){
  X <- c(start)
  for(i in 1:(length-1)){
    Y <- rchisq(1,floor(X[i] + 1))
    U <- runif(1)
    ratio <- phi(Y)/phi(X[i]) * 
      dchisq(X[i], df = Y)/dchisq(Y, df = floor(X[i] + 1))
    alpha <- min(1,ratio)
    if(U <= alpha){
      X[i + 1] <- Y
    }else{
      X[i + 1] <- X[i]
    }
  }
  return(X)
}

plot(1:10000,MCMC(10000,4),type="l",
     main ="Chain of M-H alg. with chi-squared proposal distribution",
     xlab ="iterations") # You can pick any start as long as its not too large above 5
#Burn-in around 30 iterations
#well exploring the dchisq density
hist(MCMC(10000,4),breaks=100,main=c("Histogram of M-H alg. output","chi squared proposal distribution"))
paste("Mean value of MCMC chain:",round(mean(MCMC(10000,4)),3)) # This should be 6. it is close.

frame <- data.frame(MCMC(500,4))
for(i in 1:10){
  frame[,i] <- MCMC(500,i)
}
f = mcmc.list()
for(i in 1:10) f[[i]] = as.mcmc(frame[,i])
gelman.diag(f)

