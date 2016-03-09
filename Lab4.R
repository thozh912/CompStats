library(coda)
load("chemical.RData")
plot(X,Y,main="measured concentration of chemical vs day",xlab="day",ylab="measured conc")

#p(Yvec|muvec) = Prod(p(Y_1|mu_1) * p(Y_2 | mu_2) ... * p(Y_n | mu_n))
#every of form 1/(sigma * sqrt(2 * pi)) * exp(-(Y[i] - mu[i])^2/(2 * sigma^2))
#p(muvec) = alot of normal dists multiplied together of form 
#1/(sigma * sqrt(2 * pi)) * exp(-(mu[i+1] - mu[i])^2/(2 * sigma^2)) <- n-1 of these since p(mu1) = 1

# posterior distribution for mu[i], i = 1 is prop to exp(-(Y[1] - mu[1])^2/(2 * sigma^2)) * 
# exp(-(mu[2] - mu[1])^2/(2 * sigma^2))

# posterior distribution for mu[i], i = anything but 1 or n is prop to 
#exp(-(Y[i] - mu[i])^2/(2 * sigma^2)) * exp(-(mu[i+1] - mu[i])^2/(2 * sigma^2)) *
#exp(-(mu[i] - mu[i-1])^2/(2 * sigma^2))

# posterior distribution for mu[i], i = n is prop to exp(-(Y[n] - mu[n])^2/(2 * sigma^2)) *
#exp(-(mu[n] - mu[n-1])^2/(2 * sigma^2))
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
