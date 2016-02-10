#Assignment 1
mortality <- read.csv2("mortality_rate.csv")

#1.1
mortality$LMR <- log(mortality$Rate)

n=dim(mortality)[1]
set.seed(123456)
id=sample(1:n, floor(n*0.5))
train=mortality[id,]
test=mortality[-id,]

#1.2 and 1.3
tr <- train[,1]
trY <- train[,3]
tes <- test[,1]
tesY <- test[,3]
li <- list(X = tr, Y = trY, Xtest = tes, Ytest = tesY)

lambda <- seq(0.1, 40, 0.1)

myMSE <- function(lambda, pars){
  X <- as.matrix(pars$X)
  Y <- as.matrix(pars$Y)
  Xtest <- as.matrix(pars$Xtest)
  Ytest <- as.matrix(pars$Ytest)
  MSE <- c()
  for(i in 1:(length(lambda))){
  lo <- loess(Y ~ X, enp.target = lambda[i]) 
  pred <- predict(lo, Xtest)
  MSE[i] <- (sum((pred - Ytest)^2) )/length(Ytest)
  }
  print(MSE)
  return(MSE)
}

res <- myMSE(lambda, li)

plot(lambda, res, ylab = "MSE", xlab = "Lambda")

lambda[which.min(res)] #optimal value of lambda
which.min(res) #number of evaluation to find the optimal value
res[234] #mse for the minimum

#1.4
op <- optimize(myMSE, c(0.1, 40), tol = 0.01, pars = li)
op
# 18 number of evaluations

#1.5
op2 <- optim(35, fn = myMSE, method = "BFGS", pars = li) #using Quasi-Newton method
op2
# 3 number of evaluations


##Assignment 2
#2.2
# $$L(\mu, \sigma^2, x_1, ..., x_100) = (2\pi\sigma^2)^{-100/2} exp(-\frac{1}{2\sigma^2} \sum_{j = 1}^100
## (x_j - \mu)^2)$$

## $$l(\mu, \sigma^2, x_1, ..., x_100) = - \frac{100}{2}ln(2\pi) - \frac{100}{2}ln(\sigma^2) - \frac{1}{2\sigma^2}
## \sum_{j = 1}^{100} (x_j - \mu)^2)$$

## $$\hat{\mu} = \frac{1}{100}\sum_{j = 1}^{100} x_j$$
## $$\hat{\sigma^2} = \frac{1}{100}\sum_{j = 1}^100 (x_j - \hat{\mu})^2$$


muhat <- sum(data)/100
sigmasquarehat <- sum((data - muhat)^2) / 100
sigmahat <- sqrt(sigmasquarehat)

#2.4
minusloglike <- function(pars){
  mu <- pars[1]
  sig <- pars[2]
  loglike <- - (-50*log(2*pi) - 50*log(sig^2) - 1/(2*sig^2)*sum((data- mu)^2))
  return(loglike)
}

gradient <- function(pars){
  mu <- pars[1]
  sig <- pars[2]
  c(-(1/sig^2*sum(data - mu)), -(-100/sig + 1/sig^3*sum((data- mu)^2)))
}


oplog <- optim(c(0,1), fn = minusloglike, method = "BFGS")
oplog

oplog2 <- optim(c(0,1), minusloglike, method = "CG")
oplog2

#specified the gradient
oploggrad <- optim(c(0,1), fn = minusloglike, method = "BFGS", gr = gradient)
oploggrad

oplog2grad <- optim(c(0,1), minusloglike, method = "CG", gr = gradient)
oplog2grad
