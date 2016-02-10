data <- read.csv2("mortality_rate.csv")
#head(data)
data[,3] <- log(data$Rate)
names(data)[3] <- "LMR"
n = dim(data)[1]
set.seed(123456)
id = sample(1:n, floor(n * 0.5))
train = data[id,]
test = data[-id,]

myMSE <- function(lambda,pars){
  lel <- unlist(pars)
  frame <- data.frame(X = lel[1:68],Y = lel[69:136], Xtest = lel[137:204], Ytest = lel[205:272])
  model1 <- loess(Y ~ X, data = frame, enp.target = lambda)
  pred <- predict(model1, newdata = frame[,3])
  #print(pred)
  predMSE <- mean((pred - frame[,4])^2)
  #print(c("The MSE of loess fit is: ",signif(predMSE,4)))
  return(predMSE)
}
pars <- list(X=train$Day,Y=train$LMR,Xtest=test$Day,Ytest=test$LMR)
res <-myMSE(0.1,pars)

lambvec <- seq(from=0.1,to=40,by=0.1)
resvec <- c()
for(i in 1:length(lambvec)){
  resvec <- c(resvec,myMSE(lambvec[i],pars))
}
plot(lambvec,resvec,main="fitted model MSE vs lambda",xlab ="lambda",ylab = "MSE")
paste("minimum MSE is:",round(resvec[which.min(resvec)],3))
paste("Corresponding optimal value of lambda is:",round(lambvec[which.min(resvec)],3))
op1<-optimize(myMSE,c(0.1,40),pars=pars, tol= 0.01) # 18 iterations required
op1
op2 <- optim(35,myMSE,method = "BFGS",pars=pars) # 3 iterations but it is worse value
op2
load("data.RData")
muhat <- sum(data)/100
paste("Maximum log-likelihood mean estimator:",round(muhat,6))
sigmasquarehat <- sum((data - muhat)^2) / 100
sigmahat <- sqrt(sigmasquarehat)
paste("Maximum log-likelihood std. deviation estimator:",round(sigmahat,6))
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
paste("BFGS method without gradient specified")
oplog

oplog2 <- optim(c(0,1), minusloglike, method = "CG")
paste("Conjugate gradient method without gradient specified")
oplog2

oploggrad <- optim(c(0,1), fn = minusloglike, method = "BFGS", gr = gradient)
paste("BFGS method with gradient")
oploggrad

oplog2grad <- optim(c(0,1), minusloglike, method = "CG", gr = gradient)
paste("Conjugate gradient method with gradient specified")
oplog2grad
## NA
