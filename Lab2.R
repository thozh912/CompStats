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
  print(pred)
  predMSE <- mean((pred - frame[,4])^2)
  print(c("The MSE of loess fit is: ",signif(predMSE,4)))
  return(predMSE)
}
pars <- list(X=train$Day,Y=train$LMR,Xtest=test$Day,Ytest=test$LMR)
res <-myMSE(0.1,pars)

lambvec <- seq(from=0.1,to=40,by=0.1)
resvec <- c()
for(i in 1:length(lambvec)){
  resvec <- c(resvec,myMSE(lambvec[i],pars))
}
plot(lambvec,resvec,main="predicted MSE vs lambda",xlab ="lambda",ylab = "MSE")
resvec[which.min(resvec)] # Minimal MSE at lambda = 11.7 (until 12.2)
# 117 iterations


op1<-optimize(myMSE,c(0.1,40),pars=pars, tol= 0.01) # 18 iterations required
op2 <- optim(35,myMSE,method = "BFGS",pars=pars) # 3 iterations but it is worse value

load("data.RData")
muhat <- sum(data)/100
paste("Maximum log-likelihood mean estimator:",round(muhat,6))
sigmasquarehat <- sum((data - muhat)^2) / 100
sigmahat <- sqrt(sigmasquarehat)
paste("Maximum log-likelihood std. deviation estimator:",round(sigmahat,6))



