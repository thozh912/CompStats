#purposes include
#How implement without overflow and other problems?
#How generate r.v.,several correlated variables,variables from multivariate dist?
#How compute M.L.
#How compute confints for dists when formulas not helping?

10^200 * 10^200 #+Inf, overflow
10^400 / 10^400 # Inf/Inf = NaN
10^-200 / 10^200 # 0, too many decimal places
0 * 10^400 # NaN, small * large
.Machine

#Float arithmetic XD

# A + X = B + X but B != A
# A + X = X but A + Y != Y
# A + X = X but X - X != A
#Not necessarily associative or distributive

#When summing data

#either sort the numbers ascending and sum in that order
#or sum numbers of similar mags pairwise, continue until one number


#Linreg Normal equations t(X) * X * beta = t(X) * Y

# abs(delta(x)) / abs(x) < condnum *  abs(delta(b)) / abs(b)

# condnum = 2-norm(A) * 2-norm(solve(A)) = kappa(A, exact = TRUE)
#for 2 norm is ratio of largest to smallest eigenvalue of A
# kappa_2(t(A) * A) = kappa_2(A) ^ 2 > kappa_2(A)

# Cholesky decomp pos def sym matr A -> L * t(L) 
#solve L * y = b
#solve t(L) * x = y

#####Lab1#####
library(XLConnect)
wb = loadWorkbook("tecator.xls")
data = readWorksheet(wb,sheet = "data" ,header = TRUE)

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

#Myvar has catastrophic cancellation problems or magnitude problems or sth.

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
#####Lab1 - END #####

#1-dim minimization by golden ratio alpha = (1 - sqrt(5)) / 2
#Choose interval end points [a,b]
#Choose step <- alpha * (b - a)
# c <- a + step, d <- b - step
# if f(d) > f(c) pick new interval [a,c]
# if f(d) < f(c) pick new interval [c,b]

#Multidim optimization is about choosing step dir p and size alpha
# steepest descent p = (in theory) -grad(f(x)) / abs(grad(f(x)))
# Newtons method p = - solve(hessian(f(x))) %*% grad(f(x)) and hessian have to be p.d.

#find step size alpha as global minimum along p or a sufficient decrease.

#BFGS (quasi-Newton) iterations less time than Newtons since no matrix inversion
# non-linear CG much faster than steepest slower than Q/N or N but less memory
#####Lab2#####
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
plot(lambvec,resvec,main="fitted model MSE vs lambda",
     type = "l",xlab ="lambda",ylab = "MSE")
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
#####Lab2 - END #####

#Linear congruential RNG x_{k + 1} = (a*x_{k} + c) mod m
#x_{0} is seed, divide nums by m if U(0,1) needed
#Will loop with certain period
#transform to X = a + U * (b - a) or X = [n * U] + 1 (discrete unif dist)

#inverse CDF
# X is random var with CDF F_{X}.
#Set Y = F^-1_{X}(U) where U is U(0,1)
#Y has same prob dist as X

#normally distributed variables
#assume THETA from U(0,2 * pi) and D from U(0,1)
#then X_1 and X_2 def as sqrt(-2 * log(D))*(cos/sin)(THETA) are indep norm distr.

#Accept/reject method
#generate Y from dist density f_{Y}
# Generate U from U(0,1)
# c * f_{Y}(x) >= f_{X}(x) for all x (majorizing > target)
# if U <= f_{X}(Y) / (c * f_{Y}(Y)) take Y else reject and generate new U
#variables taken will be from f_{X}

#generate from N(mu,SIGMA)
# take i.i.d. N(0,1) seq X =( X_1,X_2..X_n)
#compute cholesky factor A where A * t(A) = SIGMA
#compute Y as mu + A * X

#####Lab 3 #####
## Assignment 1
library(XLConnect)
wb = loadWorkbook(paste0(getwd(),"/population.xls"))
data = readWorksheet(wb, sheet = "Table", header = TRUE)
data <- data[-which(nchar(data$Statistics.Sweden) == 2),]
data <- data[-(1:4),c(2,4)]
data[,2] <- as.numeric(data[,2])

propcitypicker <- function(cities){
  citypopulations <- data[match(cities,data[,1]),2]
  totalcitypop <- sum(citypopulations)
  binborders <- cumsum(citypopulations / totalcitypop)
  roll <- runif(1)
  if( roll < binborders[1]){
    return(cities[1])
  }
  for(i in 1:(length(cities)-1)){
    if( binborders[i] < roll && roll <= binborders[i+1]){
      return(cities[i+1])
    }
  }
}

cities <- data[,1]
listofcities <-c()
counter <- 1
while(counter <= 20){
  chosencity <- propcitypicker(cities)
  cities <- cities[-match(chosencity,cities)]
  listofcities <- c(listofcities,chosencity)
  counter <- counter + 1
}

listofcitysizes <- data[match(listofcities,data[,1]),2]
hist(listofcitysizes,breaks = 50,main="Histogram of Swedish city populations,chosen cities",
     xlab="Population",ylab = "No. of cities")
hist(data[,2],breaks=100,main="Histogram of Swedish city populations, all cities",
     xlab="Population",ylab = "No. of cities")

## Assignment 2
unifinput <- runif(10000)
norminput <- rnorm(10000)
DEnumbers <- function(x){
  res <- c()
  for(i in 1:length(x)){
    if(x[i] < 0.5){
      res <- c(res,log(2*x[i]))
    } else{
      res <- c(res,-log(2*(1 - x[i])))
    }
  }
  return(res)
}
DEoutput <- DEnumbers(unifinput)
hist(DEoutput,main="Histogram of inverse CDF method DE(0,1) numbers")

doubleexp <- function(x){
  return(1/2 * exp(-abs(x)))
}
xordered <- seq(from=-3,to=3,by=0.001)
plot(xordered,dnorm(xordered),type="l",ylim=c(-0.1,0.6), ylab="f(x)",xlab ="x",
     main= "Std. Normal and Double exponential functions")
lines(xordered,doubleexp(xordered))
c <- dnorm(xordered)[which.max(dnorm(xordered)/doubleexp(xordered))] / 
  doubleexp(xordered)[which.max(dnorm(xordered)/doubleexp(xordered))]

unifnumbers <- runif(8000)
DEnumbers <- sample(DEoutput,8000)
normnumbers <- sample(norminput,8000)
j <- 1

acceptednumbers <-c()

repeat{
  if(unifnumbers[j] <= dnorm(DEnumbers[j]) / (c * doubleexp(DEnumbers[j]))){
    acceptednumbers <- c(acceptednumbers,DEnumbers[j])
  }
  if(length(acceptednumbers) >= 2000){
    break
  }
  j <- j + 1
}
paste("Theoretical Rejection rate:",round(1 - 1/c,3))
paste("Actual Rejection rate:",(j - 2000) / j)
hist(acceptednumbers,main="Histogram of accept-reject method N(0,1) numbers")
hist(sample(norminput,2000),main="Histogram of rnorm() N(0,1) numbers",xlab="rnormnumbers")
#####Lab 3-End #####

#to generate general multivariate dist variables by MCMC M-H alg
# init chain to X_{0} and set t = 0
# gen candidate point Y from q(.|X_{t})
# if U =< alpha(X_{t},Y) then set X_{t+1} = Y, else set X_{t+1} = X_{t}
# alpha(X_{t},Y) = min{1, pi(Y)q(X_{t}|Y)/pi(X_{t})q(Y|X_{t})}
# where pi is the dist you want to draw from and q is the proposal dist
# if q(Y|X) = q(abs(X-Y)) then alpha(X_{t},Y) = min{1, pi(Y)/pi(X_{t})}
#chain will converge to pi(X) (and at that jump around exploring that density)
#observe burn-in and seed

#gibbs sampler uses conditional distributions, needs rngs that sample from
#univeriate distributions
# generate starting point X_{0} = (X_{0,1}...X_{0,d})
# generate point X_{t+1,1} from f(X_{t+1,1} | X_{t,2} = x_{t,2}... X_{t,n} = x{t,n})
# generate point X_{t+1,2} from f(X_{t+1,2} | X_{t+1,1} = x_{t+1,1}... X_{t,n} = x{t,n})
# generate point X_{t+1,n} from f(X_{t+1,n} | X_{t+1,1} = x_{t+1,1}... X_{t+1,n-1} = x{t+1,n-1})
#increment t and repeat
#convergence slow

#If one can decompose f(x) as g(x)p(x) where p(x) is a p.d.f.
#then estimate of theta = int f(x) dx is sum(g(x_{i})) / m, m elements i  1 up to m
#Variance of integral estimation sum((g(x_{i}) - mean(g(x)))^2) / (m * (m - 1))
#biased variance estimator
#####Lab 4#####
library(coda)
phi <- function(x){
  result <- x^5 * exp(-x)
  return(result)
}
#xes <- seq(from=0, to=100,by=0.01)
#plot(xes,1/100 * phi(xes),xlim=c(-1,30),type="l")
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
     main ="Chain of M-H alg. with log normal proposal distribution",
     xlab = "iteration")
# You can pick any start as long as its not too large above 5
hist(lognormMCMC(10000,3,1),breaks=100, 
     main=c("Histogram of M-H alg. output","log normal proposal distribution"))
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
     xlab ="iteration") # You can pick any start as long as its not too large above 5
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
#####Lab 4-End #####
#build a histogram of test statistics, use hist as distrib of t under H_0
#power = 1 - type 2 error (failure to reject false null hypothesis)
#to compute power generate data from H_a and compute percent of correct rejections. thats all.

#if not know dist of data, and want do hypothesis testing, permutation test or bootstrap

#perm test create permutations g_1...g_B of group variable
#eval test stat on each (or a subset of group variables)
#evaluate p-value phat = #{T(b) >= T_original} / B , two-sided #{|T(b)| >= |T_original|} / B

#non-para boots
#assume X \sim F(X,w) , F, w unknown
# Estimate hat(w) from data
#generate D_1 = (X*_1,...X*_n) by sampling with replacement
#repeat that B times
# The distribution of w is given by T(D_1)...T(D_B)

# if F is known then generate the bootstraps from F(X,hat(w))

#bootstrap percentile method

#####Lab 5#####
library(bootstrap)
library(boot)
library(XLConnect)
wb = loadWorkbook("lottery.xls")
wb2 = loadWorkbook("prices1.xls")
data = readWorksheet(wb,sheet = "Sheet1" ,header = TRUE)
#data is changed around alot in data$Draft_No. reset often.
data2 = readWorksheet(wb2,sheet = "Sheet1" ,header = TRUE)

plot(data$Day_of_year,data$Draft_No,xlab="X=Day_of_year",ylab="Y=Draft_No",pch="+",
     main="Scatterplot of Draft_No vs Day_of_year")
#It looks random
polyfit <- loess(Draft_No ~ Day_of_year,data=data)
plot(data$Day_of_year,data$Draft_No,xlab="X=Day_of_year",ylab="Y=Draft_No",pch="+",
     main="Scatterplot of Draft_No vs Day_of_year")
lines(polyfit$fitted)
bigT <- function(dat,index){
  dat2 <- dat[index,]
  poly <- loess(Draft_No ~ Day_of_year, data = dat2)
  smallest <- which.min(poly$fitted)
  largest <- which.max(poly$fitted)
  TEE <- (poly$fitted[largest] - poly$fitted[smallest]) / 
    (dat2$Day_of_year[largest] - dat2$Day_of_year[smallest])
  return(TEE)
}
set.seed(-3447)
boot1 <- boot(data,bigT,R=2000)
boot1
signif1 <- mean(boot1$t > 0) 
paste("P-value of T greater than zero outcome: ",signif1) # P-level
hist(boot1$t,breaks = 100)

originalT <- boot1$t0
permfun <- function(data,B){
  n <- dim(data)[1]
  permvec <-rep(0,B)
  for(i in 1:B){
    rando <- sample(1:n,n)
    dat2 <- data.frame(day = rando, draft = data$Draft_No)
    poly <- loess(draft ~ day, data = dat2)
    smallest <- which.min(poly$fitted)
    largest <- which.max(poly$fitted)
    TEE <- (poly$fitted[largest] - poly$fitted[smallest]) / 
      (dat2$day[largest] - dat2$day[smallest])
    permvec[i] <- TEE
  }
  poly <- loess(Draft_No ~ Day_of_year, data = data)
  smallest <- which.min(poly$fitted)
  largest <- which.max(poly$fitted)
  originalstatistic <- (poly$fitted[largest] - poly$fitted[smallest]) / 
    (data$Day_of_year[largest] - data$Day_of_year[smallest])
  prop <- mean(permvec > originalstatistic)
  return(prop)
}
set.seed(-3447)
pval2 <- permfun(data,B= 2000)
paste("p-value from permutation testing: ",pval2)
alpha <- 0.1

nonrandgen <- function(data,alpha){
  nonrandY <- numeric(366)
  for(i in 1:366){
    beta <- rnorm(1,183,10)
    nonrandY[i] <- max(0,min(alpha * data$Day_of_year[i] + beta, 366))
  }
  return(nonrandY)
}
#look, above alpha = 0.5 we get the ceiling alot.
# what is the point?
#nvm, forgot about the permutation of days

nonrandY <- nonrandgen(data,alpha)
data$Draft_No <- nonrandY
pval3 <- permfun(data,B = 200)
paste("p-value from permutation testing non-random Draft_No, alpha = 0.1: ",pval3)
alpha <- seq(from = 0.1, to = 10 , by = 0.1)

pvals <- rep(0,length(alpha))
for(j in 1:length(alpha)){
  nonrandY <- nonrandgen(data,alpha[j])
  data$Draft_No <- nonrandY
  pvalcurr <- permfun(data, B = 200)
  pvals[j] <- pvalcurr
}
paste("Crude estimate of Power of the test statistic T: ", round(mean(pvals < 0.05),2))
hist(data2$Price,breaks = 20,main="Histogram of home prices in Albuquerque 1993",
     xlab="most likely 100s of USD")
paste("mean price of home in Albuquerque 1993: ",round(mean(data2$Price),3),"hundred USD")
m <- length(data2$Price)

meanstat <- function(data,ind){
  datar <- data[ind,]
  res <- mean(datar$Price)
  return(res)
}
set.seed(-3447)
boot2 <- boot(data2,meanstat, R = 2000)
#hist(boot2$t,main="Histogram of bootstrap mean house prices",xlab = "mean house price")
plot(boot2)

#bias correction, here mean could be any test statistic

biascorrectedest <- 2 * mean(data2$Price) - mean(boot2$t)
paste("Bootstrap mean house price bias-correction estimate",
      round(mean(data2$Price) - mean(boot2$t),3)," hundred USD")
paste("Bias-corrected bootstrap estimate of mean house price: ",
      round(biascorrectedest,3)," hundred USD")
varstat <- function(data,ind){
  datar <- data[ind,]
  res <- var(datar)
  return(res)
}
set.seed(-3447)
boot3 <- boot(boot2$t,varstat, R = 2000)
plot(boot3)
paste("Bootstrap estimate of variance of mean house price: ",
      round(mean(boot3$t),3)," hundred USD squared")
varbyjack <- jackknife(data2$Price,mean)
Tstarjs <- m * rep(mean(data2$Price),m) - (m-1) * varbyjack$jack.values
jackknifedT <- mean(Tstarjs) # this is the same as mean of house price...
jackvarofmeans <- sum((Tstarjs-jackknifedT)^2) / (m*(m-1)) # is the same as varbyjack$jack.se^2
paste("Estimate of variance of mean home price by jackknife: ",
      round((varbyjack$jack.se)^2,3)," hundred USD squared")
set.seed(-3447)
confintsboot <- boot.ci(boot2, type=c("norm","perc", "bca"))
normci <- confintsboot$normal
percci <- confintsboot$percent
bcaci <- confintsboot$bca
confintinfos <- data.frame(method = c("Normal Approximation",
                                      "bootstrap percentile method","BCa method"),
                           lower = c(normci[2],percci[4],bcaci[4]),upper = c(normci[3],percci[5],bcaci[5]),
                           length = c(normci[3] - normci[2],percci[5] - percci[4],bcaci[5]-bcaci[4]),
                           midpoint = 1/2 * c(normci[3] + normci[2],percci[5] + percci[4],bcaci[5] + bcaci[4]))
confintinfos #as you go down the list the interval creeps higher,
#also normal approx midpoint is same as bias corrected bootstrap est.
#####Lab 5-End #####
#slides
#####Lab 6#####
## Assignment 1

#In this assignment we try to perform one-dimensional maximization,
#using the genetic algorithm specified in the instructions,
#on the objective function:
$$f(x) = \frac{x^2}{e^x}-2e^{\frac{-9sin(x)}{x^2+x+1}}$$
  
  
```{r,echo=FALSE,message=FALSE,warning=FALSE}
data <- read.csv("physical.csv")
n <- dim(data)[1]
#head(data)
objfunc <- function(x){
  res <- x^2/exp(x) - 2 * exp(-9 * sin(x) / (x^2 + x + 1))
  return(res)
}
crossover <- function(x,y){
  return((x + y) / 2)
}
mutate <- function(x){
  res <- x^2 %% 30
  return(res)
}
genalgfunc <- function(maxiter = 100 , mutprob = 0.5){
  X <- seq(0,30,0.01)
  plot(X,objfunc(X), main = c("Genetic Algorithm maximization",
                              paste("iterations =",maxiter,
                                    " p(mutation) =",mutprob)),
       type="l")
  X <- seq(0,30,5)
  Values <- objfunc(X)
  points(X,Values,col = "blue",cex = 1, pch = 1,lwd = 2)
  count <- 0
  maxvals <- c()
  repeat{
    if(count == maxiter){
      break
    }
    parents <- sample(X,2)
    victim <- order(Values)[1]
    kid <- crossover(parents[1],parents[2])
    if(runif(1) < mutprob){
      kid <- mutate(kid)
    }
    X[victim] <- kid
    Values <- objfunc(X)
    maxvals <- c(maxvals,max(Values))
    count <- count + 1
  }
  points(X,Values,col = "red",cex = 1, pch = 4,lwd = 2)
  legend("topright",legend = c("Initial points","Final points"),
         lty = c(0,0), col= c("blue","red"), pch = c(1,4), lwd = c(2,2))
  paste("Final maximum value achieved:", round(maxvals[maxiter],4))
}

#par(mfrow = c(1,1))
#maxiter <- c(10,20,30)
#mutprob <- c(0.1,0.2,0.3)
#for(i in 1:2){
#  for(j in 1:2){
#    genalgfunc(maxiter[i],mutprob[j])
#  }
#}

set.seed(-3456)
genalgfunc( maxiter = 100, mutprob = 0.5)
```

#As we can see, setting `maxiter = 100` and `p(mutation) = 0.5` 
#gives rather good results, in the sense that the set of final points
#all are located close to the true maximum. The setting `maxiter = 100`
#and `p(mutation) = 0.9` produces similar results but the final points 
#are more spread out along the function curve, due to the higher mutation rate.

```{r,echo=FALSE}
set.seed(-3456)
genalgfunc( maxiter = 100, mutprob = 0.9)
```

#To contrast, we can try setting `maxiter = 10` and `p(mutation) = 0.1`, 
#which often results in the final points being located not far from where
#they started.

```{r,echo=FALSE}
set.seed(-3456)
genalgfunc( maxiter = 10, mutprob = 0.1)
```

#We can infer that the genetic algorithm used requires more than 10 iterations
#to be effective and that the mutation rate should be moderately high for best
#results.

## Assignment 2

We plot $Z$ and $Y$ as time series vs $X$.

```{r,echo=FALSE,fig.align='center'}
plot(data$X,data$Z, type = "l", col = "blue",
     main=c("Time series plot of Y and Z against X"),xlab = "X",ylab = "value")
lines(data$X,data$Y,col = "red")
legend("topright",legend = c("Y","Z"),
       lty = c(1,1), col= c("red","blue"))
```
#We see that both are $Z$ and $Y$ stochastic processes,
#with a high degree of correlation. There are some $Z$ values missing
#at $X = 3,8,10$ and $Z$ values tend to be larger than $Y$ values for a given $X$.
#The values appear to be decreasing with increasing $X$.

#We are going to estimate $Z$ and $Y$ using the EM-algorithm, existing data and the models

$$Y_i \sim Exp(\dfrac{X_i}{\lambda}) \text{ and } Z_i \sim Exp(\dfrac{X_i}{2\lambda})$$
  
#Where $\lambda$ is a unknown parameter to be estimated.

#We start by writing down the likelihood functions for $Z$ and $Y$.

$$L(Y|\lambda) = \dfrac{\prod_{i=1}^{n} X_i}{\lambda^n} \exp{\left(- \dfrac{\sum_{i=1}^{n} Y_i X_i}{\lambda}\right)}$$
  
$$L(Z|\lambda) = \dfrac{\prod_{i=1}^{n} X_i}{2^n\lambda^n} \exp{\left(- \dfrac{\sum_{i=1}^{n} Z_i X_i}{2\lambda}\right)} = \dfrac{\prod_{i=1}^{n} X_i}{2^n\lambda^n}\exp{\left(- \dfrac{\sum_O Z_i X_i}{2\lambda}-\dfrac{\sum_M Z_i X_i}{2\lambda}\right)}$$
  
#where O is the set of indices for the observed values of Z and M is
#the set of indices for the missing values of Z.

#Then, we multiply the likelihoods together and take the logarithm.

$l(Y, Z|\lambda) =\log{\dfrac{(\prod_{i=1}^{n} X_i)^2}{2^n\lambda^{2n}} - \dfrac{\sum_{i=1}^{n} Y_i X_i}{\lambda}- \dfrac{\sum_O Z_i X_i}{2\lambda}-\dfrac{\sum_M Z_i X_i}{2\lambda}}$
  
#At this point the E-step can be done.
#We are going to set every missing $Z_{i}$ to its Expected value
#given $X_{i}$ and the last lambda value, $\lambda_{t}$.

$$E[Z_{i} | X_{i},\lambda_{t}] = \dfrac{2\lambda_{t}}{X_{i}} $$
  
#Due to the exponential distribution of $Z_{i}$.

$$E(l(Y, Z|\lambda)) =\log{\dfrac{(\prod_{i=1}^{n} X_i)^2}{2^n\lambda^{2n}}- \dfrac{\sum_{i=1}^{n} Y_i X_i}{\lambda}- \dfrac{\sum_O Z_i X_i}{2\lambda}-\dfrac{|M|\lambda_t}{\lambda}}$$
  
#Where $|M|$ is the number of missing $Z$.
#For the M-step, we have to compute the derivative
#with respect to $\lambda$ and put it equal to zero. 

#Doing this we get:
  
  $$\lambda_{t+1}=\dfrac{\sum_{i=1}^{n} Y_i X_i}{2n}+\dfrac{\sum_O Z_i X_i}{4n}+\dfrac{|M|\lambda_t}{2n}$$
  
#We implement this algorithm in R where we start with $\lambda_{0} = 100$
#and stop when one step yields less than 0.001 change in $\lambda_{t}$.
#Then we plot the expected values of $Z$ and $Y$ given our model and this
#$\lambda$ value.

```{r,echo=FALSE}
plot(data$X,data$Z, type = "l", col = "blue",
     main=c("Time series plot of Y and Z against X",
            "with the EM-alg. expected values"),xlab = "X",ylab = "value")
lines(data$X,data$Y,col = "red")
legend("topright",legend = c("Y","Z","EY","EZ"),
       lty = c(1,1,1,1), col= c("red","blue","green","black"))

Zmiss <- which(is.na(data$Z))
numbofmiss <- length(Zmiss)
Zobs <- which(!is.na(data$Z))
Zobsvals <- data$Z[Zobs]

helper <- function(lambdaold){
  res <- 1/ (2 * n) * ( sum(data$Y * data$X) + sum(data$Z[Zobs] * data$X[Zobs]) / 2 +
                          numbofmiss * lambdaold )
  return(res)
}

runner <- function(startlambda){
  oldlambda <- startlambda
  newlambda <- 0
  iterashuns <- 0
  repeat{
    if(abs(newlambda - oldlambda) < 0.001){
      break
    }
    oldlambda <- newlambda
    newlambda <- helper(oldlambda)
    iterashuns <- iterashuns + 1
  }
  
  return(newlambda)
}
paste("Optimal Lambda value: ",runner(100))
paste("No of EM alg. iterations run: ",4)
lambdaconverge <- runner(100)

lines(data$X,lambdaconverge / data$X ,col = "green")
lines(data$X,2 * lambdaconverge / data$X ,col = "black")
```

#We see that the expected values fit the data well.
#####Lab 6-End #####

#####heres the parametric bootstrap in action 

rng <- function(data2,mle){
  data1 = data.frame(MET = data2$MET, EX = data2$EX)
  n = length(data2$EX)
  data1$EX = rnorm(n,predict(mle, newdata=data1),
                   sd(data$EX-predict(mle, newdata=data1)))
  return(data1)
}

f1 = function(data1){
  res <- tree(EX ~ MET,data1, control = tree.control(dim(data1)[1],minsize=2))
  bestrestree <- prune.tree(res,best=3)
  expenditures <- predict(bestrestree, newdata=data)
  return(expenditures)
}

f2 = function(data1){
  res <- tree(EX ~ MET,data1, control = tree.control(dim(data1)[1],minsize=2))
  bestrestree <- prune.tree(res,best=3)
  expenditures <- rnorm(dim(data)[1],predict(bestrestree, newdata=data),
                        sd(resid))
  return(expenditures)
}

set.seed(12345)
bootobj2 <- boot(data, statistic= f1,R=1000,
                 mle=bestregtree,ran.gen=rng,sim="parametric")
set.seed(12345)
bootobj3 <- boot(data,statistic= f2,R=1000,
                 mle=bestregtree,ran.gen=rng,sim="parametric")
#plot(bootobj2)
confintvs2 <- envelope(bootobj2)
confintvs3 <- envelope(bootobj3)

#####end

##### here is svd decomp in action
powerlist <-list()
validlist <- list()
wvalues <- list()
for(i in 1:6){
  powerlist[[i]] <- train$Protein^i
  validlist[[i]] <- valid$Protein^i
}

MSEtrain <-c()
MSEvalid <-c()


for(j in 1:6){
  jpt <- 1
  X <- rep(1,dim(train)[1])
  Xvalid <- rep(1,dim(valid)[1])
  while(jpt <= j){
    X <- cbind(X,powerlist[[jpt]])
    Xvalid <- cbind(Xvalid,validlist[[jpt]])
    jpt <- jpt + 1
  }
  
  #Using SVD decomp to find least squares as the conditionnumber is too high 
  
  svdobj <- svd(X)
  #d <- diag(svdobj$d)
  z <- t(svdobj$u) %*% (train$Moisture)
  z <- z / svdobj$d
  wvalues[[j]] <- svdobj$v %*% z
  
  
  #wvalues <- solve(t(X) %*% X) %*% t(X) %*% (train$Moisture)
  
  trainpredicted <- X %*% wvalues[[j]]
  validpredicted <- Xvalid %*% wvalues[[j]]
  
  MSEtrain <- c(MSEtrain,sum(((train$Moisture) - trainpredicted)^2) / dim(train)[1])
  MSEvalid <- c(MSEvalid,sum(((valid$Moisture) - validpredicted)^2) / dim(valid)[1])
}
#####end 