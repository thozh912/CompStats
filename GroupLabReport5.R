library(bootstrap)
library(boot)
library(XLConnect)
wb = loadWorkbook("lottery.xls")
wb2 = loadWorkbook("prices1.xls")
data = readWorksheet(wb,sheet = "Sheet1" ,header = TRUE)
#data is changed around alot in data$Draft_No. reset often.
data2 = readWorksheet(wb2,sheet = "Sheet1" ,header = TRUE)

t <- loadWorkbook("lottery.xls")
lottery <-readWorksheet(t, sheet = "Sheet1", header = TRUE)

#1.1
plot(lottery$Day_of_year, lottery$Draft_No, ylab="Draft number", xlab = "Day of year",
     main = "Scatterplot of Draft number vs Day of year")

lo <- loess(Draft_No ~ Day_of_year, lottery)
pred <- predict(lo)

plot(lottery$Day_of_year, lottery$Draft_No, ylab="Draft number", xlab = "Day of year",
     main = "Scatterplot of Draft number vs Day of year", ylim = c(0, 400))
lines(lottery$Day_of_year, pred, col = "red")
legend("topright", c("expected response"),
       lty=c(1,1), lwd = c(2, 2), col = "red")
D <- 2000
stat <- numeric(D) 
n <- dim(lottery)[1]
set.seed(-3447)
for(d in 1:D){
  data1 <- lottery[sample(nrow(lottery), nrow(lottery), replace = TRUE), ]
  los <- loess(Draft_No~Day_of_year, data1)
  new_data1 <- data.frame(X = data1$Day_of_year, Y = los$fitted)
  mx <- which.max(new_data1$Y)
  mi <- which.min(new_data1$Y)
  stat[d] <-  (new_data1$Y[mx] - new_data1$Y[mi]) / (new_data1$X[mx] - new_data1$X[mi])
}

hist(stat, 50, main = "Histogram", xlab= "",breaks=100)
pval <- mean(stat>0) 
pval
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

#bias correction

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
print(confintsboot)
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
## NA
