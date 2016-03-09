#Assignment 1
library(XLConnect)
t <- loadWorkbook("population.xls")
population <-readWorksheet(t, sheet = "table", header = TRUE)







#Assignment 2
u <- runif(10000, 0, 1)

invfunc <- function(u){
  res <- c()
  for(i in 1:(length(u))){
if(u[i] < 1/2){
  invcdf <- log(2*u[i])
  res <- c(res, invcdf)
}else if(u[i] >= 1/2){
  invcdf <- -log(2 - 2*u[i])
  res <- c(res, invcdf)
}
  }
return(res)
}

ucdf <- invfunc(u)

hist(ucdf, xlim =c(-4,4))


# library(distr)
# d <- DExp(rate = 1)
# rd <- r(d)(10000)
# hist(rd, xlim =c(-4,4))

#2
x <- seq(from=-3, to=3,by=0.001)

#Laplace distribution
doubleexp <- function(x){
  result <- 1/2 * exp(-abs(x))
  return(result)
}

dedistr <- doubleexp(x)
ndistr <- dnorm(x, 0, 1)

#plot of Normal and Laplace together
plot(dnorm(x, 0, 1), type="l", ylim = c(0 , 0.6), ylab = "y", 
     main = c("Standard Normal density and", "Double Exponential density"))
lines(doubleexp(x), col = "red")
legend("topright", legend = c("Red line: Double Exponential density"))

#find the c value
diff <- ndistr - dedistr
m <- which.max(diff)
c <- ndistr[m]/dedistr[m]

#acceptance/rejection
newu <- runif(2000, 0, 1)

newucdf <- doubleexp(ucdf)
norm <- dnorm(ucdf, 0, 1)

rate <- norm / (newucdf * c)

choosennumb <- c()
for(i in 1:2000){
  if(newu[i] <= rate[i]){
   choosennumb <- c(choosennumb, ucdf[i] )
  }
}

hist(choosennumb, main = "Random number 
     from a standard Normal density ")
hist(rnorm(2000, 0, 1), main = "Random number 
     from a standard Normal density ")

#rejection rate
rejrate <- length(choosennumb) / 2000

#expected rejection
ER <- 1 / c




