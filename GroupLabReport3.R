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
listofcities
listofcitysizes
hist(listofcitysizes,breaks = 50,main="Histogram of Swedish city populations,chosen cities",
     xlab="Population",ylab = "No. of cities")
hist(data[,2],breaks=100,main="Histogram of Swedish city populations, all cities",
     xlab="Population",ylab = "No. of cities")
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

hist(ucdf, xlim =c(-4,4), main = "Histogram of 10000 random numbers")
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
c
#acceptance/rejection
newu <- runif(5000, 0, 1)

newucdf <- doubleexp(ucdf)
norm <- dnorm(ucdf, 0, 1)

rate <- norm / (newucdf * c)

choosennumb <- c()
i <- 1
while(length(choosennumb) < 2000 ){
  if(newu[i] <= rate[i]){
   choosennumb <- c(choosennumb, ucdf[i] )
  }
  i <- i + 1
}

hist(choosennumb, main = "Histogram of accept-reject method N(0,1) numbers")
hist(rnorm(2000, 0, 1), main = "Histogram of rnorm() N(0,1) numbers")
#rejection rate R
rejrate <- (i - 2000) / 2000
rejrate
#expected rejection
ER <- 1 - 1 / c
ER
## NA
