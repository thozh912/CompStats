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
hist(DEoutput,main="Histogram of inverse CDF method DE(0,1) numbers",xlim=c(-7,7),xlab = "numbers" )
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
paste("Actual Rejection rate:",(j - 2000) / 2000)
hist(acceptednumbers,main="Histogram of accept-reject method N(0,1) numbers")
hist(sample(norminput,2000),main="Histogram of rnorm() N(0,1) numbers",xlab="rnormnumbers")
## NA
