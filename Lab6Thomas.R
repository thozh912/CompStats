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
set.seed(-3456)
genalgfunc( maxiter = 100, mutprob = 0.9)
set.seed(-3456)
genalgfunc( maxiter = 10, mutprob = 0.1)
plot(data$X,data$Z, type = "l", col = "blue",
     main=c("Time series plot of Y and Z against X"),xlab = "X",ylab = "value")
lines(data$X,data$Y,col = "red")
legend("topright",legend = c("Y","Z"),
       lty = c(1,1), col= c("red","blue"))
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
  print(iterashuns)
  return(newlambda)
}
paste("Optimal Lambda value: ",runner(100))
lambdaconverge <- runner(100)

lines(data$X,lambdaconverge / data$X ,col = "green")
lines(data$X,2 * lambdaconverge / data$X ,col = "black")
## NA
