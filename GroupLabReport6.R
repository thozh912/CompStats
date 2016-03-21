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
genalgfunc( maxiter = 100, mutprob =0.9)
set.seed(-3456)
genalgfunc( maxiter = 10, mutprob =0.1)
physical <- read.csv2("physical.csv", sep = "," ,header = TRUE, stringsAsFactors = FALSE)

#2.1
physical$X <- as.numeric(physical$X)
physical$Y <- as.numeric(physical$Y)
physical$Z <- as.numeric(physical$Z)

plot(physical$X, physical$Y, type = "l", ylim = c(0, 35), xlab = "X", ylab="value",main = "Time series plot of Y and Z against X")
lines(physical$X, physical$Z, col="red")
legend("topright", c("Y vs X", "Z vs X"),
       lty=c(1,1), lwd = c(2.5, 2.5), col = c("black", "red"))
em <- function(Y, Z, X){ 
Zobs <- Z[!is.na(Z)]
Zmiss <- Z[is.na(Z)]
Xobs <- which(!is.na(Z))
n <- length(Z)
r <- length(Zmiss)
# Initial value
lambda <- 100
i <- 1

repeat{
  # E- step
  EY <- sum(Y*X) / 2
  EZo <- sum(Zobs*X[Xobs]) / 4
  EZm <- (r * lambda) / 2
  # M - step
  lambda1 <- (EY + EZm + EZo) / n
  # Stop if converged
  if ( abs(lambda1 - lambda) < 0.001) break
  lambda <- lambda1
  i <- i + 1
}
res <- data.frame(Iterations = i, Lambda = lambda)
return(res)
}


em_exp <- em(physical$Y, physical$Z, physical$X)

em_exp
plot(physical$X, physical$Y, type = "l", ylim = c(0, 35), xlab = "X", ylab ="value",main=c("Time series plot of Y and Z against X",
            "with the EM-alg. expected values"))
lines(physical$X, physical$Z, col="red")
lines(physical$X, em_exp$Lambda/physical$X, col="blue")
lines(physical$X, (2*em_exp$Lambda)/physical$X, col="green")
legend("topright", c("Y vs X", "Z vs X", "EY vs X", "EZ vs X"),
       lty=c(1,1), lwd = c(2.5, 2.5), col = c("black", "red", "blue", "green"))
## NA
