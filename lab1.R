set.seed(27)

#### Eg 2
muX <- c(1, 2)
muX
SigmaX <- matrix(c(1, 0.9, 0.9, 1), nrow = 2, ncol = 2)
SigmaX

par(mfrow = c(2, 2))
## Choleski method to generate MVN
rmvn.Choleski <- function(n, mu, Sigma) {
  # generate n random vectors from MVN(mu, Sigma) dimension is inferred from
  # mu and Sigma
  d <- length(mu)
  C <- chol(Sigma) # Choleski factorization of Sigma
  Z <- matrix(rnorm(n * d), nrow = n, ncol = d) # generate n*d samples from N(0,1) and matrix it
  X <- Z %*% C + matrix(mu, n, d, byrow = TRUE)
  X
}

benchmark.choleski <- function() {
  for (i in 1:1000){
    X <- rmvn.Choleski(1000, muX, SigmaX)
  }
}
time_choleski <- system.time({benchmark.choleski()})
print(time_choleski)

#plot(X, xlab = "x", ylab = "y", pch = 20, main ="Choleski factorization")


### Eigen decomposition method to generate MVN


rmvn.eigen <- function(n, mu, Sigma) {
  # generate n random vectors from MVN(mu, Sigma) dimension is inferred from
  # mu and Sigma
  d <- length(mu)
  ev <- eigen(Sigma, symmetric = TRUE)
  lambda <- ev$values #eigenvalues
  V <- ev$vectors #eigenvectors
  C <- V %*% diag(sqrt(lambda)) %*% t(V)
  Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
  X <- Z %*% C + matrix(mu, n, d, byrow = TRUE)
  X
}

X <- rmvn.eigen(1000, muX, SigmaX)
plot(X, xlab = "x", ylab = "y", pch = 20, main ="Eigen decomposition")

library(MASS)
library(mvtnorm)

plot(rmvnorm(1000,muX,SigmaX),xlab = "x", ylab = "y", pch = 20, main ="rmvnorm")
plot(mvrnorm(1000,muX,SigmaX),xlab = "x", ylab = "y", pch = 20, main ="mvrnorm")




## # generating n datapoints from a mixture of K Gaussians with dimensions d
# k  : the respective datapoint classes, take k = 3 as example her
# mu : kxd matrix with means
# sig: kxdxd matrix with dxd covariate matrices


muK <- matrix(c(4.0,4.0,
                5.0,5.0,
                6.5,  5), ncol=2, byrow=T)
muK

sigsK <- array(rep(NA,2*2*3), c(2,2,3))  # 3D matrix
sigsK[,,1] <- matrix(c(1, 0.9, 0.9,1), nrow=2, byrow=TRUE)
sigsK[,,2] <- matrix(c(0.5,0,0,0.5), nrow=2, byrow=TRUE)
sigsK[,,3] <- matrix(c(2,0.2,0.2,2), nrow=2, byrow=TRUE)
sigsK

probk <- c(0.05,0.85,.1) # mixing coeffs ## probability for each class
classes <- sample(1:3, n, replace=TRUE, prob=probk)  ## understand sample function
classes


### function to generate from mixtured Gaussian
gen.mix <- function(n, k, mu, sig) { 
  library(MASS)
  
  d <- length(mu[1,])  # number of dimensions
  result <- matrix(rep(NA,n*d), ncol=d)
  colnames(result) <- paste0("X",1:d)
  
  for(i in 1:n) {
    result[i,] <- mvrnorm(1, mu = mu[k[i],], Sigma=sig[,,k[i]])
  }
  
  result
}

##generate example with k=3, d=2
benchmark.genmix <- function(){
  for (i in 1:1000){
    n <- 500
    mydata <- gen.mix(n, classes, muK, sigsK)
  }
}
print(system.time(benchmark.genmix()))

## compare the number and spread of the data from three classes in plot

par(mfrow = c(1, 1))
plot(mydata, col=c("orange","green","red")[classes], xlab="X1", ylab="X2", pch=19)

