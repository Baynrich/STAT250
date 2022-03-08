n <- 10000
samples <- vector(length = n)
i <- 1
while(i < n){
  poiss <- rpois(1, 10)
  Xt <- 0
  for (j in 1:poiss) {
    Xt <- Xt + rgamma(1, shape=1, rate=1)
  }
  samples[i] <- Xt
  i <- i+i
}

mean(samples)
var(samples)
