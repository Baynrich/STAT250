loc_mix <- function(x, P=0.75) {
  if(x < P){
    return(rnorm(1))
  }
  else {
    return(rnorm(1, 3, 1))
  }
}

n = 1000
samples = vector(mode="list", length=n)
i = 0
while(i < n){
  x <- runif(1)
  samples[i] <- loc_mix(x)
  i <- i + 1
}
hist(samples)