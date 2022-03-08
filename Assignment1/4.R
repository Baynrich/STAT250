beta_dist.pdf <- function(x, alph = 3, bet = 2) {
  return((x**(alph-1) * (1-x)**(bet-1)) / (beta(alph, bet)))
}

beta_dist.pdf(0.5)

# Use normal dist with exp=0.5 and var=0.5 to sample candidates
# Set M=4 to contain beta_dist
n <- 1000
samples <- vector(mode="list", length=n)
i <- 1
while(i <= n){
  s <- rnorm(1, mean=0.5, sd=0.5)
  p <- beta_dist.pdf(s) / (4*dnorm(s, mean=0.5, sd=0.5))
  x = runif(1)
  # If we accept sample, add sample to samples
  if(x < p) {
    samples[i] <- s
    i <- i + 1
  }
}
