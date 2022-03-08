pmf <- function(input){
  if (input < 0.1) {
    return(0)
  }
  if (input < 0.3) {
    return(1)
  }
  if (input < 0.5) {
    return(2)
  }
  if (input < 0.7) {
    return(3)
  }
  if (input < 1) {
    return(4)
  }
}

unifs <- runif(1000)
samples <- lapply(unifs, pmf)

zeros = sum(samples == 0)
zeros
ones = sum(samples == 1)
ones
twos = sum(samples == 2)
twos
threes = sum(samples == 3)
threes
fours = sum(samples == 4)
fours
