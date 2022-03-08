n <- 10000
total <- 0
current_pi <- vector(mode = "list", length = n)

for (i in 1:n){
  x <- runif(1, min=-1, max=1)
  y <- runif(1, min=-1, max=1)
  if (sqrt(x**2 + y**2) <= 1){
    total <- total + 1
  }
  current_pi[i] <- 4 * total / i
}

current_pi[10000]
x_ax = c(1:n)
plot(x_ax, current_pi)