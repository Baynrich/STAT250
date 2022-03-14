samples <- c(4, 5, 6, 7, 7, 5, 5, 5)
mu <- 5

t_val <- qt(0.025, length(samples) - 1)
s_sum <- 0
for (sample in samples) {
  s_sum <- s_sum + (sample - mu)**2
}
S <- sqrt( (1 / (length(samples)-1)) * s_sum )
lower_bound <- mu - (t_val * (S / sqrt(length(samples))))
upper_bound <- mu + (t_val * (S / sqrt(length(samples))))

lower_bound
upper_bound