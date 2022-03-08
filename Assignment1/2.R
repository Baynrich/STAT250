n <- 10000
total <- 0

for (i in 1:n){
  points <- 0
  
  for (j in 1:10){
    spinval = runif(1, min = 0, max = 360)
    if(spinval >= 0 && spinval < 90){
      points <- points - 1
    }
    if(spinval >= 90 && spinval < 180){
      points <- points + 2
    }
    if(spinval >= 180 && spinval <= 360){
      points <- points + 1
    }
  }
  if(points < 0){
    total <- total + 1
  }
}
prob <- total / n
prob