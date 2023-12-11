# function to convert estimate from linear function to r
​
​est =
se =
n1 =
n2 =

# estimate
est_se <- function(est, se, n1, n2){ # n2 = higher/larger group
  n12 <- n1 + n2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  t <- est/se
  r_pb <- t/sqrt(t^2+ n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  r_b #r_b = r
}
​
​est =
se =
N =

#adjusting for continuous n and assumed balanced design
est_se_b<- function(est, se, N){ # n2 = higher/larger group
  n12 <- N
  n1 <- N/2
  n2 <- N/2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  t <- est/se
  r_pb <- t/sqrt(t^2+ n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  r_b #r_b = r
}