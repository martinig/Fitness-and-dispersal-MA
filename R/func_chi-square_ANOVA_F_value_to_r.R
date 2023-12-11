# function to convert chi-square, ANOVA, and F-statistic to r value
​
F_val=8.14
n1=1012
n2=103

# F values (sign required)
F_vals <- function(F_val, n1, n2, reverse = FALSE){ # m2 = higher/larger group
  n12 <- n1 + n2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  #t <- est/se
  r_pb <- sqrt(F_val)/sqrt(F_val + n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  if(reverse == TRUE){
  r_b = r_b*(-1)}
  r_b
}
​

​F_val=8.14
N=1012

#adjusting for continuous n and assumed balanced design
F_vals_b <- function(F_val, N, reverse = FALSE){
  n12 <- N
  n1 <- N/2
  n2 <- N/2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  #t <- est/se
  r_pb <- sqrt(F_val)/sqrt(F_val + n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  if(reverse == TRUE){
    r_b = r_b*(-1)}
  r_b
}
