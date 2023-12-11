# function to convert p-value to r

p_val = 
n1 =
n2 =
​
# p value (sign required)
#Don't actually need this function as we got test stats 
p_vals <- function(p_val, n1, n2, reverse = FALSE){ # n2 = higher/larger group
  n12 <- n1 + n2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  t <- qt(1 - p_val, n12 - 2)
  #t <- est/se
  r_pb <- t/sqrt(t^2 + n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  if(reverse == TRUE){
    r_b = r_b*(-1)}
  r_b
}
​
p_val = 
N = 
​
p_vals_b <- function(p_val, N, reverse = FALSE){ # n2 = higher/larger group
  n12 <- N
  n1 <- N/2
  n2 <- N/2
  n12 <- n1 + n2
  #h <- n12/n1 + n12/n2
  p <- n1/n12 # prop for n1
  q <- n2/n12 # prop for n2
  t <- qt(1 - p_val, n12 - 2)
  #t <- est/se
  r_pb <- t/sqrt(t^2 + n12 -2)
  
  r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
  if(reverse == TRUE){
    r_b = r_b*(-1)}
  r_b
}
