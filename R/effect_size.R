# function to calculate effect sizes
# Zr - correlation
# there is always n

effect_size <- function(m1, m2, sd1, sd2, n1, n2, n, 
                        est , se, p_val, direction, method) # 12 arguments
  {
  
  if(method == "mean_method"){
  
    h <- n/n1 + n/n2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    s_pool <- sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n - 2) )
    j <- 1 - (3 / (4*n - 9))
    d <- ((m2 - m1) / s_pool) * j
    r_pb <-  d / sqrt(d^2 + h)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r    
    
  }else if(method == "count_method"){
    
    h <- n/n1 + n/n2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    s1 <- sqrt(m1)
    s2 <- sqrt(m2)
    s_pool <- sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2) / (n - 2) )
    j <- 1 - (3 / (4*n - 9))
    d <- ((m2 - m1) / s_pool) * j
    r_pb <-  d / sqrt(d^2 + h)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r     
    
  }else if(method == "percent_method1"){
    
    h <- n/n1 + n/n2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    m1 <- m1/100
    m2 <- m2/100
    s1 <- 1/sqrt(8)
    s2 <- 1/sqrt(8)
    m1 <- asin(sqrt(m1/100))
    m2 <- asin(sqrt(m2/100))
    s_pool <- sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2) / (n - 2) )
    j <- 1 - (3 / (4*n - 9))
    d <- ((m2 - m1) / s_pool) * j
    r_pb <-  d / sqrt(d^2 + h)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r 
    
  }else if(method == "percent_method2"){
    
    h <- n/n1 + n/n2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    m1 <- m1/100
    m2 <- m2/100
    sd1 <- sd1/100
    sd2 <- sd2/100
    s1 <- 1/sqrt(sd1^2/(4*m1*(1-m1)))
    s2 <- 1/sqrt(sd2^2/(4*m2*(1-m2)))
    m1 <- asin(sqrt(m1/100))
    m2 <- asin(sqrt(m2/100))
    s_pool <- sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2) / (n - 2) )
    j <- 1 - (3 / (4*n - 9))
    d <- ((m2 - m1) / s_pool) * j
    r_pb <-  d / sqrt(d^2 + h)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r 
    
  }else if(method == "proportion_method1"){
    
    h <- n/n1 + n/n2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    s1 <- 1/sqrt(8)
    s2 <- 1/sqrt(8)
    m1 <- asin(sqrt(m1))
    m2 <- asin(sqrt(m2))
    s_pool <- sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2) / (n - 2) )
    j <- 1 - (3 / (4*n - 9))
    d <- ((m2 - m1) / s_pool) * j
    r_pb <-  d / sqrt(d^2 + h)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r 
    
  }else if(method == "proportion_method2"){
    
    h <- n/n1 + n/n2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    s1 <- 1/sqrt(sd1^2/(4*m1*(1-m1)))
    s2 <- 1/sqrt(sd2^2/(4*m2*(1-m2)))
    m1 <- asin(sqrt(m1/100))
    m2 <- asin(sqrt(m2/100))
    s_pool <- sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2) / (n - 2) )
    j <- 1 - (3 / (4*n - 9))
    d <- ((m2 - m1) / s_pool) * j
    r_pb <-  d / sqrt(d^2 + h)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r 
    
  }else if(method == "t_method"){
    
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    #t <- est/se
    r_pb <- est/sqrt(est^2+ n -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r
    
  }else if(method == "F_method1"){
    
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    #t <- est/se
    r_pb <- sqrt(est)/sqrt(est + n -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r_b = r_b*(direction)
    r <- r_b
  
  }else if(method == "F_method2"){
    
    n1 <- n/2
    n2 <- n/2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    r_pb <- sqrt(est)/sqrt(est + n -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r_b = r_b*(direction)
    r <- r_b
    
  }else if(method == "p_method1"){
    
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    t <- qt(1 - p_val, n - 2)
    r_pb <- t/sqrt(t^2 + n12 -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r_b <- r_b*(direction)
    r <- r_b
    
  }else if(method == "p_method2"){
    
    n1 <- n/2
    n2 <- n/2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    t <- qt(1 - p_val, n - 2)
    r_pb <- t/sqrt(t^2 + n12 -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r_b <- r_b*(direction)
    r <- r_b
    
  }else if(method == "correlation_method1"){
    
    r <- est
    
  }else if(method == "correlation_method2"){
    
    r <- 2*sin((pi/6)*est)
    
  }esle {
    # ask April - estimate always have just n not n1 and n2
    
    n1 <- n/2
    n2 <- n/2
    #h <- n12/n1 + n12/n2
    p <- n1/n12 # prop for n1
    q <- n2/n12 # prop for n2
    t <- est/se
    r_pb <- t/sqrt(t^2+ n -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r
  
    }

  Zr <- atanh(r) # r = correlation
  VZr <- 1 /(n - 3)
  
  invisible(data.frame(yi = Zr , vi = VZr))
  
}





# try to use pmap
# 
# # Load the purrr package
# library(purrr)
# 
# # Define a function that adds two numbers
# add_func <- function(x, y) {
#   return(x + y)
# }
# 
# # Define a list of two vectors
# list1 <- list(c(1, 2, 3), c(4, 5, 6))
# 
# # Use pmap to apply the function over the list
# result <- pmap(list1, add_func)
# 
# # Print the result
# print(result)

# or mapply
# 
# add_func <- function(x, y) {
#   return(x + y)
# }
# 
# # Define two vectors
# vec1 <- c(1, 2, 3)
# vec2 <- c(4, 5, 6)
# 
# # Use mapply to apply the function over the vectors
# result <- mapply(add_func, vec1, vec2)
# 
# # Print the result
# print(result)

# t_method: must have df (typically n-2)

## count_method: when there is the mean, but no error
## mean_method: mean and sd

# F_method1: sample size for each group (chi-square here too)
# F_method2: sample size overall only
# percent_method1: no error around percentage
# percent_method2: percentage has error
# proportion_method1: no error around proportion
# proportion_method2: proportion has error
# correlation_method1: pearson's correlation or unspecified correlation type
# correlation_method2: spearman rank correlation
# p_method1: sample size for each group must have direction
# p_method2: sample size overall only must have direction
# estimate_method1: linear regression with estimate and SE