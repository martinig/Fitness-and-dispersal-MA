# function to calculate effect sizes
# Zr - correlation
# there is always n

effect_size <- function(m1, m2, sd1, sd2, n1, n2, n, # 12 arguments
                        est , se, p_val, direction, method){

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
    
  }else if(method == "t_method1"){
    

    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    r_pb <- est/sqrt(est^2 + n - 2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r
    
  }else if(method == "t_method2"){
    
    n1 <- n/2
    n2 <- n/2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    r_pb <- est/sqrt(est^2 + n - 2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r
    
  }else if(method == "F_method1"){
    
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
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
    r_pb <- t/sqrt(t^2 + n -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r_b <- r_b*(direction)
    r <- r_b
    
  }else if(method == "p_method2"){
    
    n1 <- n/2
    n2 <- n/2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    t <- qt(1 - p_val, n - 2)
    r_pb <- t/sqrt(t^2 + n -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r_b <- r_b*(direction)
    r <- r_b
    
  }else if(method == "correlation_method1"){
    
    r <- est
    
  }else if(method == "correlation_method2"){
    
    r <- 2*sin((pi/6)*est)
    
  }else if(method == "estimate_method1"){
    
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    t <- est/se
    r_pb <- t/sqrt(t^2+ n -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r
  
  }else if(method == "estimate_method2"){
    
    n1 <- n/2
    n2 <- n/2
    p <- n1/n # prop for n1
    q <- n2/n # prop for n2
    t <- est/se
    r_pb <- t/sqrt(t^2+ n -2)
    r_b <- r_pb*(sqrt(p*q)/dnorm(qnorm(p)))
    r <- r_b #r_b = r
  
  } 

  Zr <- atanh(r) # r = correlation
  VZr <- 1 /(n - 3)
  
  data.frame(yi = Zr , vi = VZr)
  
}

