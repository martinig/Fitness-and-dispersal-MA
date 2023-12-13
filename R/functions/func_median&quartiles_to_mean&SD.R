# function for getting mean and sd from median, quartiles and sample size

get_mean_sd <- function(median, q1, q3, n){
  sd <- (q3 - q1) / (2 * (qnorm((0.75 * n - 0.125) / (n + 0.25)))) # sd
  mean <- (median + q1 + q3)/3 # mean
  c(mean, sd)
}

median=7329
q1=187
q3=17430
n=51

mean_sd= get_mean_sd(median, q1, q3, n)
mean_sd

