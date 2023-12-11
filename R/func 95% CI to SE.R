# conversion 95% CI to SE
# I need a function to convert 95% CI to SE

ci_to_se <-function(upper, lower, n){
    # upper: upper limit of CI
    # lower: lower limit of CI
    # n: sample size
    # return: SE
    sd_value<-(upper - lower) / (2 * qt(0.975, n-1))
    #se_result<-sd_value/sqrt(n)
    return(sd_value)
}

upper_ci = 1.67 # Replace with your upper CI value
lower_ci = 0.679 # Replace with your lower CI value
sample_size = 124  # Replace with your sample size

se_result <- ci_to_se(upper_ci, lower_ci, sample_size)
se_result
