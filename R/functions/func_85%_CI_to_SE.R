# conversion 85% CI to SE
# I need a function to convert 85% CI to SE

ci85_to_se <-function(upper, lower, n){
    # upper: upper limit of CI
    # lower: lower limit of CI
    # n: sample size
    # return: SE
    se_value<-(upper - lower) / (2 * qt(0.925, n-1))
    return(se_value)
}

upper_ci <- 0.29  # Replace with your upper CI value
lower_ci <- 0.07  # Replace with your lower CI value
sample_size <- 104  # Replace with your sample size

se_result <- ci85_to_se(upper_ci, lower_ci, sample_size)
se_result
