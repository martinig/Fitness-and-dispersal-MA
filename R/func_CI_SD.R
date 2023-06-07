# conversion CI to SD
# I need a function to convert CI to SD using t distribution

ci_to_sd <-function(upper, lower, n){
    # upper: upper limit of CI
    # lower: lower limit of CI
    # n: sample size
    # return: SD
    # SD = (upper - lower) / (2 * qt(0.975, n-1))
    return((upper - lower) / (2 * qt(0.975, n-1)))
}

# testing code for ci_to_sd
ci_to_sd(upper = 0.5, lower = 0.4, n = 10)
ci_to_sd(upper = 1, lower = -2, n = 100)
