# conversion CI to SD
# I need a function to convert CI to SD using t distribution

ci_to_sd <-function(upper, lower, n){
    # upper: upper limit of CI
    # lower: lower limit of CI
    # n: sample size
    # return: SD
    se <- (upper - lower) / (2 * qt(0.975, n-1))
    sd <- se*sqrt(n)
    return(sd)
}

# testing code for ci_to_sd
ci_to_sd(upper = 5.44, lower = 3.54, n = 157)
ci_to_sd(upper = 15.27, lower = 3.53, n = 4)


ci_to_sd(upper = 0.23, lower = 0.15, n = 105)
ci_to_sd(upper = 0.34, lower = 0.11, n = 19)