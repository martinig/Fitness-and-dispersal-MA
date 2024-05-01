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
ci_to_sd(upper = 2.11, lower = 1.60, n = 837)
ci_to_sd(upper = 2.61, lower = 1.64, n = 264)


ci_to_sd(upper = 2.33, lower = 2.02, n = 913)
ci_to_sd(upper = 3.49, lower = 2.03, n = 276)