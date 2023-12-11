# function for getting mean and sd from median, IQR, and sample size

mean_sd_from_iqr <- function(n, med, iqr) {
  mean_val <- med
  sd_val <- iqr / (2 * qnorm(0.75))
  return(list(mean = mean_val, sd = sd_val))
}

# estimation:
n <- 106
med <- 33
iqr <- 24

result <- mean_sd_from_iqr(n, med, iqr)
cat("Mean:", result$mean, "\n")
cat("Standard Deviation:", result$sd, "\n")

# estimation:
n <- 71
med <- 34
iqr <- 25

result <- mean_sd_from_iqr(n, med, iqr)
cat("Mean:", result$mean, "\n")
cat("Standard Deviation:", result$sd, "\n")
