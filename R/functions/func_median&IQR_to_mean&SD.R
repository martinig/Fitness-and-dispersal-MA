# function for getting mean and sd from median, IQR, and sample size

mean_sd_from_iqr <- function(n, med, iqr) {
	a <- med - (iqr/2) 
	b <- med + (iqr/2)
  mean_val <- (a + med + b)/3
  sd_val <- iqr / 1.35
  return(list(mean = mean_val, sd = sd_val))
}

# estimation:
n <- 145
med <- 29
iqr <- 24

result <- mean_sd_from_iqr(n, med, iqr)
cat("Mean:", result$mean, "\n")
cat("Standard Deviation:", result$sd, "\n")

# estimation:
n <- 78
med <- 28
iqr <- 20.5

result <- mean_sd_from_iqr(n, med, iqr)
cat("Mean:", result$mean, "\n")
cat("Standard Deviation:", result$sd, "\n")
