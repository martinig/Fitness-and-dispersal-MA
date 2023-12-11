#function to convert median and min/max value to mean and SD

calculate_mean_sd <- function(n, med, min, max) {
  mean_val <- (2 * med + (max + min)) / 4
  sd_val <- (max - min) / 4
  return(list(mean = mean_val, sd = sd_val))
}

#calculation
n <- 20
med <- 9930
min_val <- 1048
max_val <- 17239

result <- calculate_mean_sd(n, med, min_val, max_val)
cat("Mean:", result$mean, "\n")
cat("Standard Deviation:", result$sd, "\n")

# calculation
n <- 51
med <- 7329
min_val <- 187
max_val <- 17430

result <- calculate_mean_sd(n, med, min_val, max_val)
cat("Mean:", result$mean, "\n")
cat("Standard Deviation:", result$sd, "\n")







