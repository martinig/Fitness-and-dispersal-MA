# conversion SE to SD

# Function to convert SE to SD
se_to_sd <- function(se) {
  sd <- se * sqrt(n)  # Assuming 'n' is the sample size
  return(sd)
}

# Example usage
se_value <- 47  # Replace with your SE value
n <- 28          # Replace with your sample size
sd_result <- se_to_sd(se_value)
sd_result