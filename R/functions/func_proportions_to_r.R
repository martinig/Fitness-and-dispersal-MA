#function to convert proportion to r value 
# calculate_SMD takes two proportions (p1 and p2) as input, converts them into logit scale using the logit function from the car package, and then calculates the SMD using the provided formula.

library(car) # Load the car package for the logit function 

calculate_smd <- function(p1, p2, n1, n2) {
 
 # Step 1: Convert proportions to logit scale
  logit_p1 <- logit(p1)
  logit_p2 <- logit(p2)
  
  # Step 2: Calculate the Standardized Mean Difference (SMD)
  smd <- (logit_p1 - logit_p2) / (pi / sqrt(3))
  
  # Step 3: Adjust SMD for sample sizes
  smd_adjusted <- smd * sqrt((n1 + n2) / (n1 * n2))
  
  return(smd_adjusted)
}

#example 1:
n1=1
n2=13
p1=1
p2=0.9230769231

result <- calculate_smd(p1, p2, n1, n2) #gives me a warning message
print(result) #0.6743569

#example 2:
n1=256
n2=157
p1=0
p2=0.4840764331

result <- calculate_smd(p1, p2, n1, n2) #also gives me a warning message
print(result) #-0.201187









