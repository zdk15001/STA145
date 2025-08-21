# Define parameters for sample size calculation based on margin of error
Z_c_star <- 2  # Critical value for 95% confidence interval
sigma <- 5.0  # Estimated standard deviation for job creation
ME <- 1  # Desired margin of error

# Calculate the required sample size using the formula n = (Z_c_star * sigma / ME)^2
n_required <- (Z_c_star * sigma / ME)^2
print(n_required)