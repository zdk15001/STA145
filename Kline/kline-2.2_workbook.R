
## Project:  Class script for STA 145, Fall 2024, sampling theory
# Located:   Kline TCNJ Google Drive
# File Name: kline-2.2
# Date:      2024_9_23
# Who:       Zachary D. Kline

## Before running this script
# Step 1: Download starwars.csv and move to class working directory
# Step 2: Double check that the GSS

# Set Working Directory
setwd("H:/sta215_fall2024")

# Install "haven" packag
#install.packages("haven")

# Load "haven" package
library("haven")

# Load GSS 2022 as an object called "GSS2022"
GSS <- read_dta("GSS2022.dta")
starwars <- read.csv("starwars.csv")

## Central Limit Theorem
# mu or population average mass
mean(starwars$mass) ## Why doesn't this work??
mean(starwars$mass, na.rm = TRUE)

# take sample of 30 random observations from starwars dataset
starwars_sample1 <- starwars[sample(nrow(starwars), 30), ] 
mean(starwars_sample1$mass, na.rm = TRUE)

# create 10 sub-samples of size 30
starwars_sub_samples <- list() # create an empty list to store sub-samples

for (i in 1:10) {
  sub_sample <- starwars[sample(nrow(starwars), 30), ]
  starwars_sub_samples[[i]] <- sub_sample # store sub-sample in list
}

# calculate mean mass value for each sub-sample
starwars_means <- sapply(starwars_sub_samples, function(x) mean(x$mass, na.rm = TRUE))

# plot mean mass values
plot(starwars_means, main = "Mean Mass in Sub-Samples", xlab = "Sub-Sample", ylab = "Mean Mass")

# Calculate mean of means
mean_of_means <- mean(starwars_means)

# Plot mean mass values for each sub-sample
plot(starwars_means, main = "Mean Mass in Sub-Samples", xlab = "Sub-Sample", ylab = "Mean Mass")

# Add line for mean of means 
abline(h = mean_of_means, col = "red")

# calculate mean of samples
mean_of_samples <- mean(starwars_means)
print(mean_of_samples)

# histogram of starwars mean
hist(starwars_means)

# histogram of starwars population mean
hist(starwars$mass)

# remove problem and recreate simulation
starwars_nojabba <- starwars[!starwars$name=="Jabba Desilijic Tiure",]

starwars_sub_samples_nojabba <- list() # create an empty list to store sub-samples

for (i in 1:100) {
  sub_sample_nojabba <- starwars_nojabba[sample(nrow(starwars_nojabba), 30), ]
  starwars_sub_samples_nojabba[[i]] <- sub_sample_nojabba # store sub-sample in list
}

# calculate mean mass value for each sub-sample
starwars_means_nojabba <- sapply(starwars_sub_samples_nojabba, function(x) mean(x$mass, na.rm = TRUE))

# plot mean mass values
plot(starwars_means_nojabba, main = "Mean Mass in Sub-Samples", xlab = "Sub-Sample", ylab = "Mean Mass")

# Calculate mean of means
mean_of_means_nojabba <- mean(starwars_means_nojabba)

# Plot mean mass values for each sub-sample
plot(starwars_means_nojabba, main = "Mean Mass in Sub-Samples", xlab = "Sub-Sample", ylab = "Mean Mass")

# Add line for mean of means 
abline(h = mean_of_means_nojabba, col = "red")

# calculate mean of samples
mean_of_samples_nojabba <- mean(starwars_means_nojabba)
print(mean_of_samples_nojabba)

# histogram of starwars mean
hist(starwars_means_nojabba)


# hint: try the same exercise with GSS age
# create 1000 sub-samples of size 30
GSS_sub_samples <- list() # create an empty list to store sub-samples

# Hypothetical: Make missing age to be 200 years old
sum(is.na(GSS$age))
GSS_old <- GSS
GSS_old$age <- ifelse(is.na(GSS_old$age), 200, GSS_old$age)


for (i in 1:1000) {
  sub_sample <- GSS_old[sample(nrow(GSS_old), 30), ]
  GSS_sub_samples[[i]] <- sub_sample # store sub-sample in list
}

# calculate mean age value for each sub-sample
GSS_means <- sapply(GSS_sub_samples, function(x) mean(x$age, na.rm = TRUE))

# plot mean age values
plot(GSS_means, main = "Mean age in Sub-Samples", xlab = "Sub-Sample", ylab = "Mean age")

# Calculate mean of means
mean_of_means <- mean(GSS_means)

# Plot mean age values for each sub-sample
plot(GSS_means, main = "Mean age in Sub-Samples", xlab = "Sub-Sample", ylab = "Mean age")

# Add line for mean of means 
abline(h = mean_of_means, col = "red")

# calculate mean of samples
mean_of_samples <- mean(GSS_means)
print(mean_of_samples)

# histogram of GSS mean
hist(GSS_means)


# ttest
t.test(starwars_nojabba$mass)
t.test(GSS$age)