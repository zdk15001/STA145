
# STEP 1: Generate your sample
sample(1:300, 30, replace = FALSE)


# STEP 2: Enter your data as an object called "MARCUS"
marcus <- c(1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1)

# Count the number of 1's
count_ones <- sum(marcus == 1)

# Print the result
print(count_ones)


# Step 3: Calculate CIs
prop.test(20,30, correct=FALSE)


