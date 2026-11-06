# Load necessary library
library(ggplot2)

# Define Bill Size data
bill_size <- c(22, 15, 10, 35, 40, 55, 30, 60, 45, 33, 25, 48, 37, 43, 55, 60)

# Define Tip based on a beta (slope) of 0.2 and add random noise
set.seed(123)  # For reproducibility
tip <- 0.2 * bill_size + rnorm(length(bill_size), mean = 0, sd = 1.5)  # Adding noise to simulate real data

# Combine into a data frame
data <- data.frame(bill_size = bill_size, tip = tip)

# calc mean
mean(data$bill_size)
mean(data$tip)

# Create scatter plot
ggplot(data, aes(x = bill_size, y = tip)) +
  geom_point() +
  labs(title = "Relationship Between Bill Size and Tip Amount",
       x = "Bill Size ($)",
       y = "Tip Amount ($)") +
  coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +  # show 0 on both axes
  theme_minimal()

# linear model
lm <- lm(tip ~ bill_size)
summary(lm)
