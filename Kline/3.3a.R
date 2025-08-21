# Load necessary library
library(ggplot2)

# Define Bill Size data
Bill_Size <- c(22, 15, 10, 35, 40, 55, 30, 60, 45, 33, 25, 48, 37, 43, 55, 60)

# Define Tip based on a beta (slope) of 0.2 and add random noise
set.seed(123)  # For reproducibility
Tip <- 0.2 * Bill_Size + rnorm(length(Bill_Size), mean = 0, sd = 1.5)  # Adding noise to simulate real data

# Combine into a data frame
data <- data.frame(Bill_Size = Bill_Size, Tip = Tip)

# calc mean
mean(data$Bill_Size)
mean(data$Tip)

# Create scatter plot
ggplot(data, aes(x = Bill_Size, y = Tip)) +
  geom_point() +
  labs(title = "Relationship Between Bill Size and Tip Amount",
       x = "Bill Size ($)",
       y = "Tip Amount ($)") +
  coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +  # show 0 on both axes
  theme_minimal()

# linear model
lm <- lm(Bill_Size ~ Tip)