# Load necessary library
library(ggplot2)
library(gridExtra)

# Set the seed for reproducibility
set.seed(123)

# Generate independent normal distributions representing social science measures
n <- 10000  # Sample size
education_scores <- rnorm(n, mean = 60, sd = 10)  # Education test scores (mean=60, sd=10)
income_levels <- rnorm(n, mean = 50000, sd = 15000)  # Income levels (mean=50000, sd=15000)
political_ideology <- rnorm(n, mean = 50, sd = 20)  # Political ideology scale (mean=50, sd=20)

# Compute the linear combination
combined_measure <-  education_scores + 
   income_levels + 
   political_ideology

# Visualization 1: Histograms of the Independent Normal Distributions
p1 <- ggplot(data.frame(education_scores), aes(x = education_scores)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "lightblue", color = "black", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(education_scores), sd = sd(education_scores)), color = "red", size = 1) +
  labs(title = "Histogram of Education Scores", x = "Scores", y = "Density") +
  theme_minimal()

p2 <- ggplot(data.frame(income_levels), aes(x = income_levels)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "lightgreen", color = "black", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(income_levels), sd = sd(income_levels)), color = "red", size = 1) +
  labs(title = "Histogram of Income Levels", x = "Income", y = "Density") +
  theme_minimal()

p3 <- ggplot(data.frame(political_ideology), aes(x = political_ideology)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "lightcoral", color = "black", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(political_ideology), sd = sd(political_ideology)), color = "red", size = 1) +
  labs(title = "Histogram of Political Ideology Scores", x = "Ideology Score", y = "Density") +
  theme_minimal()

# Visualization 2: Linear Combination Resulting in a Normal Distribution
p4 <- ggplot(data.frame(combined_measure), aes(x = combined_measure)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightgray", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(combined_measure), sd = sd(combined_measure)), color = "blue", size = 1) +
  labs(title = "Linear Combination of Independent Distributions", x = "Combined Measure", y = "Density") +
  theme_minimal()

# Visualization 3: Density Plot of All Distributions
p5 <- ggplot() +
  geom_density(data = data.frame(education_scores), aes(x = education_scores, color = "Education Scores"), size = 1) +
  geom_density(data = data.frame(income_levels), aes(x = income_levels, color = "Income Levels"), size = 1) +
  geom_density(data = data.frame(political_ideology), aes(x = political_ideology, color = "Political Ideology Scores"), size = 1) +
  geom_density(data = data.frame(combined_measure), aes(x = combined_measure, color = "Combined Measure"), size = 1.5, linetype = "dashed") +
  scale_color_manual(values = c("Education Scores" = "blue", "Income Levels" = "green", 
                                "Political Ideology Scores" = "red", "Combined Measure" = "black")) +
  labs(title = "Density Plot of Individual Distributions and Combined Measure",
       x = "Value", y = "Density", color = "Distributions") +
  theme_minimal()

# Arrange the plots
grid.arrange(p1, p2, p3, p4, ncol = 2)
