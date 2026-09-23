# ------------------------------------------------------------
# Sampling distribution of a proportion
# Data: marcus.csv (300 students; voted for Marcus or Rosanne)
# Treat the 300 students as the POPULATION.
# ------------------------------------------------------------

library(ggplot2)

# 1. Load the population ------------------------------------------------
# (Put the csv in your working directory, or use read.csv(file.choose()))
votes <- read.csv("marcus.csv")

dim(votes)
names(votes)
summary(votes)
head(votes)
table(votes$voted_for)

# 2. Recode into a dummy variable: 1 = Marcus, 0 = Rosanne --------------
votes$marcus <- ifelse(votes$voted_for == "Marcus", 1, 0)

# 3. Population proportion (the parameter, p) ---------------------------
# The mean of a 0/1 variable is the proportion of 1s
p <- mean(votes$marcus)
p

# 4. Draw ONE random sample and compute the sample proportion (p-hat) --
set.seed(145)   # makes the random draws reproducible
n <- 30         # sample size

one_sample <- sample(votes$marcus, size = n)
mean(one_sample)

# 5. Repeat that many times to build the sampling distribution ----------
reps <- 30

p_hats <- replicate(reps, mean(sample(votes$marcus, size = n)))

head(p_hats)

# 6. Describe the sampling distribution ---------------------------------
mean(p_hats)   # center: should be close to p
sd(p_hats)     # spread: the standard error (from the simulation)

# Note: Test changing reps here to see how number of samples affects sd

# Standard error from the formula for the parameter
sqrt(p * (1 - p) / n)

# The simulated SE comes out a little smaller than the formula because
# sample() draws WITHOUT replacement from only 300 students. The finite
# population correction accounts for that:
N <- nrow(votes)
sqrt(p * (1 - p) / n) * sqrt((N - n) / (N - 1))

# 7. Plot the sampling distribution -------------------------------------
sim <- data.frame(p_hat = p_hats)

ggplot(sim, aes(x = p_hat)) +
  geom_histogram(binwidth = 1 / n, center = 0,
                 color = "black", fill = "grey80") +
  geom_vline(xintercept = p, linewidth = 1) +
  labs(title = paste0("Sampling distribution of p-hat (n = ", n,
                      ", ", reps, " samples)"),
       x = "Sample proportion voting for Marcus (p-hat)",
       y = "Number of samples") +
  theme_minimal()

# 8. (Optional) How does sample size change the spread? -----------------
# Law of Large Numbers
sizes <- c(10, 50, 100)

sim_sizes <- data.frame(
  n     = rep(sizes, each = reps),
  p_hat = unlist(lapply(sizes, function(k)
            replicate(reps, mean(sample(votes$marcus, size = k)))))
)

# Standard error for each sample size
aggregate(p_hat ~ n, data = sim_sizes, FUN = sd)

ggplot(sim_sizes, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.05, center = 0,
                 color = "black", fill = "grey80") +
  geom_vline(xintercept = p, linewidth = 1) +
  facet_wrap(~ n, ncol = 1, labeller = label_both) +
  labs(x = "Sample proportion voting for Marcus (p-hat)",
       y = "Number of samples") +
  theme_minimal()
