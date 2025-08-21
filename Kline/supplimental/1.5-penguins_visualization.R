# If needed, install:
#install.packages(c("tidyverse", "palmerpenguins", "ggthemes"))

# load packages
library(tidyverse)
library(palmerpenguins)
library(ggthemes)  
theme_set(theme_minimal()) # my personally-preffered style

penguins <- penguins

## Bar Chart (categorical counts)
# Counts of penguin species
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar(na.rm = TRUE) +
  labs(title = "Counts by Species",
       x = "Species", y = "Count") 

## Histogram (one quantitative variable)
# Distribution of body mass (grams)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200, boundary = 0, na.rm = TRUE) +
  labs(title = "Body Mass Distribution",
       x = "Body mass (g)", y = "Number of penguins")

##Boxplot (quantitative by categorical)
# Body mass by species
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "Body Mass by Species",
       x = "Species", y = "Body mass (g)")

## Scatterplot (two quantitative variables)
# Flipper length vs body mass, colored by species, with linear fit
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species), alpha = 0.8, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, color = "black", na.rm = TRUE) +
  scale_color_colorblind() +
  labs(title = "Body Mass vs Flipper Length",
       subtitle = "With linear best-fit line",
       x = "Flipper length (mm)", y = "Body mass (g)", color = "Species")

