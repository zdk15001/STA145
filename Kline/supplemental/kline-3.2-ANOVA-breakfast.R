
## Project:  SOC302, spring 2022, ANOVA
# Located:   Kline TCNJ Google Drive
# File Name: Kline(2023_4_14)ANOVA-activity
# Date:      2023_4_14
# Who:       Zachary D. Kline

### Settings + Packages

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)
library(readxl)



###################################################################### 
############# Pre-Analysis: create the data frame   ##################
###################################################################### 

math_scores <- data.frame(
  teaching_method = c(rep("Traditional", 10), rep("Interactive", 10), rep("Project-based", 10)),
  score = c(71, 79, 77, 88, 73, 75, 65, 82, 69, 68,
            83, 88, 75, 73, 77, 84, 86, 70, 81, 79,
            76, 95, 89, 93, 80, 91, 84, 82, 78, 87),
  name = c("Alex", "Bella", "Calvin", "Marcus", "Eva", 
           "Finn", "Greta", "Henry", "Isla", "James", 
           "Kira", "Liam", "Mia", "Noah", "Olivia", 
           "Paul", "Quinn", "Ruby", "Stella", "Theo", 
           "Uma", "Vera", "William", "Xander", "Yara", 
           "Zoe", "Alice", "Ben", "Charlie", "Dahlia"),
  breakfast = c("No Breakfast", "Sugary Cereal", "No Breakfast", "Sugary Cereal", "No Breakfast", 
                "No Breakfast", "No Breakfast", "No Breakfast", "No Breakfast", "No Breakfast", 
                "Full Breakfast", "Full Breakfast", "Full Breakfast", "No Breakfast", "No Breakfast", 
                "Sugary Cereal", "Sugary Cereal", "Sugary Cereal", "Full Breakfast", "Sugary Cereal", 
                "Sugary Cereal", "Sugary Cereal", "Full Breakfast", "Full Breakfast", "Full Breakfast", 
                "Full Breakfast", "Sugary Cereal", "Full Breakfast", "Sugary Cereal", "Full Breakfast"),
  video_game_usage = c("More than an hour", "No video games", "No video games", "No video games", "No video games",
                       "No video games", "No video games", "No video games", "No video games", "No video games",
                       "An hour of video games", "An hour of video games", "An hour of video games", "More than an hour", "No video games",
                       "An hour of video games", "No video games", "No video games", "More than an hour", "No video games",
                       "More than an hour", "More than an hour", "No video games", "An hour of video games", "More than an hour",
                       "More than an hour", "An hour of video games", "No video games", "An hour of video games", "No video games"),
  music_type = c("Pop", "Rock", "Hip-Hop", "Classical", "Country",
                 "Pop", "Rock", "Hip-Hop", "Classical", "Country",
                 "Pop", "Rock", "Hip-Hop", "Classical", "Country",
                 "Pop", "Rock", "Hip-Hop", "Classical", "Country",
                 "Pop", "Rock", "Hip-Hop", "Classical", "Country",
                 "Pop", "Rock", "Hip-Hop", "Classical", "Country"))


math_scores$teacher_liked_by_parents <- c("Yes", "No", "No", "Yes", "Yes", 
                                          "No", "Yes", "No", "No", "Yes", 
                                          "No", "Yes", "Yes", "Yes", "No", 
                                          "Yes", "Yes", "No", "No", "No", 
                                          "Yes", "No", "Yes", "No", "Yes", 
                                          "No", "No", "Yes", "No", "Yes")

hist(math_scores$score)

###################################################################### 
###################################################################### 
############# Step 1: Examine the Data   ##################
###################################################################### 
###################################################################### 


# Create a scatter plot of grades with teaching method labeled
plot1_adapted <- ggplot(math_scores, aes(x = breakfast, y = score, color = breakfast, label = name)) +
  geom_point(size = 3) +
  ggtitle("Math Scores by Breakfast Type") +
  ylab("Score") +
  xlab("Breakfast Type") +
  geom_text(nudge_y = 1)
print(plot1_adapted)


# BOX PLOT
ggplot(math_scores, aes(x = breakfast, y = score)) +
  geom_boxplot() +
  labs(title = "Box Plot of Math Scores by Breakfast Type",
       x = "Breakfast",
       y = "Math Score") +
  theme_minimal()

###################################################################### 
#####################################################################
#################   Step 2: Between Group Variation    ################# 
#####################################################################
###################################################################### 

# Compute the mean score for each breakfast type
mean_scores_adapted <- aggregate(score ~ breakfast, data = math_scores, FUN = mean)

# Create a barplot
plot2_adapted <- ggplot(mean_scores_adapted, aes(x = breakfast, y = score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Math Scores by Breakfast Type", x = "Breakfast Type", y = "Mean Score")
print(plot2_adapted)

#####################################################################
#################   Step 3: Variation from Grand Mean    ################# 
#####################################################################

math_scores$difference <- math_scores$score - mean(math_scores$score)

plot3_adapted <- ggplot(math_scores, aes(x = breakfast, y = difference, color = breakfast)) + 
  geom_point(size = 3) +
  ggtitle("Math Scores Deviation by Breakfast Type") +
  ylab("Deviation from Mean Score")
print(plot3_adapted)



#####################################################################
#################   Step 4: Within group variation    ################# 
#####################################################################

### Create a scatter plot with mean lines and labels for each student
plot4_adapted <- ggplot(math_scores, aes(x = score, y = breakfast, label = name, fill = breakfast)) +
  geom_point(shape = 21, size = 3, stroke = 0.5) +
  geom_text(size = 3, nudge_x = 1, nudge_y = 0.2) +
  geom_vline(data = mean_scores_adapted, aes(xintercept = score, color = breakfast, linetype = breakfast), show.legend = FALSE) +
  geom_text(data = mean_scores_adapted, aes(x = score, y = breakfast, label = paste("Mean:", round(score, 2))), size = 3, hjust = -0.1, color = "black") +
  scale_fill_manual(values = c("Sugary Cereal" = "blue", "Full Breakfast" = "orange", "No Breakfast" = "green")) +
  scale_y_discrete(limits = rev(levels(math_scores$breakfast))) +
  labs(title = "Math Scores by Breakfast Type", x = "Score", y = "Breakfast Type")
print(plot4_adapted)




#####################################################################
#################   Step 5: ANOVA    ################# 
#####################################################################

# Perform ANOVA
anova_adapted <- aov(score ~ breakfast, data = math_scores)
# Summarize ANOVA results
summary(anova_adapted)
# total SS; TSS
952+766
# get R2
# between/total
# OR between/(between+within)
766/(766+952)

