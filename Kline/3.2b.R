
## Project:  SOC302, Fall 2025, ANOVA
# Located:   Kline TCNJ Google Drive
# File Name: Kline-3.2-ANOVA-teaching_method
# Date:      2025_7_11
# Who:       Zachary D. Kline

### Settings + Packages

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(ggplot2)
library(haven)
library(forcats)
library(psych)



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
mean(math_scores$score)
sd(math_scores$score)

###################################################################### 
###################################################################### 
############# Step 1: Examine the Data   ##################
###################################################################### 
###################################################################### 

plot1 <- ggplot(math_scores, aes(x = teaching_method, y = score, color = teaching_method, label = name)) +
  geom_point(size = 3) +
  ggtitle("Math Scores by Teaching Method") +
  ylab("Score") +
  xlab("Teaching Method") +
  geom_text(nudge_y = 1)
print(plot1)


# BOX PLOT
ggplot(math_scores, aes(x = teaching_method, y = score)) +
  geom_boxplot() +
  labs(title = "Box Plot of Math Scores by Teaching Method",
       x = "Teaching Method",
       y = "Math Score") +
  theme_minimal()


###################################################################### 
#####################################################################
#################   Step 2: Between Group Variation    ################# 
#####################################################################
###################################################################### 

# Compute the mean score for each teaching method
mean_scores <- aggregate(score ~ teaching_method, data = math_scores, FUN = mean)

# Create a barplot
plot2 <- ggplot(mean_scores, aes(x = teaching_method, y = score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Math Scores by Teaching Method", x = "Teaching Method", y = "Mean Score")
print(plot2)

#####################################################################
#################   Step 3: Variation from Grand Mean    ################# 
#####################################################################

#create difference from the overall mean
math_scores$difference <- math_scores$score - mean(math_scores$score)

plot3 <- ggplot(math_scores, aes(x = teaching_method, y = difference, color = teaching_method)) + 
  geom_point(size = 3) +
  ggtitle("Math Scores Deviation by Teaching Method") +
  ylab("Deviation from Mean Score")
print(plot3)


### A Note: Sum of squared differences here = SST or total sum of squares or variation
# (worry less about the mathmatical nuance than the premise)
#math behind var
math_scores$difference2 <- math_scores$difference*math_scores$difference
sum(math_scores$difference2)
1717.867/(30-1)# R doesn't subtract 1 from n
var(math_scores$difference)
sd(math_scores$difference)
#####################################################################
#################   Step 4: Within group variation    ################# 
#####################################################################

# Students are different!!!
### Create a scatter plot with mean lines and labels for each student
plot4 <- ggplot(math_scores, aes(x = score, y = teaching_method, label = name, fill = teaching_method)) +
  geom_point(shape = 21, size = 3, stroke = 0.5) +
  geom_text(size = 3, nudge_x = 1, nudge_y = 0.2) +
  geom_vline(data = mean_scores, aes(xintercept = score, color = teaching_method, linetype = teaching_method), show.legend = FALSE) +
  geom_text(data = mean_scores, aes(x = score, y = teaching_method, label = paste("Mean:", round(score, 2))), size = 3, hjust = -0.1, color = "black") +
  scale_fill_manual(values = c("Traditional" = "blue", "Interactive" = "orange", "Project-based" = "green")) +
  scale_y_discrete(limits = rev(levels(math_scores$teaching_method))) +
  labs(title = "Math Scores by Teaching Method", x = "Score", y = "Teaching Method")
print(plot4)


#####################################################################
#################   Step 5: ANOVA    ################# 
#####################################################################

# Perform ANOVA
anova <- aov(score ~ teaching_method, data = math_scores)
# Summarize ANOVA results
summary(anova)
# total
585+1133
# get R2
# between/total
# OR between/(between+within)
766/(1718)


