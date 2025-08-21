
# Set Working Directory
setwd("H:/sta215_fall2024")

# Install "haven" packag
#install.packages("haven")

# Load "haven" package
library("haven")

# Load fifthgrader data
fifth_graders <- read.csv("fifth_graders.csv")


# create and interpret one-sample confidence intervals for all quantitative variables
t.test(fifth_graders$age)
t.test(fifth_graders$reading_score)
t.test(fifth_graders$math_score)
t.test(fifth_graders$science_score)
t.test(fifth_graders$household_income_in_1000s)
t.test(fifth_graders$attendance_rate_.)
t.test(fifth_graders$hours_spent_on_homework_per_week)
t.test(fifth_graders$physical_activity_hours_week)
t.test(fifth_graders$hours_of_sleep_per_night)


# Q1: Calculate mean reading score
mean(fifth_graders$reading_score, na.rm = TRUE)

# Q2: Calculate percentage of students in extracurricular activities
table(fifth_graders$extracurricular_activities)
252/(252+248)

# Q3: Calculate percentage of students whose parents have bachelors or graduate degree
table(fifth_graders$parent_education_level)
(125+68)/(125+68+156+151)

# Q4: # Calculate 95% confidence interval for household income
t.test(fifth_graders$household_income_in_1000s)

# Q5 Calculate 95% confidence interval for hours spent on homework
t.test(fifth_graders$hours_spent_on_homework_per_week)

# Q6 Better than average
t.test(fifth_graders$reading_score)
t.test(fifth_graders$math_score)
t.test(fifth_graders$science_score)