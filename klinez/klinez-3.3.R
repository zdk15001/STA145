##### PRE ANALYSIS: Create the data frame        ##### 

# Create a data frame 
# Create the dataframe with a total of 30 observations
vote_table <- data.frame(
  Township = c(
    "Newark", "Jersey City", "Trenton", "Camden", "Elizabeth", "Paterson", "Clifton", "Passaic", "Union", "Edison",
    "Woodbridge", "Hoboken", "Bayonne", "Toms River", "Union City", "East Orange", "Irvington", "Plainfield", "North Bergen",
    "Bloomfield", "West New York", "West Orange", "Hackensack", "Sayreville", "Linden", "Kearny"
  ),
  campaign_spending = c(
    50, 75, 85, 100, 125, 150, 175, 180, 200, 225, 230, 250, 260,
    270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370, 380, 390
  ),
  observed_vote_share = c(
    15, 22, 29, 48, 28, 40, 26, 44, 51, 50, 25, 56, 46,
    15, 51, 41, 55, 75, 51, 39, 56, 72, 45, 62, 51, 57
  )
)


##### STEP 1: Examine the scatter plot
# showing the relationship between campaign spending and observed vote share 


linear_plot <- plot(vote_table$campaign_spending, vote_table$observed_vote_share)
print(linear_plot)

# add x line and y line for means
meany <- mean(vote_table$campaign_spending)
meanx <- mean(vote_table$observed_vote_share)

abline(v = meany, col = "black")
abline(h = meanx, col = "black")



##### STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot
linear_relationship <- lm(observed_vote_share ~ campaign_spending, data = vote_table)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

##### STEP 3: Plot the residuals

# Plot the residuals
plot(vote_table$campaign_spending, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


