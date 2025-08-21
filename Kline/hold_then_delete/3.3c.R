##### STEP 1: Create the data frame        ##### 

# Create a vector of campaign advertising spending values
campaign_spending <- c(50000, 75000, 85000, 100000, 125000, 150000, 175000, 180000, 200000, 225000, 230000, 250000, 260000)

# Create a vector of observed vote share values
observed_vote_share <- c(55, 66, 69, 88, 68, 70, 66, 84, 81, 90, 65, 96, 84)

# Create a data frame with campaign spending and observed vote share
vote_table <- data.frame(campaign_spending, observed_vote_share)

# Print the table
vote_table

##### STEP 2: Examine the scatter plot  "plot()" 
# REMINDER: To call a variable, use the $ (e.g., vote_table$campaign_spending)


##### STEP 3: Calculate the mean vote share and mean campaign spending "mean()"


##### STEP 5: Calculate the correlation between  vote share and  campaign spending "cor()"


