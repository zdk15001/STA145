## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(haven)


# set working directory
setwd("H:/sta215_fall2024")

# Load the data
dating <- read_delim("dating.csv")

###########################################################################
###########################       Phase 1           #######################
###########################       Descriptive Statistics            ######################
###########################################################################

### Use the table() command to gather summary statistics
# example
table(dating$RESPONDENT_RACE)
table(dating$PARTNER_RACE)

###########################################################################
###########################       Phase 2           #######################
###########################       Contingency Table            ######################
###########################################################################
########## use the table() command with two variables. 
# The first variable should be the rows and the second should be the column
########## Example
table(dating$RESPONDENT_RACE , dating$PARTNER_RACE)

###########################################################################
###########################       Phase 5           #######################
###########################       chi2            ######################
###########################################################################
########## use the chisq.test() command with two variables in a contingency table
chisq.test(table(dating$RESPONDENT_RACE , dating$PARTNER_RACE))
