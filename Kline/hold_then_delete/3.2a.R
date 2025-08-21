
## Project:  STA 215, Fall 2023, Comparing groups 
# Located:   Kline TCNJ Google Drive
# File Name: Kline(2023_3_14)STA215_groups
# Date:      2023_3_14
# Who:       Zachary D. Kline

##################################################################################
#################### STEP 1: Settings and Packages                 ####################   
##################################################################################

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(ggplot2)
library(haven)
library(psych)
install.packages(psych)

# set working directory
setwd("H:/sta215_fall2024")

# Load data 
dataset <- read_delim("ttest.csv")

##################################################################################
#################### STEP 2: Summary Statistics                  ####################   
##################################################################################


describe(dataset)



##################################################################################
###########      Phase 2: Comparing Groups Proportions           ###############
##################################################################################


### COMPARISON 1
table(dataset$partisan, dataset$extreme)


### COMPARISON 2
table(dataset$partisan, dataset$politics_participation)

### COMPARISON 3
table(dataset$R_grad, dataset$politics_participation)



##################################################################################
###########             Comparing Groups: Means              #####################
##################################################################################

### TEST 1
t.test(dataset$conservativism ~ dataset$father_degree)

### TEST 2
t.test(dataset$conservativism ~ dataset$rural)

### TEST 3
t.test(dataset$conservativism ~ dataset$kid_suffers)

### TEST 4
t.test(dataset$MNTLHLTH ~ dataset$rural)

### TEST 5
t.test(dataset$MNTLHLTH ~ dataset$two_parents)


