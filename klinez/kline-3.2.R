
## Project:  STA 215, Fall 2025, Comparing groups 
# Located:   Kline TCNJ Google Drive
# File Name: klinez-3.2
# Date:      2025_6_14
# Who:       Zachary D. Kline

##################################################################################
#################### Phase 1: Settings and Packages                 ####################   
##################################################################################

## Load packages
# NOTE: Install packages if these commands return an error!
library(readr)
library(psych)

# set working directory
setwd("/courses/STA145/klinez")

# Load data 
data <- read_delim("ttest.csv")

##################################################################################
#################### Phase 2: Summary Statistics                  ####################   
##################################################################################


describe(data)



##################################################################################
###########      Phase 3: Comparing Groups Proportions           ###############
##################################################################################


### COMPARISON 1
table(data$partisan, data$extreme)

### COMPARISON 2
table(data$partisan, data$politics_participation)

### COMPARISON 3
table(data$R_grad, data$politics_participation)



##################################################################################
###########            Phase 4 Comparing Groups: Means              #####################
##################################################################################

### TEST 1
t.test(data$conservativism ~ data$father_degree)

### TEST 2
t.test(data$conservativism ~ data$rural)

### TEST 3
t.test(data$conservativism ~ data$kid_suffers)

### TEST 4
t.test(data$MNTLHLTH ~ data$rural)

### TEST 5
t.test(data$MNTLHLTH ~ data$two_parents)


