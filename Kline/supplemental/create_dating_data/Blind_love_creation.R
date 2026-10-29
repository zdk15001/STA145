
## Project:  STA 215, Fall 2025, Is Love Really Blind?
# Located:   Kline TCNJ Google Drive
# File Name: Blind_love_creation
# Date:      2025_7_12
# Who:       Zachary D. Kline


#https://www.icpsr.umich.edu/web/pages/instructors/ddlgs/guides/romance/index.html

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

# Load and rename data and remove old data 
setwd("/courses/STA145/Kline/create_dating_data")
load("dating_data.rda")
HCMST <- da30103.0001
rm(da30103.0001) #  removed to keep environment clear

###########################################################################
################################       Phase 1       #######################
################################       Clean Data   ######################
###########################################################################


######### Clean Var: Respondent's race (RESPONDENT_RACE)
### Step 1: Examine Var
table(HCMST$RESPONDENT_RACE)
### Step 2: Recoding not needed
### Step 3: Recoding not needed


######### Clean Var: Partner's race (PARTNER_RACE)
table(HCMST$PARTNER_RACE)
### Step 1: Examine Var
### Step 2: Recoding not needed
### Step 3: Recoding not needed

######### Clean Var: Does R have a spouse or partner? (QFLAG)
### Step 1: Examine Var
table(HCMST$QFLAG)

### Step 2: Recoding not needed
### Step 3: Recoding not needed

######### Clean Var: How old is partner? (Q9)
### Step 1: Examine Var
table(HCMST$Q9)

### Step 2: Recoding not needed
### Step 3: Recoding not needed

######### Clean Var: R's age--7 categories (PPAGECAT)
### Step 1: Examine Var
table(HCMST$PPAGECAT)

### Step 2: Recoding not needed
### Step 3: Recoding not needed

######### Clean Var: What is your religion? (PAPRELIGION)
### Step 1: Examine Var
table(HCMST$PAPRELIGION)

### Step 2: Recoding not needed
### Step 3: Recoding not needed

######### Clean Var: What is partner's religion? (Q7B)
### Step 1: Examine Var
table(HCMST$Q7B)

### Step 2: Recoding not needed
### Step 3: Recoding not needed

######### Clean Var: R's education--highest degree received (PPEDUC)
### Step 1: Examine Var
table(HCMST$PPEDUC)

### Step 2: into no bachelors, bachelors, advanced
# Get numeric codes 1–14 even if PPEDUC is a labeled factor
HCMST$PPEDUC_numeric <- parse_number(as.character(HCMST$PPEDUC))

HCMST <- HCMST %>%
  mutate(
    respondent_education = case_when(
      PPEDUC_numeric %in% 1:11      ~ "Less than Bachelors",  # includes some college & associate
      PPEDUC_numeric == 12          ~ "Bachelors",
      PPEDUC_numeric %in% 13:14     ~ "Advanced degree",
      TRUE                   ~ NA_character_
    ),
    respondent_education = factor(respondent_education, levels = c("Less than Bachelors", "Bachelors", "Advanced degree"))
  )


### Step 3: assess
table(HCMST$PPEDUC, HCMST$respondent_education)

######### Clean Var: What is the highest level of schooling partner has completed? (Q10)
### Step 1: Examine Var
table(HCMST$Q10)

### Step 2: Recoding same as respondent
HCMST$Q10_numeric <- parse_number(as.character(HCMST$Q10))

HCMST <- HCMST %>%
  mutate(
    partner_education = case_when(
      Q10_numeric %in% 1:11      ~ "Less than Bachelors",  # includes some college & associate
      Q10_numeric == 12          ~ "Bachelors",
      Q10_numeric %in% 13:14     ~ "Advanced degree",
      TRUE                   ~ NA_character_
    ),
    partner_education = factor(partner_education, levels = c("Less than Bachelors", "Bachelors", "Advanced degree"))
  )


### Step 3: assess
table(HCMST$Q10, HCMST$partner_education)
### Step 3: Recoding not needed

######### Clean Var: Political party affiliation 3 categories (PPPARTYID3)
### Step 1: Examine Var
table(HCMST$PPPARTYID3)

### Step 2: Recoding not needed
### Step 3: Recoding not needed

######### Clean Var: Does [partner] think of him/herself as Republican, Democrat, independent, or another party? (Q12)
### Step 1: Examine Var
table(HCMST$Q12)

### Step 2: Recoding not needed
### Step 3: Recoding not needed

######### Clean Var: Where did you meet your partner? (Q31_1 though Q31_8)
### Step 1: Examine Var
table(HCMST$Q31_1)
table(HCMST$Q31_2)
table(HCMST$Q31_3)
table(HCMST$Q31_4)
table(HCMST$Q31_5)
table(HCMST$Q31_6)
table(HCMST$Q31_7)
table(HCMST$Q31_8)
#work, school, church, internet, trip,bar,   extracurr, party
### Step 2: Make into dummy binary variables
HCMST <- mutate(HCMST, work = ifelse(Q31_1 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, school = ifelse(Q31_2 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, church = ifelse(Q31_3 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, internet = ifelse(Q31_4 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, trip = ifelse(Q31_5 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, bar = ifelse(Q31_6 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, extracurr = ifelse(Q31_7 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, party = ifelse(Q31_8 == "(1) yes", 1, 0))

### Step 3: Confirm correct
table(HCMST$Q31_1, HCMST$work)
table(HCMST$Q31_2, HCMST$school)
table(HCMST$Q31_3, HCMST$church)
table(HCMST$Q31_4, HCMST$internet)
table(HCMST$Q31_5, HCMST$trip)
table(HCMST$Q31_6, HCMST$bar)
table(HCMST$Q31_7, HCMST$extracurr)
table(HCMST$Q31_8, HCMST$party)

######### Clean Var: Who introduced you to your partner? (Q33_1 through Q33_7)
### Step 1: Examine Var
table(HCMST$Q33_1)
table(HCMST$Q33_2)
table(HCMST$Q33_3)
table(HCMST$Q33_4)
table(HCMST$Q33_5)
table(HCMST$Q33_6)
table(HCMST$Q33_7)

### Step 2: Recode into dummy vars
#family, friends, coworkers, classmates, neighbor, self, partner_HCMST
HCMST <- mutate(HCMST, family = ifelse(Q33_1 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, friends = ifelse(Q33_2 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, coworkers = ifelse(Q33_3 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, classmates = ifelse(Q33_4 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, neighbor = ifelse(Q33_5 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, self = ifelse(Q33_6 == "(1) yes", 1, 0))
HCMST <- mutate(HCMST, partner_dating = ifelse(Q33_7 == "(1) yes", 1, 0))

### Step 3: Confirm
table(HCMST$Q33_1, HCMST$family)
table(HCMST$Q33_2, HCMST$friends)
table(HCMST$Q33_3, HCMST$coworkers)
table(HCMST$Q33_4, HCMST$classmates)
table(HCMST$Q33_5, HCMST$neighbor)
table(HCMST$Q33_6, HCMST$self)
table(HCMST$Q33_7, HCMST$partner_dating)


######### Clean Var: How long ago romance with partner began, in years (HOW_LONG_AGO_FIRST_ROMANTIC)
### Step 1: Examine Var
table(HCMST$HOW_LONG_AGO_FIRST_ROMANTIC)

table(HCMST$Q33_1, HCMST$family)

### Step 2: Recoding not needed
### Step 3: Recoding not needed

######### Clean Var: How long ago respondent met partner, in years (HOW_LONG_AGO_FIRST_MET)
### Step 1: Examine Var
table(HCMST$HOW_LONG_AGO_FIRST_MET)

### Step 2: Recoding not needed
### Step 3: Recoding not needed


######### Clean Var: What is the highest level of schooling partner has completed? (Q10)
### Step 1: Examine Var
table(HCMST$Q10)

### Step 2: Create Catagorical educ
HCMST <- mutate(HCMST, partner_edu_BA = ifelse(Q10 == "(12) bachelor's degree", 1, 0))
HCMST <- mutate(HCMST, partner_edu_MA = ifelse(Q10 == "(13) master's degree", 1, 0))
HCMST <- mutate(HCMST, partner_edu_phd = ifelse(Q10 == "(14) professional or doctorate degree", 1, 0))
HCMST <- mutate(HCMST, partner_cat_edu = ifelse(Q10 == "(12) bachelor's degree", 2, ifelse(Q10 == "(13) master's degree", 3, ifelse(Q10 == "(14) professional or doctorate degree", 4, 1))))
HCMST <- mutate(HCMST, partner_graduate = ifelse(Q10 == "(12) bachelor's degree", 1, ifelse(Q10 == "(13) master's degree", 1, ifelse(Q10 == "(14) professional or doctorate degree", 1, 0))))


### Step 3: confirm by comparing to initial variable
table(HCMST$Q10, HCMST$partner_edu_BA)
table(HCMST$Q10, HCMST$partner_edu_MA)
table(HCMST$Q10, HCMST$partner_edu_phd)
table(HCMST$Q10, HCMST$partner_cat_edu)

######### Clean Var: R's education--highest degree received (PPEDUC)
### Step 1: Examine Var
table(HCMST$PPEDUC)

### Step 2: Create Catagorical educ
HCMST <- mutate(HCMST, edu_BA = ifelse(PPEDUC == "(12) bachelors degree", 1, 0))
HCMST <- mutate(HCMST, edu_MA = ifelse(PPEDUC == "(13) masters degree", 1, 0))
HCMST <- mutate(HCMST, edu_phd = ifelse(PPEDUC == "(14) professional or doctorate degree", 1, 0))
HCMST <- mutate(HCMST, cat_edu = ifelse(PPEDUC == "(12) bachelors degree", 2, ifelse(PPEDUC == "(13) masters degree", 3, ifelse(PPEDUC == "(14) professional or doctorate degree", 4, 1))))
HCMST <- mutate(HCMST, graduate = ifelse(PPEDUC == "(12) bachelors degree", 1, ifelse(PPEDUC == "(13) masters degree", 1, ifelse(PPEDUC == "(14) professional or doctorate degree", 1, 0))))

### Step 3: confirm by comparing to initial variable
table(HCMST$PPEDUC, HCMST$edu_BA)
table(HCMST$PPEDUC, HCMST$edu_MA)
table(HCMST$PPEDUC, HCMST$edu_phd)
table(HCMST$PPEDUC, HCMST$cat_edu)


######### Clean Var: R's age
### Step 1: Examine Var
table(HCMST$PPAGE)
### STEP 2 and 3: Not needed; age is quantititative

###########################################################################
###########################       Phase 2       ###########################
###########################       Subset Data   ###########################
###########################################################################

### Step 1: Create list of vars
varlist_dating <- c("RESPONDENT_RACE", "PARTNER_RACE", 
                    'respondent_education', "partner_education", 
                    "PPPARTYID3", "Q12")
### Step 2: create a new dataset with only your variables and complete case
dating <- HCMST %>%
  select(all_of(varlist_dating)) %>%
  filter(complete.cases(.))

# Save data
write.csv(dating, "dating.csv")


