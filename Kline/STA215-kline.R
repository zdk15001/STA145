## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(haven)


# set working directory
setwd("H:/sta215_fall2024")


# if data
data <- read_delim("raw_data.csv")

# table 2
table(data$var1, data$var2)