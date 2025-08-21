# Set Working Directory
setwd("H:/sta215_fall2024")

# Install "haven" packag
#install.packages("haven")

# Load "haven" package
library("haven")

# Load GSS 2022 as an object called "GSS2022"
GSS2022 <- read_dta("GSS2022.dta")


# Table for Variable 1
table(GSS2022$finalter)


# Table for Variable 2
table(GSS2022$immfate)


