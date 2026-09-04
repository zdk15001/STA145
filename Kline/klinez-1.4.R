# load package haven Install package if needed.
library(haven)

# use read_dta function to load GSS as an object in the environment
GSS2024 <- read_dta("GSS2024.dta")

# View the GSS2022 dataframe
View(GSS2024)


# create frequency table showing R's general happiness 
table(GSS2024$happy, useNA = "ifany")

table(GSS2024$immfate)