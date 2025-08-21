# Set Working Directory
setwd("H:/sta215_fall2024")

# Install "haven" packag
#install.packages("haven")

# Load "haven" package
library("haven")

# Load GSS 2022 as an object called "GSS2022"
GSS2022 <- read_dta("GSS2022.dta")

# create frequency table showing R's general happiness 
table(GSS2022$happy, useNA = "ifany")

# create dummy variable for "not happy"
GSS2022$not_happy <- ifelse(GSS2022$happy == 3, 1, 0)
table(GSS2022$not_happy)

# Question 1
table(GSS2022$hapcohab, useNA = "ifany")
GSS2022$happy_cohab <- ifelse(GSS2022$hapcohab == 1, 1, 0)
table(GSS2022$happy_cohab)


# Question 2
table(GSS2022$relig, useNA = "ifany")
GSS2022$jewish <- ifelse(GSS2022$relig == 3, 1, 0)
table(GSS2022$jewish)

# Question 3
table(GSS2022$trust, useNA = "ifany")
GSS2022$trust_most <- ifelse(GSS2022$trust == 3, 1, 0)
table(GSS2022$trust_most)

# Question 4
table(GSS2022$owngun, useNA = "ifany")
GSS2022$gun_owner <- ifelse(GSS2022$owngun == 1, 1, 0)
table(GSS2022$gun_owner)

# Question 5
table(GSS2022$hunt, useNA = "ifany")
GSS2022$hunter <- ifelse(GSS2022$hunt < 3, 1, 0)
table(GSS2022$hunter)

# Question 6
table(GSS2022$finalter, useNA = "ifany")
GSS2022$worse_finances <- ifelse(GSS2022$finalter == 2, 1, 0)
table(GSS2022$worse_finances)

# Question 7
table(GSS2022$immfate, useNA = "ifany")
GSS2022$favors_deportation <- ifelse(GSS2022$immfate == 3, 1, 0)
table(GSS2022$favors_deportation)

# Question 8
table(GSS2022$richwork, useNA = "ifany")
GSS2022$would_work <- ifelse(GSS2022$richwork == 1, 1, 0)
table(GSS2022$would_work)
