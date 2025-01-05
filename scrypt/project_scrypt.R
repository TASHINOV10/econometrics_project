
library(readxl)

#== DATA QUALITY CHECK == 

#== Initial data load
dataset <- read.csv('credit_amount.csv')
View(dataset)

ncol(dataset)
# 13 - total attributes

nrow(dataset)
# 21161 - total records

#== Checking the unique values of each categorical attribute
#- Gender
# "F" "M" NA
unique_values <- unique(dataset$Gender)
print(unique_values)

#- Income Stability
# "Low"  "High" NA    
unique_values <- unique(dataset$Income.Stability)
print(unique_values)

#- Location
# "Semi-Urban" "Rural" "Urban"  
unique_values <- unique(dataset$Location)
print(unique_values)

#- Dependents
# 3  1  2 NA  4  5  8  6  7 10
unique_values <- unique(dataset$Dependents)
print(unique_values)

#- Has Active Credit Card
unique_values <- unique(dataset$Has.Active.Credit.Card)
print(unique_values)

