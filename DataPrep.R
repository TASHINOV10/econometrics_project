#Libraries Import
library(mice) #Used to generate missing values matrix

# I. Data Review ===========================================================

#Loading the data
loan_data <- read.csv('credit_amount.csv')
View(loan_data)

# Summary
str(loan_data)
summary(loan_data)

# Missing values
colSums(is.na(loan_data))

# Duplicates
duplicates <- loan_data[duplicated(loan_data), ]
print(duplicates)

#NA Values
count_total <- nrow(loan_data)
count_na <- sum(is.na(loan_data))
pct_na <- count_na / count_total
print(count_total)
print(count_na)
print(pct_na)

#Missing Values Matrix

md.pattern(loan_data)

#Na Values per columns------------------------------------------------------

na_percentage <- colSums(is.na(loan_data)) / nrow(loan_data) * 100

na_summary <- data.frame(
  Column = names(na_percentage),
  NA_Percentage = na_percentage
)

print(na_summary)
------------------------------------------------------
  
#Column Data Types------------------------------------------------------

# Count columns with continuous variables

num_continuous <- sum(sapply(loan_data, function(x) is.numeric(x) && length(unique(x)) > 20))

# Count columns with categorical variables

num_categorical <- sum(sapply(loan_data, function(x) is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 20)))

cat("Number of Continuous Columns:", num_continuous, "\n")
cat("Number of Categorical Columns:", num_categorical, "\n")
------------------------------------------------------

#Unique Values, Categorical Vars------------------------------------------------------

# columns with categorical variables

categorical_columns <- loan_data[, sapply(loan_data, function(x) is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) < 20))]

# Get unique values for these columns
unique_values <- lapply(categorical_columns, function(x) list(unique_values = unique(x)))
names(unique_values) <- names(categorical_columns)

# Print
for (col in names(unique_values)) {
  cat("\nColumn:", col, "\n")
  print(unique_values[[col]])
}

#Column Labels, Continuous Vars------------------------------------------------------

# Get names of continuous columns (numeric with > 20 unique values)

continuous_columns_labels <- names(loan_data)[sapply(loan_data, function(x) is.numeric(x) && length(unique(x)) > 20)]

# Print only the labels
print(continuous_columns_labels)


# II. N/A Replacement/Removal ===========================================================

# # Dependents------------------------------------------------------

loan_data$Dependents[is.na(loan_data$Dependents)] <- 0
unique_dependents <- unique(loan_data$Dependents)
print(unique_dependents)


#No 0 values are present in the original data set for this column
#I assume that this stands for lack of dependents
#Replaced N/A with 0s

# # Current.Loan.Expenses..USD.------------------------------------------------------  

count_zero_records <- sum(loan_data$Current.Loan.Expenses..USD. == 0, na.rm = TRUE)
print(count_zero_records)

loan_data$Current.Loan.Expenses..USD.[(loan_data$Current.Loan.Expenses..USD.) == -999.0 ] <- 0
unique_dependents <- unique(loan_data$Current.Loan.Expenses..USD.)
summary(loan_data$Current.Loan.Expenses..USD.)

#loan_data <- loan_data[!is.na(loan_data$Current.Loan.Expenses..USD), ]


#There seems to be no records with value 0 but rather -999.
#I will replace these value with 0, assuming that it stands for no expenses.


# # Active.Credit.Card------------------------------------------------------  


#Checking what is the distribution of Loan Sanction Amount 
#given different values of Active.Credit.Card var


summary(loan_data[loan_data$Has.Active.Credit.Card == "Unpossessed", "Loan.Sanction.Amount..USD."])
summary(loan_data[loan_data$Has.Active.Credit.Card == "Active", "Loan.Sanction.Amount..USD."])
summary(loan_data[loan_data$Has.Active.Credit.Card == "Inactive", "Loan.Sanction.Amount..USD."])
summary(loan_data[is.na(loan_data$Has.Active.Credit.Card), "Loan.Sanction.Amount..USD."])


#The Loan.Sanction.Amount..USD. for NA rows differs significantly in its mean and quartile ranges compared to the other categories (Unpossessed, Active, Inactive).
#This suggests that rows with NA might belong to a unique group with specific characteristics.
#Replacing NAs with "unknown"

loan_data$Has.Active.Credit.Card[is.na(loan_data$Has.Active.Credit.Card)] <- "Unknown"

# # Income.Stability------------------------------------------------------  

summary(loan_data[loan_data$Income.Stability  == "Low", "Loan.Sanction.Amount..USD."])
summary(loan_data[loan_data$Income.Stability  == "High", "Loan.Sanction.Amount..USD."])
summary(loan_data[is.na(loan_data$Income.Stability ), "Loan.Sanction.Amount..USD."])


#Mean Values do not differ significantly
#Removing the NA values

loan_data <- loan_data[!is.na(loan_data$Income.Stability), ]

# # Gender------------------------------------------------------  
summary(loan_data[loan_data$Gender  == "M", "Loan.Sanction.Amount..USD."])
summary(loan_data[loan_data$Gender  == "F", "Loan.Sanction.Amount..USD."])
summary(loan_data[is.na(loan_data$Gender ), "Loan.Sanction.Amount..USD."])


#Due to the small number of NA observations (35) together with the 
#lack of significant differences between the means, NAs will be deleted.


loan_data <- loan_data[!is.na(loan_data$Gender), ]

# # Co.Applicant

summary(loan_data[loan_data$Co.Applicant  == 1 , "Loan.Sanction.Amount..USD."])
summary(loan_data[loan_data$Co.Applicant  == 0 , "Loan.Sanction.Amount..USD."])
summary(loan_data[loan_data$Co.Applicant  == -999, "Loan.Sanction.Amount..USD."])


#No significant differences between the means

#Checking the number of rows that contain -999

filtered_df <- loan_data[loan_data$Co.Applicant == -999, ]

nrow(filtered_df) #118

#Removing rows with -999

loan_data <- loan_data[loan_data$Co.Applicant != -999, ]
View(loan_data)

# # Income..USD.

loan_data <- loan_data[!is.na(loan_data$Income..USD.), ]

# # Credit.Score

loan_data <- loan_data[!is.na(loan_data$Credit.Score), ]

# # Current.Loan.Expenses..USD

loan_data <- loan_data[!is.na(loan_data$Current.Loan.Expenses..USD), ]

#Missing Values Matrix
md.pattern(loan_data)

# II. Exporting to csv ===========================================================

write.csv2(loan_data, "loan_data_preped.csv", row.names = FALSE)


