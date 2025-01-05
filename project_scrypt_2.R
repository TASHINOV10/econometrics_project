# ==== Exploratory Data Analysis ==
# I. Data Review
#===========================================================

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

#------------------------------------------------------------------------
# Count columns with continuous variables
num_continuous <- sum(sapply(loan_data, function(x) is.numeric(x) && length(unique(x)) > 20))

# Count columns with categorical variables
num_categorical <- sum(sapply(loan_data, function(x) is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 20)))

cat("Number of Continuous Columns:", num_continuous, "\n")
cat("Number of Categorical Columns:", num_categorical, "\n")


# Printing unique values of categorical variables
#------------------------------------------------------------------------
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

# Printing labels of columns with continuous variables
#------------------------------------------------------------------------
# Get names of continuous columns (numeric with > 20 unique values)
continuous_columns_labels <- names(loan_data)[sapply(loan_data, function(x) is.numeric(x) && length(unique(x)) > 20)]

# Print only the labels
print(continuous_columns_labels)

# II. NA Replacement
#===========================================================
# # Dependents

loan_data$Dependents[is.na(loan_data$Dependents)] <- 0
unique_dependents <- unique(loan_data$Dependents)
print(unique_dependents)
# No 0 values are present in the original data set for this column
# I assume that this stands for lack of dependents
# Replaced NA with 0s

# # Current.Loan.Expenses..USD  

count_zero_records <- sum(loan_data$Current.Loan.Expenses..USD == 0, na.rm = TRUE)
print(count_zero_records)

loan_data$Current.Loan.Expenses..USD[(loan_data$Current.Loan.Expenses..USD) == -999.0 ] <- 0
unique_dependents <- unique(loan_data$Current.Loan.Expenses..USD)
summary(loan_data$Current.Loan.Expenses..USD)

# There seems to be no records with value 0 but rather -999.
# I will replace this value with 0, assuming that it stands for no expenses.


# =====================================
# Creating Age classification bin
# Might be helpful to fill in the missing values for the continuous vars based on the distribution per age class
# If a certain variable is normally distributed it means that there are no outliers
# I can use the mean of such variable to fill in the blanks

summary(loan_data$Age)

loan_data$Age_Bins <- cut(
  loan_data$Age,
  breaks = c(18,26,35,45,65), # Age breakpoints
  labels = c("Youth", "Young Adult", "Adult", "Middle Age"), # Bin labels
  include.lowest = TRUE, # Include the lowest boundary (18)
  right = TRUE # Include the right edge (e.g., 26 is part of Young Adult)
)
  
'''
Category	Age Range
Youth 18–26
Young Adult	27 - 35
Adult	36–45
Middle Age	46–65
'''
#---------------------------------------------------
library(dplyr)
# # Income..USD  

age_bin_summary <- loan_data %>%
  group_by(Age_Bins) %>%
  summarise(
    Mean_Income = mean(Income..USD., na.rm = TRUE),
    Median_Income = median(Income..USD., na.rm = TRUE),
    Count = n()
  )
print(age_bin_summary)

library(ggplot2)

ggplot(loan_data, aes(x = Age_Bins, y = Income..USD.)) +
  geom_boxplot() +
  labs(title = "Income Distribution Across Age Bins", 
       x = "Age Bins", y = "Income")
# Income..USD.. contains an excessive outliers.

max_value <- max(loan_data$`Income..USD.`, na.rm = TRUE)
loan_data_1 <- subset(loan_data,Income..USD. < max_value)
max_value <- max(loan_data_1$`Income..USD.`, na.rm = TRUE)
loan_data_2 <- subset(loan_data_1,Income..USD. < max_value)

ggplot(loan_data_2, aes(x = Age_Bins, y = `Income..USD.`)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Age_Bins), width = 0.2, alpha = 0.3) 
  labs(
    title = "Income Distribution Across Age Bins", 
    x = "Age Bins", 
    y = "Income (USD)"
  ) +
  theme_minimal()


