# ==== Exploratory Data Analysis ==

#Loading the data
loan_data <- read.csv('credit_amount.csv')
View(loan_data)

# Summary
str(loan_data)
summary(loan_data)

# Missing values
colSums(is.na(loan_data))


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
# continuous columns (numeric values with > 20 unique)
continuous_columns <- loan_data[, sapply(loan_data, function(x) (is.numeric(x) && length(unique(x)) > 20))]

# Print
print(continuous_columns)