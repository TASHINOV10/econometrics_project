#Libraries Import
library(mice) #Used to generate missing values matrix
library(ggplot2)

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
#PERC
na_percentage <- colSums(is.na(loan_data_f4)) / nrow(loan_data_f4) * 100

na_summary <- data.frame(
  Column = names(na_percentage),
  NA_Percentage = na_percentage
)

print(na_summary)

#NUM
na_percentage <- colSums(is.na(loan_data))

na_summary <- data.frame(
  Column = names(na_percentage),
  NA_num = na_percentage
)

print(na_summary)
#------------------------------------------------------

# Print the data type of each column
column_types <- sapply(loan_data, class)
print(column_types)
  

#------------------------------------------------------
# Count columns with continuous variables

num_continuous <- sum(sapply(loan_data, function(x) is.numeric(x) && length(unique(x)) > 20))

# Count columns with categorical variables

num_categorical <- sum(sapply(loan_data, function(x) is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 20)))

cat("Number of Continuous Columns:", num_continuous, "\n")
cat("Number of Categorical Columns:", num_categorical, "\n")
#------------------------------------------------------

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

# # Co.Applicant------------------------------------------------------  
summary(loan_data[loan_data$Co.Applicant  == 1 , "Loan.Sanction.Amount..USD."])
summary(loan_data[loan_data$Co.Applicant  == 0 , "Loan.Sanction.Amount..USD."])
summary(loan_data[loan_data$Co.Applicant  == -999, "Loan.Sanction.Amount..USD."])

#No significant differences between the means

#Removing rows with -999
loan_data <- loan_data[loan_data$Co.Applicant != -999, ]


#Missing Values Matrix-------------------
md.pattern(loan_data)

# III. Extreme Values ===========================================================

# # Income..USD.--------------------------
q1 <- quantile(loan_data$Income..USD., 0.25, na.rm = TRUE)
q3 <- quantile(loan_data$Income..USD., 0.75, na.rm = TRUE)
iqr <- q3 - q1

lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

loan_data_f1 <- loan_data[!is.na(loan_data$Income..USD.) &
                            loan_data$Income..USD. >= lower_bound &
                            loan_data$Income..USD. <= upper_bound, ]
# # Credit.Score--------------------------
q1 <- quantile(loan_data_f1$Credit.Score, 0.25, na.rm = TRUE)
q3 <- quantile(loan_data_f1$Credit.Score, 0.75, na.rm = TRUE)
iqr <- q3 - q1

lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

loan_data_f2 <- loan_data_f1[!is.na(loan_data_f1$Credit.Score) &
                               loan_data_f1$Credit.Score >= lower_bound &
                               loan_data_f1$Credit.Score <= upper_bound, ]
# # Current.Loan.Expenses..USD.--------------------------
q1 <- quantile(loan_data_f2$Current.Loan.Expenses..USD., 0.25, na.rm = TRUE)
q3 <- quantile(loan_data_f2$Current.Loan.Expenses..USD., 0.75, na.rm = TRUE)
iqr <- q3 - q1

lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

loan_data_f3 <- loan_data_f2[!is.na(loan_data_f2$Current.Loan.Expenses..USD.) &
                               loan_data_f2$Current.Loan.Expenses..USD. >= lower_bound &
                               loan_data_f2$Current.Loan.Expenses..USD. <= upper_bound, ]
# # Loan.Amount.Request..USD.--------------------------
q1 <- quantile(loan_data_f3$Loan.Amount.Request..USD., 0.25, na.rm = TRUE)
q3 <- quantile(loan_data_f3$Loan.Amount.Request..USD., 0.75, na.rm = TRUE)
iqr <- q3 - q1

lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

loan_data_f4 <- loan_data_f3[!is.na(loan_data_f3$Loan.Amount.Request..USD.) &
                               loan_data_f3$Loan.Amount.Request..USD. >= lower_bound &
                               loan_data_f3$Loan.Amount.Request..USD. <= upper_bound, ]


#Nrows------------------------------------------------------
cat("Original rows:", nrow(loan_data), "\n")
cat("Filtered rows 1:", nrow(loan_data_f1), "\n")
cat("Filtered rows 2:", nrow(loan_data_f2), "\n")
cat("Filtered rows 3:", nrow(loan_data_f3), "\n")
cat("Filtered rows 4:", nrow(loan_data_f4), "\n")

#Boxplots---------------------------
ggplot(loan_data_f4, aes(x=1,y=Loan.Amount.Request..USD.))+
  geom_boxplot()+
  theme_light()

ggplot(loan_data_f4, aes(x=1,y=Current.Loan.Expenses..USD.))+
  geom_boxplot()+
  theme_light()

ggplot(loan_data_f4, aes(x=1,y=Credit.Score))+
  geom_boxplot()+
  theme_light()

ggplot(loan_data_f4, aes(x=1,y=Income..USD.))+
  geom_boxplot()+
  theme_light()

#Histograms---------------------------
#replace loan data version(f1,f2,f3,f4) to alternate between results

ggplot(loan_data_f4, aes(Income..USD.))+
  geom_histogram(bins=20,aes(y=..density..))

ggplot(loan_data_f4, aes(Credit.Score))+
  geom_histogram(bins=20,aes(y=..density..))

ggplot(loan_data_f4, aes(Current.Loan.Expenses..USD.))+
  geom_histogram(bins=20,aes(y=..density..))

ggplot(loan_data_f4, aes(Loan.Amount.Request..USD.))+
  geom_histogram(bins=20,aes(y=..density..))


# Correlation matrix ==============================================================
library(reshape2)

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Select only the specified numeric variables
selected_vars <- loan_data_f4[, c("Loan.Sanction.Amount..USD.", 
                                  "Income..USD.", 
                                  "Loan.Amount.Request..USD.", 
                                  "Current.Loan.Expenses..USD.", 
                                  "Credit.Score")]

# Compute the correlation matrix
correlation_matrix <- cor(selected_vars, use = "complete.obs")

# Melt the correlation matrix for ggplot
melted_corr_matrix <- melt(correlation_matrix)

# Create the heatmap with labels
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap with Labels", x = "", y = "")


#--------------------------------------------
loan_data_f4$AgeGroup <- cut(loan_data_f4$Age, 
                          breaks = c(0, 30, 40, 50, 60, 70, Inf), 
                          labels = c("0-30", "30-40", "40-50", "50-60", "60-70", "70+"),
                          include.lowest = TRUE)

ggplot(loan_data_f4, aes(x = AgeGroup, y = Loan.Sanction.Amount..USD.)) +
  geom_boxplot() +
  labs(title = "Loan Sanction Amount by Age Group",
       x = "Age Group",
       y = "Loan Sanction Amount (USD)") +
  theme_minimal()

anova_test <- aov(Loan.Sanction.Amount..USD. ~ AgeGroup, data = loan_data_f4)
summary(anova_test)


tree <- rpart(Loan.Sanction.Amount..USD. ~ Age, 
              data = loan_data_f4, 
              method = "anova", 
              control = rpart.control(cp = 0.0001, minsplit = 20))
print(tree)


correlation <- cor(loan_data_f4$AgeGroup, loan_data_f4$Loan.Sanction.Amount..USD., use = "complete.obs")
cat("Correlation Coefficient:", correlation, "\n")




# III. Exporting to csv ===========================================================

write.csv2(loan_data, "loan_data_preped.csv", row.names = FALSE)





