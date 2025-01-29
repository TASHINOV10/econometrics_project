#Libraries Import
library(mice) 
library(ggplot2)
library(reshape2)
library(dplyr)
library(car)
library(tseries)
library(lmtest)


#Data Review

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
na_percentage <- colSums(is.na(loan_data)) / nrow(loan_data) * 100

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

# A. Data Cleaning ===========================================================

# # Dependents------------------------------------------------------

loan_data$Dependents[is.na(loan_data$Dependents)] <- 0
unique_dependents <- unique(loan_data$Dependents)
print(unique_dependents)

#No 0 values are present in the original data set for this column
#I assume that this stands for lack of dependents

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
md.pattern(loan_data_f4)


# Extreme Values

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

# B. Descriptive Analysis  ===========================================================

#Boxplots---------------------------

plot1 <- ggplot(loan_data_f4, aes(x = 1, y = Loan.Amount.Request..USD.)) +
  geom_boxplot() +
  theme_light() +
  labs(title = "Loan Amount Request")

plot2 <- ggplot(loan_data_f4, aes(x = 1, y = Current.Loan.Expenses..USD.)) +
  geom_boxplot() +
  theme_light() +
  labs(title = "Current Loan Expenses")

plot3 <- ggplot(loan_data_f4, aes(x = 1, y = Credit.Score)) +
  geom_boxplot() +
  theme_light() +
  labs(title = "Credit Score")

plot4 <- ggplot(loan_data_f4, aes(x = 1, y = Income..USD.)) +
  geom_boxplot() +
  theme_light() +
  labs(title = "Income")

combined_plot <- (plot1 | plot2) / (plot3 | plot4)

print(combined_plot)

#Linear Relationship------------------

#Loan.Amount.Request..USD.
plot1 <- ggplot(data = loan_data_f4, aes(x = Loan.Amount.Request..USD., y = Loan.Sanction.Amount..USD.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Linear Fit",
       x = "Loan.Amount.Request..USD.",
       y = "Loan Sanction Amount USD")

#Current.Loan.Expenses..USD.
plot2 <- ggplot(data = loan_data_f4, aes(x = Current.Loan.Expenses..USD., y = Loan.Sanction.Amount..USD.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Linear Fit",
       x = "Current.Loan.Expenses..USD.",
       y = "Loan Sanction Amount USD")

#Credit.Score

plot3 <- ggplot(data = loan_data_f4, aes(x = Credit.Score, y = Loan.Sanction.Amount..USD.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Linear Fit",
       x = "Credit.Score.",
       y = "Loan Sanction Amount USD")

#Income..USD.

plot4 <- ggplot(data = loan_data_f4, aes(x = Income..USD., y = Loan.Sanction.Amount..USD.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Linear Fit",
       x = "Income..USD.",
       y = "Loan Sanction Amount USD")

#Dependents

plot5 <- ggplot(data = loan_data_f4, aes(x = Dependents, y = Loan.Sanction.Amount..USD.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Linear Fit",
       x = "Dependents",
       y = "Loan Sanction Amount USD")


combined_plot <- (plot1 | plot2) / (plot3 | plot4) 

print(combined_plot)

#Histograms---------------------------
#replace loan data version(f1,f2,f3,f4) to alternate dataset version

plot1 <- ggplot(loan_data_f4, aes(Income..USD.))+
  geom_histogram(bins=20,aes(y=..density..))

plot2 <- ggplot(loan_data_f4, aes(Credit.Score))+
  geom_histogram(bins=20,aes(y=..density..))

plot3 <- ggplot(loan_data_f4, aes(Current.Loan.Expenses..USD.))+
  geom_histogram(bins=20,aes(y=..density..))

plot4 <- ggplot(loan_data_f4, aes(Loan.Amount.Request..USD.))+
  geom_histogram(bins=20,aes(y=..density..))

combined_plot <- (plot1 | plot2) / (plot3 | plot4) 

print(combined_plot)

# Correlation matrix --------------------------------------------------------------

# Select only the specified numeric variables
selected_vars <- loan_data_f4[, c("Loan.Sanction.Amount..USD.", 
                                  "Income..USD.", 
                                  "Loan.Amount.Request..USD.", 
                                  "Current.Loan.Expenses..USD.", 
                                  "Credit.Score",
                                  "Dependents",
                                  "Age")]

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


# C. Feature Engineering ===========================================================

# Age -----------
# This variable has no linear relationship with Loan Sanction Amount
# Creating separate bins to overcome the linearity issue

loan_data_f4$AgeGroup <- cut(loan_data_f4$Age, 
                          breaks = c(18, 30, 40, 50, 60, 70, Inf), 
                          labels = c("18-30", "30-40", "40-50", "50-60", "60-70", "70+"),
                          include.lowest = TRUE)

#18-30: Includes all values ≥18 and <30.
#30-40: Includes all values ≥30 and <40.
#40-50: Includes all values ≥40 and <50.
#50-60: Includes all values ≥50 and <60.
#60-70: Includes all values ≥60 and <70.
#70+: Includes all values ≥70.


ggplot(data = loan_data_f4, aes(x = AgeGroup, y = Loan.Sanction.Amount..USD.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Linear Fit",
       x = "Independent Variable",
       y = "Dependent Variable")

plot1 <- ggplot(data = loan_data_f4, aes(x = Age, y = Loan.Sanction.Amount..USD.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Linear Fit",
       x = "Age",
       y = "Loan Sanction Amount USD")

#Determining the base group by number of observations
age_group_counts <- loan_data_f4 %>%
  group_by(AgeGroup) %>%
  summarise(Count = n())
print(age_group_counts)

#18-30 is the most numerous with 4784

loan_data_f5 <- loan_data_f4 %>%
  mutate(
         Age_30_40 = ifelse(AgeGroup == "30-40", 1, 0),
         Age_40_50 = ifelse(AgeGroup == "40-50", 1, 0),
         Age_50_60 = ifelse(AgeGroup == "50-60", 1, 0),
         Age_60_70 = ifelse(AgeGroup == "60-70", 1, 0)
         )

View(loan_data_f5)


#Gender -----------

#Determining the base group by number of observations
gender_count <- loan_data_f5 %>%
  group_by(Gender) %>%
  summarise(Count = n())
print(gender_count)

#Pretty much the same number of observations
#Defining M as base

loan_data_f6 <- loan_data_f5 %>%
  mutate(female = ifelse(Gender == "F", 1, 0))


#Location -----------

#Determining the base group by number of observations
locations_count <- loan_data_f6 %>%
  group_by(Location) %>%
  summarise(Count = n())
print(locations_count)

# Rural       2432
# Semi-Urban 10307 **
# Urban       1243


loan_data_f7 <- loan_data_f6 %>%
  mutate(
    Location_Rural = ifelse(Location == "Rural", 1, 0),
    Location_Urban = ifelse(Location == "Urban", 1, 0)
  )

#Income Stability -----------

#Determining the base group by number of observations
count <- loan_data_f7 %>%
  group_by(Income.Stability) %>%
  summarise(Count = n())
print(count)

# High              1538
# Low              12444**

loan_data_f8 <- loan_data_f7 %>%
  mutate(
    stable_income = ifelse(Income.Stability == "High", 1, 0)
  )

# Has.Active.Credit.Card -----------

count <- loan_data_f8 %>%
  group_by(Has.Active.Credit.Card) %>%
  summarise(Count = n())
print(count)


# Active                  4513 **
# Inactive                4448
# Unknown                  647
# Unpossessed             4374

loan_data_f9 <- loan_data_f8 %>%
  mutate(
    CC_inactive = ifelse(Has.Active.Credit.Card == "Inactive", 1, 0),
    CC_unknown = ifelse(Has.Active.Credit.Card == "Unknown", 1, 0),
    CC_unpossessed = ifelse(Has.Active.Credit.Card == "Unpossessed", 1, 0)
  )

# Co.Applicant -----------

count <- loan_data_f9 %>%
  group_by(Co.Applicant) %>%
  summarise(Count = n())
print(count)


# Dependents -----------


loan_data_f10 <- loan_data_f9 %>%
  mutate(
    Dep_Low = ifelse(Dependents >= 0 & Dependents <= 2, 1, 0),
    Dep_High = ifelse(Dependents >= 3, 1, 0)
  )

counts <- loan_data_f10 %>%
  summarise(
    Low = sum(Dep_Low, na.rm = TRUE),
    High = sum(Dep_High, na.rm = TRUE)
  )

print(counts)


plot2 <- ggplot(data = loan_data_f4, aes(x = Dependents, y = Loan.Sanction.Amount..USD.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot with Linear Fit",
       x = "Dependents",
       y = "Loan Sanction Amount USD")

combined_plot <- (plot1 | plot2) 

print(combined_plot)

# Co.Applicant -----------

count <- loan_data_f9 %>%
  group_by(Co.Applicant) %>%
  summarise(Count = n())
print(count)


#Ratio Variables-------------

loan_data_f11 <- loan_data_f10 %>%
  mutate(
    Requested_to_Income = Loan.Amount.Request..USD. / Income..USD.,
    Expenses_to_Income = Current.Loan.Expenses..USD. / Income..USD.
  )


# D. Model Development ===========================================================
par(mfrow = c(2, 2))
# model 1

model1 <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD.
            ,data = loan_data_f11)

summary(model1)
plot(model1)


# model 2

model2 <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD. + Gender
             ,data = loan_data_f11)

summary(model2)
plot(model2)

# model 3

model3 <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD. + Gender + Location
             ,data = loan_data_f11)

summary(model3)
plot(model3)

# model 4

model4 <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD. + Gender + Location + Credit.Score
             ,data = loan_data_f11)

summary(model4)
plot(model4)


# model 5

model5 <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD. + Gender  + Credit.Score + Income..USD. + AgeGroup
             ,data = loan_data_f11)

summary(model5)
plot(model5)

# model 6

model6 <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD. + Gender  + Credit.Score + Income..USD. + AgeGroup + Income.Stability
             ,data = loan_data_f11)

summary(model6)
plot(model6)


# model 7

model7 <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD. + Gender  + Credit.Score + Income..USD. + Income.Stability
             ,data = loan_data_f11)

summary(model7)
plot(model7)



# model 8
model8 <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD. + Gender  + Credit.Score + Income..USD. + Income.Stability  + Co.Applicant + No..of.Defaults + Dep_High
             ,data = loan_data_f11)

summary(model8)
plot(model8)


#The residuals are not normally distributed
#Correcting by removing the extreme values
#=========================
cooksd <- data.frame(cook=cooks.distance(model8))
cooksd$obs<-rownames(cooksd)
ggplot(cooksd)+
  geom_bar(aes(x=obs, y=cook),stat = "identity",position=position_dodge())+
  geom_hline(yintercept=4*mean(cooksd$cook), color='red')+
  geom_text(data=subset(cooksd, cook > 4*mean(cooksd$cook)),
            aes(obs,cook,label=obs))

loan_data_f11 <- loan_data_f11 %>% 
  mutate(cook = cooksd$cook)

# Filter based on Cook's Distance
loan_data_f11_cook <- loan_data_f11 %>% 
  filter(cook <= 4 * mean(cook))
#==============================


model8_c <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD. + Gender  + Credit.Score + Income..USD. + Income.Stability  + Co.Applicant + No..of.Defaults + Dep_High
             ,data = loan_data_f11_cook)

summary(model8_c)


model9_c <- lm(Loan.Sanction.Amount..USD. ~ Loan.Amount.Request..USD. + Income..USD. + Credit.Score + Income..USD. + Income.Stability  + Co.Applicant + Dep_High
               ,data = loan_data_f11_cook)

summary(model9_c)


# E. Diagnostics Validation ===========================================================

#Residual Autocorrelation
durbinWatsonTest(model9_c)


vif(model9_c)

jarque.bera.test(residuals(model9_c)) # H0: normal




#White's heteroscedasticity test
#======================================================

residuals_squared <- resid(model9_c)^2
loan_data_f11_cook$residuals_squared <- residuals_squared

auxiliary_model <- lm(residuals_squared ~ Loan.Amount.Request..USD. +
                        I(Loan.Amount.Request..USD.^2) +
                        Credit.Score + I(Credit.Score^2) +
                        Income..USD. + I(Income..USD.^2) +
                        Income.Stability + Co.Applicant + Dep_High +
                        Loan.Amount.Request..USD. * Credit.Score +
                        Loan.Amount.Request..USD. * Income..USD. +
                        Credit.Score * Income..USD.,
                      data = loan_data_f11_cook)

summary(auxiliary_model)
#======================================================


#Logarithmisation=====================
# Logarithmisation for model9_c
model9_c_log <- lm(
  log(Loan.Sanction.Amount..USD.) ~ 
    log(Loan.Amount.Request..USD.) + 
    log(Credit.Score) + 
    log(Income..USD.) + 
    Income.Stability + 
    Co.Applicant + 
    Dep_High,
  data = loan_data_f11_cook
)

summary(model9_c_log)

plot(model9_c_log)



#White's heteroscedasticity test
#======================================================

residuals_squared_log <- resid(model9_c_log)^2
loan_data_f11_cook$residuals_squared_log <- residuals_squared_log

auxiliary_model_log <- lm(residuals_squared_log ~ 
                            log(Loan.Amount.Request..USD.) +
                            I(log(Loan.Amount.Request..USD.)^2) +
                            log(Credit.Score) + 
                            I(log(Credit.Score)^2) +
                            log(Income..USD.) + 
                            I(log(Income..USD.)^2) +
                            Income.Stability + 
                            Co.Applicant + 
                            Dep_High +
                            log(Loan.Amount.Request..USD.) * log(Credit.Score) +
                            log(Loan.Amount.Request..USD.) * log(Income..USD.) +
                            log(Credit.Score) * log(Income..USD.),
                          data = loan_data_f11_cook)

summary(auxiliary_model_log)

#======================================================




