#PART A: Logistic regression analysis the dataset. Validate assumptions, evaluate with a confusion matrix and ROC curve, and interpret the results. 

install.packages("haven")
install.packages("maxLik")
install.packages("tidyverse")
install.packages("ggplot2")
# Load necessary libraries
library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(dplyr)
setwd('D:\\R Studio')

#Reading the data
install.packages('xlsx')
library('xlsx')
df<-read.xlsx('Pumpkin_Seeds_Dataset.xlsx')

df_clean=na.omit(df)

df_clean$Class=ifelse(df$clean$Class=='Çerçevelik',1,0)

# Split the data into features (X) and target variable (y)
X <- df_clean %>% select(Class)
y <- df_clean$responded

# Split the data into training and testing sets
set.seed(42)

# Determine the indices for the training set
train_indices <- sample(seq_len(nrow(X)), size = 0.8 * nrow(X))

# Split the data into training and testing sets
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Fit logistic regression model
logistic_model <- glm(responded ~ ., data = cbind(X_train, responded = y_train), family = 'binomial')

# Print summary of the logistic regression model
print(summary(logistic_model))

# Make predictions on the test set
y_pred_proba <- predict(logistic_model, newdata = X_test, type = 'response')
y_pred <- ifelse(y_pred_proba > 0.5, 1, 0)

# 1. Create confusion matrix
conf_matrix <- confusionMatrix(factor(y_pred), factor(y_test))
print(conf_matrix)

# Plot ROC curve
roc_curve <- roc(y_test, y_pred_proba)
plot(roc_curve, main = 'ROC Curve for Logistic Regression')
auc_value <- auc(roc_curve)
print(paste('AUC:', auc_value))

# Fit decision tree model
tree_model <- rpart(responded ~ ., data = cbind(X_train, responded = y_train), method = 'class')

# Plot decision tree
rpart.plot(tree_model, main = 'Decision Tree for CHD Prediction')

# Make predictions using the decision tree
y_pred_tree <- predict(tree_model, newdata = X_test, type = 'class')

# Create confusion matrix for decision tree
conf_matrix_tree <- confusionMatrix(factor(y_pred_tree), factor(y_test))
print(conf_matrix_tree)

# Calculate ROC curve for decision tree
y_pred_proba_tree <- predict(tree_model, newdata = X_test, type = 'prob')[,2]
roc_curve_tree <- roc(y_test, y_pred_proba_tree)
plot(roc_curve_tree, main = 'ROC Curve for Decision Tree')
auc_value_tree <- auc(roc_curve_tree)
print(paste('AUC (Decision Tree):', auc_value_tree))


# 2. Perform a probit regression on "NSSO68.csv" to identify non-vegetarians. 

# Load the dataset
data_nss <- read.csv("NSSO68.csv")
# Create a binary variable for chicken consumption
data_nss$chicken_q <- ifelse(data_nss$chicken_q > 0, 1, 0)

# Verify the creation of 'chicken_binary'
table(data_nss$chicken_q)

# Probit regression model
probit_model <- glm(chicken_q ~ Age + Marital_Status + Education, data = data_nss, family = binomial(link = "probit"))

# Summary of the probit regression model
summary(probit_model)

#3 
# Load necessary libraries
library(dplyr)
library(haven)
library(maxLik)

# Load the data
data <- read.csv('NSSO68.csv', stringsAsFactors = FALSE)

# Subset data 
df <- data %>%
  select(MPCE_URP, Whether_owns_any_land, hhdsz, Religion, Social_Group, Regular_salary_earner)

# Check for missing values
print(sum(is.na(df$MPCE_URP)))
print(sum(is.na(df$Whether_owns_any_land)))
print(sum(is.na(df$hhdsz)))
print(sum(is.na(df$Religion)))
print(sum(is.na(df$Social_Group)))
print(sum(is.na(df$Regular_salary_earner)))

# Impute missing values for selected columns
columns_to_impute <- c('Whether_owns_any_land', 'Religion', 'Social_Group', 'Regular_salary_earner')

# Assuming using mode for imputation for categorical variables
for (col in columns_to_impute) {
  mode_value <- as.character(sort(table(df[[col]]), decreasing = TRUE)[1])
  df[[col]][is.na(df[[col]])] <- mode_value
}

# Drop rows with any remaining NaN values
df <- na.omit(df)

# Check for missing values
print(sum(is.na(df$MPCE_URP)))
print(sum(is.na(df$Whether_owns_any_land)))
print(sum(is.na(df$hhdsz)))
print(sum(is.na(df$Religion)))
print(sum(is.na(df$Social_Group)))
print(sum(is.na(df$Regular_salary_earner)))

# Convert the target variable to binary based on the specified condition
df$MPCE_URP <- ifelse(df$MPCE_URP < 380, 0, 1)

# Define the independent variables (X) and the dependent variable (y)
X <- df %>%
  select(Whether_owns_any_land, hhdsz, Religion, Social_Group, Regular_salary_earner)
X <- cbind(1, C)  # Add a constant term for the intercept
y <- df$MPCE_URP

# Define the Tobit model function
tobit_loglike <- function(params) {
  beta <- params[1:(length(params)-1)]
  sigma <- params[length(params)]
  XB <- as.matrix(X) %*% beta
  cens <- (y == 0) + (y == 1)
  uncens <- 1 - cens
  ll <- numeric(length(y))
  
  ll[cens == 1] <- log(dnorm(y[cens == 1], mean = XB[cens == 1], sd = sigma))
  ll[uncens == 1] <- log(dnorm(y[uncens == 1], mean = XB[uncens == 1], sd = sigma))
  
  return(-sum(ll))
}

# Initial parameter guesses
start_params <- c(rep(0, ncol(X)), 1)

# Fit the Tobit model
tobit_results <- maxLik(tobit_loglike, start = start_params, method = "BFGS")

# Print the summary of the model
summary(tobit_results)
