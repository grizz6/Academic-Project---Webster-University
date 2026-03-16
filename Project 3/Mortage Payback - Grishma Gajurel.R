# GRISHMA GAJUREL - ANALYTICS PRACTICUM PROJECT 3

# install the required libraries

# install.packages("tidyverse")
# install.packages("ROSE")
# install.packages("corrplot")
# install.packages("ggplot2")
# install.packages("GGally")
# install.packages("moments")
# install.packages("lme4")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("caret")
# install.packages("readr")
# install.packages("randomForest")
# install.packages("xgboost")
# install.packages("MASS")
# install.packages("glmnet")
# install.packages("gbm")
# install.packages("factoextra")
# install.packages("cluster")
# install.packages("Amelia")
# install.packages("tibble")
# install.packages("pROC")
# install.packages("car")

# Load the libraries
library(tidyverse)
library(ROSE)
library(corrplot)
library(ggplot2)
library(GGally)
library(moments) 
library(lme4)
library(tidyr)
library(dplyr) 
library(psych)
library(caret)
library(readr)
library(randomForest)
library(xgboost)
library(MASS)
library(glmnet)
library(gbm)
library(factoextra)
library(cluster)
library(Amelia)
library(tibble)
library(pROC)
library(car)   


# Load dataset
mortgage_df <- read.csv("Mortgage.csv")

# Check structure and summary of data
str(mortgage_df)
summary(mortgage_df)

# Identify continuous variables
continuous_vars <- mortgage_df %>%
  select_if(is.numeric) %>%
  dplyr::select(-default_time, -payoff_time, -status_time, 
         -REtype_CO_orig_time, -REtype_PU_orig_time, 
         -REtype_SF_orig_time, -investor_orig_time)

# Generate summary statistics 
continuous_summary <- psych::describe(continuous_vars) %>%
  dplyr::select(vars, mean, sd, median, trimmed, min, max, range, skew, kurtosis)

# Print summary table
print(continuous_summary, row.names = FALSE)

# Check for missing values and NAs
missing_summary <- sapply(mortgage_df, function(x) sum(is.na(x)))
missing_summary <- data.frame(
  Variable = names(missing_summary),
  Missing_Count = missing_summary,
  Missing_Percent = round((missing_summary / nrow(mortgage_df)) * 100, 2)
)
print(missing_summary)

# Correlation Matrix for Numeric Predictors
numeric_vars <- mortgage_df %>% select_if(is.numeric)

# Remove ID/time variables that may distort correlation
numeric_vars <- numeric_vars %>% dplyr::select(
  -id, -time, -orig_time, -first_time, -mat_time
)

corr_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
corrplot(corr_matrix, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.5, title = "Correlation Matrix", mar = c(0,0,1,0))

# Identify potential multicollinearity issues
high_corr <- which(abs(corr_matrix) > 0.7 & abs(corr_matrix) < 1, arr.ind = TRUE)
if (nrow(high_corr) > 0.5) {
  cat("Highly correlated predictors:\n")
  print(high_corr)
}

# Boxplots to check distributions & outliers across target variable
# Example target variable: status_time (0=Active, 1=Default, 2=Payoff)

ggplot(mortgage_df, aes(x = factor(status_time), y = balance_time)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.6) +
  labs(title = "Loan Balance by Loan Status",
       x = "Loan Status (0=Active,1=Default,2=Payoff)",
       y = "Balance at Observation Time")

ggplot(mortgage_df, aes(x = factor(status_time), y = LTV_time)) +
  geom_boxplot(fill = "tan", color = "black", alpha = 0.6) +
  labs(title = "LTV Ratio by Loan Status",
       x = "Loan Status",
       y = "Loan-to-Value Ratio (%)")

ggplot(mortgage_df, aes(x = factor(status_time), y = FICO_orig_time)) +
  geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.6) +
  labs(title = "FICO Score by Loan Status",
       x = "Loan Status",
       y = "FICO Score")

# Pairwise Scatterplots
ggpairs(
  mortgage_df %>%
  dplyr::select(balance_orig_time, LTV_time, FICO_orig_time, interest_rate_time, hpi_time, default_time), # Takes time to plot
  title = "Pairwise Relationship"
)


# Handling multicollinearity

# Removing missing values
mortgage_df <- na.omit(mortgage_df)

# Select the key correlated predictors
selected_vars <- c(
  "balance_orig_time",
  "uer_time",
  "hpi_time",
  "balance_time",
  "hpi_orig_time",
  "REtype_SF_orig_time",
  "default_time",
  "status_time",
  "payoff_time"
)

# Subset dataset
mortgage_selected <- mortgage_df[, selected_vars]

# Define predictor matrix (x) and response variable (y)
x <- model.matrix(balance_time ~ ., data = mortgage_selected)[, -1]
y <- mortgage_selected$balance_time

# Ridge Regression (alpha = 0)
ridge_model <- glmnet(
  x,
  y,
  alpha = 0,           # Ridge regression
  standardize = TRUE
)

# Ridge Trace Plot
plot(ridge_model, xvar = "lambda", label = TRUE)
title("Ridge Plot", line = 2.5)

# Cross-validation
cv_ridge <- cv.glmnet(
  x,
  y,
  alpha = 0,
  standardize = TRUE,
  nfolds = 10,
  type.measure = "mse"
)

# Extract optimal lambda
best_lambda <- cv_ridge$lambda.min
cat("Optimal Lambda:", best_lambda, "\n")

# FINAL MODEL FIT
final_ridge <- glmnet(
  x,
  y,
  alpha = 0,
  lambda = best_lambda,
  standardize = TRUE
)

# Display shrunken coefficients
coef(final_ridge)

# DATA TRANSFORMATION

# remove some rows (esp NAs), create new ones (if possible), merge some of them as well (if possible), fix/remove some rows that are not ok 
# check the balance time, balance and the other predictors with status time and remove them likewise 

# Clean dataset
mortgage_df <- mortgage_df %>%
  filter(
    # Keep if (balance = 0 and status = 2)
    (balance_time == 0 & status_time == 2) |
      # Or if (balance > 0 for any status)
      (balance_time > 0)
  ) %>%
  filter(
    # Apply same logic for LTV ratio
    (LTV_time == 0 & status_time == 2) |
      (LTV_time > 0)
  ) %>%
  na.omit()


# Derive new feature
mortgage_df <- mortgage_df %>%
  mutate(
    loan_age = as.numeric(difftime(time, orig_time)),
    time_to_maturity = as.numeric(difftime(mat_time, time)),
    observation_lag = as.numeric(difftime(time, first_time))
  )

# Taking the last row for each ID as it captures the required information 
mortgage_df_last <- mortgage_df %>%
  group_by(id) %>%
  slice_tail(n = 1) %>% 
  ungroup()

# Data Partitioning (60% Train, 20% Validation, 20% Test)

set.seed(123)
# Training (60%) vs Temp (40%)
train_index <- createDataPartition(mortgage_df_last$default_time, p = 0.60, list = FALSE)
train_data <- mortgage_df_last[train_index, ]
temp_data <- mortgage_df_last[-train_index, ]

# Split Temp into Validation (20%) and Test (20%)
val_index <- createDataPartition(temp_data$default_time, p = 20/40, list = FALSE)
validation_data <- temp_data[val_index, ]
test_data <- temp_data[-val_index, ]

# Partition Summary
partition_summary <- data.frame(
  Dataset = c("Training", "Validation", "Test"),
  Observations = c(nrow(train_data), nrow(validation_data), nrow(test_data)),
  Percentage = round(c(
    nrow(train_data) / nrow(mortgage_df_last),
    nrow(validation_data) / nrow(mortgage_df_last),
    nrow(test_data) / nrow(mortgage_df_last)
  ) * 100, 2)
)

print(partition_summary)

# Identify numeric columns, excluding target variables
numeric_vars <- sapply(train_data, is.numeric)
numeric_vars[c("id", "REtype_CO_orig_time",  "REtype_CO_orig_time", "REtype_PU_orig_time",
               "REtype_SF_orig_time", "investor_orig_time",
               "status_time", "default_time", "payoff_time")] <- FALSE  # not scaling these as they are categorical and id is more an indication 
num_cols <- names(numeric_vars[numeric_vars])

# Scale numeric predictors
train_scaled <- train_data
train_scaled[num_cols] <- scale(train_data[num_cols])

validation_scaled <- validation_data
validation_scaled[num_cols] <- scale(validation_data[num_cols])

test_scaled <- test_data
test_scaled[num_cols] <- scale(test_data[num_cols])


mortgage_scaled <- mortgage_df_last
mortgage_scaled[num_cols] <- scale(mortgage_df_last[num_cols])

# Log Regression 

set.seed(123)
# Fixed Effect
# Model 1 - All predictors
log_model1 <- glm(default_time ~ time + orig_time + first_time + mat_time + balance_time + 
                    LTV_time + interest_rate_time + hpi_time + gdp_time + uer_time + 
                    REtype_CO_orig_time + REtype_PU_orig_time + REtype_SF_orig_time + 
                    FICO_orig_time + LTV_orig_time + Interest_Rate_orig_time + hpi_orig_time,
                  data = train_data, family = binomial)

summary(log_model1)

# Model 2 - Significant predictors from the model above
log_model2 <- glm(default_time ~ time + orig_time + first_time + 
                    LTV_time + interest_rate_time + 
                    gdp_time + uer_time + FICO_orig_time + LTV_orig_time,
                  data = train_data, family = binomial)

summary(log_model2)

# Function for precision, recall, and F1-score
get_metrics <- function(true, pred_prob, threshold = 0.5){
  pred_class <- ifelse(pred_prob > threshold, 1, 0)
  cm <- confusionMatrix(factor(pred_class),
                        factor(true),
                        positive = "1")
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1 <- cm$byClass["F1"]
  list(confusion_matrix = cm,
       precision = precision,
       recall = recall,
       f1 = f1)
}

# Applying function on models for train, test, and validation

# For Model 1
train_prob_m1 <- predict(log_model1, train_data, type="response")
val_prob_m1   <- predict(log_model1, validation_data, type="response")
test_prob_m1  <- predict(log_model1, test_data, type="response")

# For Model 2
train_prob_m2 <- predict(log_model2, train_data, type="response")
val_prob_m2   <- predict(log_model2, validation_data, type="response")
test_prob_m2  <- predict(log_model2, test_data, type="response")

# Metrics for all sets
# Model 1
m1_train_metrics <- get_metrics(train_data$default_time, train_prob_m1)
m1_val_metrics   <- get_metrics(validation_data$default_time, val_prob_m1)
m1_test_metrics  <- get_metrics(test_data$default_time, test_prob_m1)

# Model 2
m2_train_metrics <- get_metrics(train_data$default_time, train_prob_m2)
m2_val_metrics   <- get_metrics(validation_data$default_time, val_prob_m2)
m2_test_metrics  <- get_metrics(test_data$default_time, test_prob_m2)

# Results
m1_train_metrics
m1_val_metrics
m1_test_metrics

m2_train_metrics
m2_val_metrics
m2_test_metrics


# ROC & AUC plot for Validation and Test

# Model 1
# Validation
roc_m1_val  <- roc(validation_data$default_time, val_prob_m1)
auc_m1_val  <- auc(roc_m1_val)

plot(roc_m1_val, main = paste("ROC Curve — Model 1 (Validation) | AUC =", round(auc_m1_val, 4)))

# Test
roc_m1_test <- roc(test_data$default_time, test_prob_m1)
auc_m1_test <- auc(roc_m1_test)

plot(roc_m1_test, main = paste("ROC Curve — Model 1 (Test) | AUC =", round(auc_m1_test, 4)))

# Model 2
# Validation
roc_m2_val  <- roc(validation_data$default_time, val_prob_m2)
auc_m2_val  <- auc(roc_m2_val)

plot(roc_m2_val, main = paste("ROC Curve — Model 2 (Validation) | AUC =", round(auc_m2_val, 4)))

# Test
roc_m2_test <- roc(test_data$default_time, test_prob_m2)
auc_m2_test <- auc(roc_m2_test)

plot(roc_m2_test, main = paste("ROC Curve — Model 2 (Test) | AUC =", round(auc_m2_test, 4)))

# Creating table for all the output variables
eval_log_model <- function(model, model_name, dataset_name,
                           x_data, y_true){
  
  prob <- predict(model, x_data, type = "response")
  pred <- ifelse(prob >= 0.5, 1, 0)
  
  cm <- confusionMatrix(
    factor(pred),
    factor(y_true),
    positive = "1"
  )
  
  auc_val <- as.numeric(auc(y_true, prob))
  
  data.frame(
    Model = model_name,
    Dataset = dataset_name,
    Threshold = 0.5,
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    Precision = cm$byClass["Precision"],
    Recall = cm$byClass["Recall"],
    F1_Score = cm$byClass["F1"],
    AUC = auc_val
  )
}

log_results <- list()

# For MODEL 1
log_results[[1]] <- eval_log_model(log_model1, "Log Model 1", "Train",
                                   train_data, train_data$default_time)

log_results[[2]] <- eval_log_model(log_model1, "Log Model 1", "Validation",
                                   validation_data, validation_data$default_time)

log_results[[3]] <- eval_log_model(log_model1, "Log Model 1", "Test",
                                   test_data, test_data$default_time)

# For MODEL 2
log_results[[4]] <- eval_log_model(log_model2, "Log Model 2", "Train",
                                   train_data, train_data$default_time)

log_results[[5]] <- eval_log_model(log_model2, "Log Model 2", "Validation",
                                   validation_data, validation_data$default_time)

log_results[[6]] <- eval_log_model(log_model2, "Log Model 2", "Test",
                                   test_data, test_data$default_time)

log_final_table <- bind_rows(log_results) %>%
  mutate(across(where(is.numeric), round, 4))

print(log_final_table)

# Random Effect

set.seed(123)
# Fit mixed-effects logistic regression with random intercept for 'time' with train_data
log_model_mixed <- glmer(
  default_time ~  (1 | time) + orig_time + first_time + mat_time + balance_time + LTV_time + interest_rate_time +
    hpi_time + gdp_time + uer_time + REtype_CO_orig_time + REtype_PU_orig_time + REtype_SF_orig_time + 
    FICO_orig_time + LTV_orig_time + Interest_Rate_orig_time + hpi_orig_time,
  data = train_scaled,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

summary(log_model_mixed, correlation = FALSE)

# Another glmm model with significant predictors
log_model_mixed2 <- glmer(
  default_time ~  (1 | time) + first_time + mat_time + LTV_time + interest_rate_time +
    hpi_time + gdp_time + uer_time + 
    FICO_orig_time + LTV_orig_time + hpi_orig_time,
  data = train_scaled,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

summary(log_model_mixed2, correlation = FALSE)

# Another glmm model with more significant predictors
log_model_mixed3 <- glmer(
  default_time ~  (1 | time) + LTV_time + interest_rate_time +
    uer_time + FICO_orig_time + LTV_orig_time + hpi_orig_time,
  data = train_scaled,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

summary(log_model_mixed3, correlation = FALSE)

# Type III ANOVA (Chi-square) for each model
anova_m1_type <- Anova(log_model_mixed, type = 3, test = "Chisq")
anova_m2_type2 <- Anova(log_model_mixed2, type = 3, test = "Chisq")
anova_m3_type3 <- Anova(log_model_mixed3, type = 3, test = "Chisq")

print("Type III ANOVA (Chi-square) - Model 1")
print(anova_m1_type)
print("Type III ANOVA (Chi-square) - Model 2")
print(anova_m2_type2)
print("Type III ANOVA (Chi-square) - Model 3")
print(anova_m3_type3)

# Function for plotting and getting the metrices output
get_metrics_glmm <- function(true_vec, pred_prob, threshold = 0.5, pos = "1"){
  # ensure factors have consistent levels
  pred_class <- factor(ifelse(pred_prob > threshold, 1, 0), levels = c(0,1))
  true_fac   <- factor(as.character(true_vec), levels = c(0,1))
  cm <- confusionMatrix(pred_class, true_fac, positive = pos)
  precision <- cm$byClass["Precision"]
  recall    <- cm$byClass["Recall"]
  f1        <- cm$byClass["F1"]
  list(confusion_matrix = cm,
       precision = as.numeric(precision),
       recall = as.numeric(recall),
       f1 = as.numeric(f1))
}

get_metrics_glmm_035 <- function(true_vec, pred_prob, pos = "1"){
  # Create predicted classes using threshold = 0.35
  pred_class <- factor(ifelse(pred_prob > 0.35, 1, 0), levels = c(0,1))
  true_fac   <- factor(as.character(true_vec), levels = c(0,1))
  
  # Confusion matrix
  cm <- confusionMatrix(pred_class, true_fac, positive = pos)
  
  # Extract metrics
  precision <- cm$byClass["Precision"]
  recall    <- cm$byClass["Recall"]
  f1        <- cm$byClass["F1"]
  
  list(
    confusion_matrix = cm,
    precision = as.numeric(precision),
    recall = as.numeric(recall),
    f1 = as.numeric(f1)
  )
}

# Predictions (use allow.new.levels = TRUE for validation & test)

# Model 1: log_model_mixed
train_prob_m1 <- predict(log_model_mixed,  newdata = train_scaled,      type = "response")
val_prob_m1   <- predict(log_model_mixed,  newdata = validation_scaled, type = "response", allow.new.levels = TRUE)
test_prob_m1  <- predict(log_model_mixed,  newdata = test_scaled,       type = "response", allow.new.levels = TRUE)

# Model 2: log_model_mixed2
train_prob_m2 <- predict(log_model_mixed2, newdata = train_scaled,      type = "response")
val_prob_m2   <- predict(log_model_mixed2, newdata = validation_scaled, type = "response", allow.new.levels = TRUE)
test_prob_m2  <- predict(log_model_mixed2, newdata = test_scaled,       type = "response", allow.new.levels = TRUE)

# Model 3: log_model_mixed3
train_prob_m3 <- predict(log_model_mixed3, newdata = train_scaled,      type = "response")
val_prob_m3   <- predict(log_model_mixed3, newdata = validation_scaled, type = "response", allow.new.levels = TRUE)
test_prob_m3  <- predict(log_model_mixed3, newdata = test_scaled,       type = "response", allow.new.levels = TRUE)


# Metrics at threshold = 0.50 

# Model 1
m1_train_050 <- get_metrics_glmm(train_scaled$default_time,      train_prob_m1, threshold = 0.5)
m1_val_050   <- get_metrics_glmm(validation_scaled$default_time, val_prob_m1,   threshold = 0.5)
m1_test_050  <- get_metrics_glmm(test_scaled$default_time,       test_prob_m1,  threshold = 0.5)

# Model 2
m2_train_050 <- get_metrics_glmm(train_scaled$default_time,      train_prob_m2, threshold = 0.5)
m2_val_050   <- get_metrics_glmm(validation_scaled$default_time, val_prob_m2,   threshold = 0.5)
m2_test_050  <- get_metrics_glmm(test_scaled$default_time,       test_prob_m2,  threshold = 0.5)

# Model 3
m3_train_050 <- get_metrics_glmm(train_scaled$default_time,      train_prob_m3, threshold = 0.5)
m3_val_050   <- get_metrics_glmm(validation_scaled$default_time, val_prob_m3,   threshold = 0.5)
m3_test_050  <- get_metrics_glmm(test_scaled$default_time,       test_prob_m3,  threshold = 0.5)


# Metrics at threshold = 0.35 
# Model 1
m1_train_035 <- get_metrics_glmm_035(train_scaled$default_time,      train_prob_m1)
m1_val_035   <- get_metrics_glmm_035(validation_scaled$default_time, val_prob_m1)
m1_test_035  <- get_metrics_glmm_035(test_scaled$default_time,       test_prob_m1)

# Model 2
m2_train_035 <- get_metrics_glmm_035(train_scaled$default_time,      train_prob_m2)
m2_val_035   <- get_metrics_glmm_035(validation_scaled$default_time, val_prob_m2)
m2_test_035  <- get_metrics_glmm_035(test_scaled$default_time,       test_prob_m2)

# Model 3
m3_train_035 <- get_metrics_glmm_035(train_scaled$default_time,      train_prob_m3)
m3_val_035   <- get_metrics_glmm_035(validation_scaled$default_time, val_prob_m3)
m3_test_035  <- get_metrics_glmm_035(test_scaled$default_time,       test_prob_m3)

# Print concise summaries
# Showing all the result in a table

print_model_metrics <- function(
    model_name,
    train_050, val_050, test_050,
    train_035, val_035, test_035
){
  
  cat("\n==============================\n")
  cat("MODEL:", model_name, "\n")
  cat("==============================\n\n")
  
  # Threshold = 0.50
  cat("---- Threshold = 0.50 ----\n\n")
  
  cat("Train confusion matrix:\n")
  print(train_050$confusion_matrix)
  cat("\nValidation confusion matrix:\n")
  print(val_050$confusion_matrix)
  cat("\nTest confusion matrix:\n")
  print(test_050$confusion_matrix)
  
  cat("\nMetrics (0.50):\n")
  cat("Train: Precision =", round(train_050$precision,4),
      "Recall =", round(train_050$recall,4),
      "F1 =", round(train_050$f1,4), "\n")
  cat("Val:   Precision =", round(val_050$precision,4),
      "Recall =", round(val_050$recall,4),
      "F1 =", round(val_050$f1,4), "\n")
  cat("Test:  Precision =", round(test_050$precision,4),
      "Recall =", round(test_050$recall,4),
      "F1 =", round(test_050$f1,4), "\n\n")
  
  
  # Threshold = 0.35
  cat("---- Threshold = 0.35 ----\n\n")
  
  cat("Train confusion matrix:\n")
  print(train_035$confusion_matrix)
  cat("\nValidation confusion matrix:\n")
  print(val_035$confusion_matrix)
  cat("\nTest confusion matrix:\n")
  print(test_035$confusion_matrix)
  
  cat("\nMetrics (0.35):\n")
  cat("Train: Precision =", round(train_035$precision,4),
      "Recall =", round(train_035$recall,4),
      "F1 =", round(train_035$f1,4), "\n")
  cat("Val:   Precision =", round(val_035$precision,4),
      "Recall =", round(val_035$recall,4),
      "F1 =", round(val_035$f1,4), "\n")
  cat("Test:  Precision =", round(test_035$precision,4),
      "Recall =", round(test_035$recall,4),
      "F1 =", round(test_035$f1,4), "\n\n")
}


# Call function for all 3 models

print_model_metrics(
  model_name = "MODEL 1",
  train_050 = m1_train_050, val_050 = m1_val_050, test_050 = m1_test_050,
  train_035 = m1_train_035, val_035 = m1_val_035, test_035 = m1_test_035
)

print_model_metrics(
  model_name = "MODEL 2",
  train_050 = m2_train_050, val_050 = m2_val_050, test_050 = m2_test_050,
  train_035 = m2_train_035, val_035 = m2_val_035, test_035 = m2_test_035
)

print_model_metrics(
  model_name = "MODEL 3",
  train_050 = m3_train_050, val_050 = m3_val_050, test_050 = m3_test_050,
  train_035 = m3_train_035, val_035 = m3_val_035, test_035 = m3_test_035
)


# Unified ROC + AUC Plot Function (Validation & Test)

plot_roc_with_auc <- function(true_vals, pred_probs, model_name, dataset_name){
  roc_obj <- roc(true_vals, pred_probs)
  auc_val <- auc(roc_obj)
  
  plot(
    roc_obj,
    main = paste0(model_name, " — ", dataset_name,
                  " | AUC = ", round(auc_val, 4))
  )
  
  return(roc_obj)
}


# ROC & AUC Plots for all models

# Model 1
roc_m1_val  <- plot_roc_with_auc(validation_scaled$default_time, val_prob_m1,  "Model 1", "Validation")
roc_m1_test <- plot_roc_with_auc(test_scaled$default_time,        test_prob_m1, "Model 1", "Test")

# Model 2
roc_m2_val  <- plot_roc_with_auc(validation_scaled$default_time, val_prob_m2,  "Model 2", "Validation")
roc_m2_test <- plot_roc_with_auc(test_scaled$default_time,        test_prob_m2, "Model 2", "Test")

# Model 3
roc_m3_val  <- plot_roc_with_auc(validation_scaled$default_time, val_prob_m3,  "Model 3", "Validation")
roc_m3_test <- plot_roc_with_auc(test_scaled$default_time,        test_prob_m3, "Model 3", "Test")


# Printing all AUC values for all models

cat("AUCs:\n")
cat("Model 1  | Val:", round(auc(roc_m1_val),4),  " Test:", round(auc(roc_m1_test),4), "\n")
cat("Model 2  | Val:", round(auc(roc_m2_val),4),  " Test:", round(auc(roc_m2_test),4), "\n")
cat("Model 3  | Val:", round(auc(roc_m3_val),4),  " Test:", round(auc(roc_m3_test),4), "\n")


# Metrics Table for All Models & Thresholds

# Helper function to extract metrics from previous objects
extract_metrics <- function(model_name, dataset_name, metrics_050, metrics_035, auc_val){
  auc_num <- as.numeric(auc_val)  # convert AUC object to numeric
  tibble(
    Model       = model_name,
    Dataset     = dataset_name,
    Threshold   = c(0.5, 0.35),
    Accuracy    = c(
      metrics_050$confusion_matrix$overall["Accuracy"],
      metrics_035$confusion_matrix$overall["Accuracy"]
    ),
    Sensitivity = c(
      metrics_050$confusion_matrix$byClass["Sensitivity"],
      metrics_035$confusion_matrix$byClass["Sensitivity"]
    ),
    Specificity = c(
      metrics_050$confusion_matrix$byClass["Specificity"],
      metrics_035$confusion_matrix$byClass["Specificity"]
    ),
    Precision   = c(metrics_050$precision, metrics_035$precision),
    Recall      = c(metrics_050$recall, metrics_035$recall),
    F1_Score    = c(metrics_050$f1, metrics_035$f1),
    AUC         = auc_num
  )
}

# Compute AUCs
auc_m1_val  <- auc(roc(validation_scaled$default_time, val_prob_m1))
auc_m1_test <- auc(roc(test_scaled$default_time,       test_prob_m1))
auc_m2_val  <- auc(roc(validation_scaled$default_time, val_prob_m2))
auc_m2_test <- auc(roc(test_scaled$default_time,       test_prob_m2))
auc_m3_val  <- auc(roc(validation_scaled$default_time, val_prob_m3))
auc_m3_test <- auc(roc(test_scaled$default_time,       test_prob_m3))

# Combine metrics for all models, datasets, thresholds
results_table <- bind_rows(
  extract_metrics("Model 1", "Train",  m1_train_050, m1_train_035, auc(roc(train_scaled$default_time, train_prob_m1))),
  extract_metrics("Model 1", "Validation", m1_val_050, m1_val_035, auc(roc(validation_scaled$default_time, val_prob_m1))),
  extract_metrics("Model 1", "Test", m1_test_050, m1_test_035, auc(roc(test_scaled$default_time, test_prob_m1))),
  
  extract_metrics("Model 2", "Train",  m2_train_050, m2_train_035, auc(roc(train_scaled$default_time, train_prob_m2))),
  extract_metrics("Model 2", "Validation", m2_val_050, m2_val_035, auc(roc(validation_scaled$default_time, val_prob_m2))),
  extract_metrics("Model 2", "Test", m2_test_050, m2_test_035, auc(roc(test_scaled$default_time, test_prob_m2))),
  
  extract_metrics("Model 3", "Train",  m3_train_050, m3_train_035, auc(roc(train_scaled$default_time, train_prob_m3))),
  extract_metrics("Model 3", "Validation", m3_val_050, m3_val_035, auc(roc(validation_scaled$default_time, val_prob_m3))),
  extract_metrics("Model 3", "Test", m3_test_050, m3_test_035, auc(roc(test_scaled$default_time, test_prob_m3)))
)

# Round numeric columns for readability
results_table <- results_table %>%
  mutate(across(c(Accuracy, Sensitivity, Specificity, Precision, Recall, F1_Score, AUC), ~ round(., 4)))

# View metrics table
print(results_table)




# Bagging and Boosting 


# Bagging model: ensemble method

set.seed(123)
# Split predictors and target
x_train <- train_data %>% dplyr::select(c(-default_time, -payoff_time, -status_time))
y_train <- as.factor(train_data$default_time)

x_valid <- validation_data %>% dplyr::select(c(-default_time, -payoff_time, -status_time))
y_valid <- as.factor(validation_data$default_time)

x_test  <- test_data %>% dplyr::select(c(-default_time, -payoff_time, -status_time))
y_test  <- as.factor(test_data$default_time)

# Bagging using random forest (all features sampled)
set.seed(123)
bag_model <- randomForest(x = x_train, y = y_train, mtry = ncol(x_train), ntree = 200) 
bag_model

bag_model2 <- randomForest(x = x_train, y = y_train, mtry = 15, ntree = 200) 
bag_model2

# Function to evaluate the performance with custom threshold
evaluate_oob <- function(model, threshold = 0.5, dataset_name = "OOB") {
  
  # Extract probabilities and true classes
  oob_prob <- model$votes[,2]
  true_classes <- model$y
  
  # Apply threshold to get predictions
  oob_pred <- factor(as.numeric(oob_prob > threshold), levels = c(0, 1))
  
  # Confusion matrix
  conf <- confusionMatrix(
    oob_pred,
    factor(true_classes, levels = c("0", "1")),
    positive = "1"
  )
  
  # Extract metrics directly from confusion matrix
  precision <- conf$byClass["Precision"]
  recall <- conf$byClass["Recall"]
  f1 <- conf$byClass["F1"]
  
  # Calculate AUC
  auc_val <- auc(true_classes, oob_prob)
  
  # Print results
  cat("\n==========", dataset_name, "PERFORMANCE (Threshold =", threshold, ") ==========\n")
  print(conf)
  cat("\nPrecision:", precision,
      "\nRecall:", recall,
      "\nF1 Score:", f1,
      "\nROC-AUC:", auc_val, "\n")
  
  return(list(conf = conf, precision = precision, recall = recall, f1 = f1, auc = auc_val))
}

# Function to evaluate validation/test performance with custom threshold and optional ROC plot
evaluate_external <- function(model, x_data, y_data, threshold = 0.5, dataset_name = "Dataset") {
  
  # Get predictions and probabilities
  pred_prob <- predict(model, newdata = x_data, type = "prob")[,2]
  
  # Apply threshold to get class predictions
  pred_class <- factor(as.numeric(pred_prob > threshold), levels = c(0, 1))
  
  # Confusion matrix
  conf <- confusionMatrix(
    pred_class,
    factor(y_data, levels = c("0", "1")),
    positive = "1"
  )
  
  # Extract metrics directly from confusion matrix
  precision <- conf$byClass["Precision"]
  recall <- conf$byClass["Recall"]
  f1 <- conf$byClass["F1"]
  auc_val <- auc(y_data, pred_prob)
  
  # Print results
  cat("\n==========", dataset_name, "PERFORMANCE (Threshold =", threshold, ") ==========\n")
  print(conf)
  cat("\nPrecision:", precision,
      "\nRecall:", recall,
      "\nF1 Score:", f1,
      "\nROC-AUC:", auc_val, "\n")
  
  return(list(conf = conf, precision = precision, recall = recall, f1 = f1, auc = auc_val, prob = pred_prob))
}

# Define thresholds to evaluate
thresholds <- c(0.5, 0.35)

# Evaluate both models at both thresholds
for (th in thresholds) {
  
  # Evaluate Model 1 (mtry = all features)
  cat("\n\n########## MODEL 1: BAGGING (mtry = all) | Threshold =", th, "##########\n")
  results1_oob <- evaluate_oob(bag_model, threshold = th, "OOB")
  results1_valid <- evaluate_external(bag_model, x_valid, y_valid, threshold = th, "Validation")
  results1_test <- evaluate_external(bag_model, x_test, y_test, threshold = th, "Test")
  
  # Plot ROC curves for Model 1
  roc1_valid <- roc(y_valid, results1_valid$prob)
  plot(roc1_valid, main = paste("ROC Curve - Model 1 Validation | Threshold =", th, "| AUC =", round(results1_valid$auc, 4)))
  
  roc1_test <- roc(y_test, results1_test$prob)
  plot(roc1_test, main = paste("ROC Curve - Model 1 Test | Threshold =", th, "| AUC =", round(results1_test$auc, 4)))
  
  # Evaluate Model 2 (mtry = 15)
  cat("\n\n########## MODEL 2: BAGGING (mtry = 15) | Threshold =", th, "##########\n")
  results2_oob <- evaluate_oob(bag_model2, threshold = th, "OOB")
  results2_valid <- evaluate_external(bag_model2, x_valid, y_valid, threshold = th, "Validation")
  results2_test <- evaluate_external(bag_model2, x_test, y_test, threshold = th, "Test")
  
  # Plot ROC curves for Model 2
  roc2_valid <- roc(y_valid, results2_valid$prob)
  plot(roc2_valid, main = paste("ROC Curve - Model 2 Validation | Threshold =", th, "| AUC =", round(results2_valid$auc, 4)))
  
  roc2_test <- roc(y_test, results2_test$prob)
  plot(roc2_test, main = paste("ROC Curve - Model 2 Test | Threshold =", th, "| AUC =", round(results2_test$auc, 4)))
}

# Helper function to extract metrics from results object
extract_bag_metrics <- function(model_name, dataset_name, results_obj) {
  tibble(
    Model       = model_name,
    Dataset     = dataset_name,
    Threshold   = results_obj$threshold, # we will set it manually
    Accuracy    = results_obj$conf$overall["Accuracy"],
    Sensitivity = results_obj$conf$byClass["Sensitivity"],
    Specificity = results_obj$conf$byClass["Specificity"],
    Precision   = results_obj$precision,
    Recall      = results_obj$recall,
    F1_Score    = results_obj$f1,
    AUC         = as.numeric(results_obj$auc)
  )
}

# Initialize empty table
bagging_results <- tibble()

# Define thresholds
thresholds <- c(0.5, 0.35)

# Loop over thresholds and models
for(th in thresholds) {
  
  # Model 1: mtry = all features
  res1_oob <- evaluate_oob(bag_model, threshold = th, "OOB")
  res1_oob$threshold <- th
  res1_valid <- evaluate_external(bag_model, x_valid, y_valid, threshold = th, "Validation")
  res1_valid$threshold <- th
  res1_test <- evaluate_external(bag_model, x_test, y_test, threshold = th, "Test")
  res1_test$threshold <- th
  
  bagging_results <- bind_rows(
    bagging_results,
    extract_bag_metrics("Bagging Model 1", "Train", res1_oob),
    extract_bag_metrics("Bagging Model 1", "Validation", res1_valid),
    extract_bag_metrics("Bagging Model 1", "Test", res1_test)
  )
  
  # Model 2: mtry = 15
  res2_oob <- evaluate_oob(bag_model2, threshold = th, "OOB")
  res2_oob$threshold <- th
  res2_valid <- evaluate_external(bag_model2, x_valid, y_valid, threshold = th, "Validation")
  res2_valid$threshold <- th
  res2_test <- evaluate_external(bag_model2, x_test, y_test, threshold = th, "Test")
  res2_test$threshold <- th
  
  bagging_results <- bind_rows(
    bagging_results,
    extract_bag_metrics("Bagging Model 2", "Train", res2_oob),
    extract_bag_metrics("Bagging Model 2", "Validation", res2_valid),
    extract_bag_metrics("Bagging Model 2", "Test", res2_test)
  )
}

# Round metrics for readability
bagging_results <- bagging_results %>%
  mutate(across(c(Accuracy, Sensitivity, Specificity, Precision, Recall, F1_Score, AUC), ~ round(., 4)))

# View final Bagging results table
print(bagging_results)


# Boosting using gradient boosting

set.seed(123)
# Combine into one dataset for gbm
train_gbm <- data.frame(x_train, default_time = as.numeric(as.character(y_train)))

# Check imbalance ratio
table(train_gbm$default_time)

# Train Gradient Boosting Model
gbm_model <- gbm(
  formula = default_time ~ .,
  distribution = "bernoulli",    # for binary classification
  data = train_gbm,
  n.trees = 300,                  
  interaction.depth = 3,          
  shrinkage = 0.05,              
  n.minobsinnode = 10,           
  bag.fraction = 0.7,             
  verbose = FALSE
)

summary(gbm_model)

# Model using the whole dataset that was reduced, as boosting uses weights on misclassified instances 
x_train2 <- mortgage_df_last %>% dplyr::select(c(-default_time, -payoff_time, -status_time))
y_train2 <- as.factor(mortgage_df_last$default_time)

train_gbm2 <- data.frame(x_train2, default_time = as.numeric(as.character(y_train2)))

gbm_model2 <- gbm(
  formula = default_time ~ .,
  distribution = "bernoulli",    # for binary classification
  data = train_gbm2,
  n.trees = 300,                  
  interaction.depth = 3,         
  shrinkage = 0.05,               
  n.minobsinnode = 10,           
  bag.fraction = 0.7,             
  verbose = FALSE
)

summary(gbm_model2)

# Predict probabilities using gmb_model2
test_prob <- predict(gbm_model2, newdata = test_data, type = "response")

# Convert probabilities to class labels using threshold 0.5
test_pred <- ifelse(test_prob >= 0.5, 1, 0)

# Generate confusion matrix
confusionMatrix(factor(test_pred), factor(test_data$default_time))

# Performing on the train, test, and validation data 
# Function to evaluate GBM performance with custom threshold
evaluate_gbm <- function(model, x_data, y_data, n_trees = 300, threshold = 0.5, dataset_name = "Dataset") {
  
  # Get predicted probabilities
  pred_prob <- predict(model, newdata = x_data, n.trees = n_trees, type = "response")
  
  # Apply threshold to get class predictions
  pred_class <- factor(as.numeric(pred_prob > threshold), levels = c(0, 1))
  
  # Confusion matrix
  conf <- confusionMatrix(
    pred_class,
    factor(y_data, levels = c("0", "1")),
    positive = "1"
  )
  
  # Extract metrics directly from confusion matrix
  precision <- conf$byClass["Precision"]
  recall <- conf$byClass["Recall"]
  f1 <- conf$byClass["F1"]
  auc_val <- auc(y_data, pred_prob)
  
  # Print results
  cat("\n==========", dataset_name, "PERFORMANCE (Threshold =", threshold, ") ==========\n")
  print(conf)
  cat("\nPrecision:", precision,
      "\nRecall:", recall,
      "\nF1 Score:", f1,
      "\nROC-AUC:", auc_val, "\n")
  
  return(list(conf = conf, precision = precision, recall = recall, f1 = f1, auc = auc_val, prob = pred_prob))
}

# Define thresholds to evaluate
thresholds <- c(0.5, 0.35)

# Define optimal number of trees to use
n_trees_opt <- 300

# Evaluate GBM at both thresholds
for (th in thresholds) {
  
  cat("\n\n########## GRADIENT BOOSTING MODEL | Threshold =", th, "##########\n")
  
  # Evaluate on all datasets
  results_train <- evaluate_gbm(gbm_model, x_train, y_train, n_trees = n_trees_opt, threshold = th, "Train")
  results_valid <- evaluate_gbm(gbm_model, x_valid, y_valid, n_trees = n_trees_opt, threshold = th, "Validation")
  results_test <- evaluate_gbm(gbm_model, x_test, y_test, n_trees = n_trees_opt, threshold = th, "Test")
  
  # Plot ROC curves
  roc_train <- roc(y_train, results_train$prob)
  plot(roc_train, main = paste("ROC Curve - GBM Train | Threshold =", th, "| AUC =", round(results_train$auc, 4)))
  
  roc_valid <- roc(y_valid, results_valid$prob)
  plot(roc_valid, main = paste("ROC Curve - GBM Validation | Threshold =", th, "| AUC =", round(results_valid$auc, 4)))
  
  roc_test <- roc(y_test, results_test$prob)
  plot(roc_test, main = paste("ROC Curve - GBM Test | Threshold =", th, "| AUC =", round(results_test$auc, 4)))
}

# Helper function to extract metrics from GBM results
extract_gbm_metrics <- function(model_name, dataset_name, results_obj, threshold) {
  tibble(
    Model       = model_name,
    Dataset     = dataset_name,
    Threshold   = threshold,
    Accuracy    = results_obj$conf$overall["Accuracy"],
    Sensitivity = results_obj$conf$byClass["Sensitivity"],
    Specificity = results_obj$conf$byClass["Specificity"],
    Precision   = results_obj$precision,
    Recall      = results_obj$recall,
    F1_Score    = results_obj$f1,
    AUC         = as.numeric(results_obj$auc)
  )
}

# Initialize empty table
boosting_results <- tibble()

# Define thresholds
thresholds <- c(0.5, 0.35)

# Optimal number of trees
n_trees_opt <- 300

# Loop over thresholds
for(th in thresholds) {
  
  # Evaluate GBM on Train, Validation, Test
  res_train <- evaluate_gbm(gbm_model, x_train, y_train, n_trees = n_trees_opt, threshold = th, "Train")
  res_valid <- evaluate_gbm(gbm_model, x_valid, y_valid, n_trees = n_trees_opt, threshold = th, "Validation")
  res_test  <- evaluate_gbm(gbm_model, x_test, y_test, n_trees = n_trees_opt, threshold = th, "Test")
  
  # Add results to table
  boosting_results <- bind_rows(
    boosting_results,
    extract_gbm_metrics("Gradient Boosting", "Train", res_train, th),
    extract_gbm_metrics("Gradient Boosting", "Validation", res_valid, th),
    extract_gbm_metrics("Gradient Boosting", "Test", res_test, th)
  )
}

# Round numeric columns for readability
boosting_results <- boosting_results %>%
  mutate(across(c(Accuracy, Sensitivity, Specificity, Precision, Recall, F1_Score, AUC), ~ round(., 4)))

# View final Boosting results table
print(boosting_results)


# K-means Clustering

set.seed(123)

# Function: Elbow Plot (WSS)     

plot_elbow <- function(data, max_k = 15) {
  wss <- sapply(1:max_k, function(k) {
    kmeans(data, centers = k, nstart = 25)$tot.withinss
  })
  
  ggplot(data.frame(K = 1:max_k, WSS = wss), aes(K, WSS)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(size = 3, color = "darkred") +
    labs(title = "Elbow Method for Optimal K",
         x = "Number of Clusters (K)",
         y = "Within-Cluster Sum of Squares") +
    theme_minimal()
}

# Function: Run K-means + Summaries

run_kmeans <- function(data, k = 3) {
  
  km <- kmeans(data, centers = k, nstart = 25)
  
  # CLARA for silhouette (handles large data)
  clara_model <- clara(data, k = k, samples = 50)
  sil <- clara_model$silinfo$widths
  avg_sil <- clara_model$silinfo$avg.width
  
  sil_by_cluster <- aggregate(
    sil[, "sil_width"],
    by = list(Cluster = sil[, "cluster"]),
    FUN = mean
  )
  colnames(sil_by_cluster)[2] <- "Average_Silhouette"
  
  # Metrics
  cohesion  <- km$tot.withinss
  separation <- km$betweenss
  ratio <- separation / km$totss
  
  list(
    model = km,
    silhouette_values = sil,
    avg_silhouette = avg_sil,
    sil_by_cluster = sil_by_cluster,
    cohesion = cohesion,
    separation = separation,
    ratio = ratio
  )
}

# Function: Silhouette Plot (CLARA)

plot_silhouette <- function(sil_data, k) {
  sil_df <- as.data.frame(sil_data)
  
  ggplot(sil_df, aes(x = cluster, y = sil_width, group = cluster, fill = factor(cluster))) +
    geom_boxplot(alpha = 0.7, show.legend = FALSE) +
    labs(
      title = paste("Silhouette Width Distribution (CLARA) — K =", k),
      x = "Cluster",
      y = "Silhouette Width"
    ) +
    theme_minimal()
}

# Analysis for all the variables

numeric_data <- mortgage_scaled %>%
  dplyr::select(where(is.numeric), -id)

# Elbow plot
plot_elbow(numeric_data)

# Run model
res1 <- run_kmeans(numeric_data, k = 3)

# Print summaries
cat("Average Silhouette:", round(res1$avg_silhouette, 3), "\n\n")
print(res1$sil_by_cluster)

cat("\nCohesion:", round(res1$cohesion, 3), "\n")
cat("Separation:", round(res1$separation, 3), "\n")
cat("BSS/TSS Ratio:", round(res1$ratio, 3), "\n\n")

cat("Cluster Sizes:\n")
print(res1$model$size)

cat("\nCluster Centers:\n")
print(res1$model$centers)

# Plot silhouette
plot_silhouette(res1$silhouette_values, 3)

# Cluster visualization
fviz_cluster(
  res1$model,
  data = numeric_data,
  geom = "point",
  ellipse.type = "norm",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "K-means Clustering (K = 3)"
)


# Second model with few required variables based on log regression
cluster_vars <- mortgage_scaled %>%
  dplyr::select(LTV_time, interest_rate_time, FICO_orig_time,
         LTV_orig_time, balance_time, gdp_time)

# Elbow for subset
plot_elbow(cluster_vars)

# Run model
res2 <- run_kmeans(cluster_vars, k = 3)

# Print summaries
cat("Average Silhouette (Subset):", round(res2$avg_silhouette, 3), "\n\n")
print(res2$sil_by_cluster)

cat("\nCohesion:", round(res2$cohesion, 3), "\n")
cat("Separation:", round(res2$separation, 3), "\n")
cat("BSS/TSS Ratio:", round(res2$ratio, 3), "\n\n")

cat("Cluster Sizes:\n")
print(res2$model$size)

cat("\nCluster Centers:\n")
print(res2$model$centers)

# Plot silhouette
plot_silhouette(res2$silhouette_values, 3)

# Cluster visualization
fviz_cluster(
  res2$model,
  data = cluster_vars,
  geom = "point",
  ellipse.type = "norm",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "K-means Clustering (Subset Variables)"
)


# Third model: time based varriables
cluster_vars_time <- mortgage_scaled %>%
  dplyr::select(loan_age, time_to_maturity, observation_lag, balance_time)

# Elbow for time-based variables
plot_elbow(cluster_vars_time, max_k = 10)

# Run model
res3 <- run_kmeans(cluster_vars_time, k = 2)

# Print summaries
cat("Average Silhouette (Time-Based):", round(res3$avg_silhouette, 2), "\n\n")
print(res3$sil_by_cluster)
cat("\nCohesion:", round(res3$cohesion, 2), "\n")
cat("Separation:", round(res3$separation, 2), "\n")
cat("BSS/TSS Ratio:", round(res3$ratio, 2), "\n\n")
cat("Cluster Sizes:\n")
print(res3$model$size)
cat("\nCluster Centers:\n")
print(res3$model$centers)

# Plot silhouette
plot_silhouette(res3$silhouette_values, 2)

# Cluster visualization
fviz_cluster(
  res3$model,
  data = cluster_vars_time,
  geom = "point",
  ellipse.type = "norm",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "K-means Clustering (Time-Based Variables)"
)



# 4th model for borrower risk
# Groups borrowers by their initial risk characteristics at origination
# Focuses on creditworthiness, leverage, pricing, and borrower type
cluster_vars_risk <- mortgage_scaled %>%
  dplyr::select(FICO_orig_time, LTV_orig_time, Interest_Rate_orig_time, 
                investor_orig_time, balance_orig_time)

# Elbow for risk profile
plot_elbow(cluster_vars_risk, max_k = 10)

# Run model
res4 <- run_kmeans(cluster_vars_risk, k = 2)

# Print summaries
cat("Average Silhouette (Borrower Risk Profile):", round(res4$avg_silhouette, 2), "\n\n")
print(res4$sil_by_cluster)
cat("\nCohesion:", round(res4$cohesion, 2), "\n")
cat("Separation:", round(res4$separation, 2), "\n")
cat("BSS/TSS Ratio:", round(res4$ratio, 2), "\n\n")
cat("Cluster Sizes:\n")
print(res4$model$size)
cat("\nCluster Centers:\n")
print(res4$model$centers)

# Plot silhouette
plot_silhouette(res4$silhouette_values, 2)

# Cluster visualization
fviz_cluster(
  res4$model,
  data = cluster_vars_risk,
  geom = "point",
  ellipse.type = "norm",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "K-means Clustering (Borrower Risk Profile at Origination)"
)


# 5th model for comprehensive risk
# Comprehensive risk model combining borrower quality, current loan status,
# loan maturity, and macroeconomic environment
# Provides holistic view of loan risk at observation time
cluster_vars_comprehensive <- mortgage_scaled %>%
  dplyr::select(FICO_orig_time, LTV_time, interest_rate_time, 
                balance_time, loan_age, uer_time, hpi_time)

# Elbow for comprehensive model
plot_elbow(cluster_vars_comprehensive, max_k = 10)

# Run model
res5 <- run_kmeans(cluster_vars_comprehensive, k = 3)

# Print summaries
cat("Average Silhouette (Comprehensive Risk):", round(res5$avg_silhouette, 3), "\n\n")
print(res5$sil_by_cluster)
cat("\nCohesion:", round(res5$cohesion, 3), "\n")
cat("Separation:", round(res5$separation, 3), "\n")
cat("BSS/TSS Ratio:", round(res5$ratio, 3), "\n\n")
cat("Cluster Sizes:\n")
print(res5$model$size)
cat("\nCluster Centers:\n")
print(res5$model$centers)

# Plot silhouette
plot_silhouette(res5$silhouette_values, 3)

# Cluster visualization
fviz_cluster(
  res5$model,
  data = cluster_vars_comprehensive,
  geom = "point",
  ellipse.type = "norm",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "K-means Clustering (Comprehensive Risk Assessment)"
)

# Run model for comprehensive risk with k = 2
res51 <- run_kmeans(cluster_vars_comprehensive, k = 2)

# Print summaries
cat("Average Silhouette (Comprehensive Risk):", round(res5$avg_silhouette, 2), "\n\n")
print(res51$sil_by_cluster)
cat("\nCohesion:", round(res51$cohesion, 2), "\n")
cat("Separation:", round(res51$separation, 2), "\n")
cat("BSS/TSS Ratio:", round(res51$ratio, 2), "\n\n")
cat("Cluster Sizes:\n")
print(res51$model$size)
cat("\nCluster Centers:\n")
print(res51$model$centers)

# Plot silhouette
plot_silhouette(res5$silhouette_values, 2)

# Cluster visualization
fviz_cluster(
  res51$model,
  data = cluster_vars_comprehensive,
  geom = "point",
  ellipse.type = "norm",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "K-means Clustering (Comprehensive Risk Assessment)"
)


# Helper function to extract K-means results into a table row
extract_kmeans_metrics <- function(res, model_name, k) {
  # Silhouette by cluster
  sil_by_cluster <- res$sil_by_cluster %>%
    rename(Average_Silhouette = Average_Silhouette) %>%
    mutate(Cluster = paste0("C", Cluster))
  
  # Flatten cluster centers for table display
  centers_flat <- as.data.frame(res$model$centers)
  centers_flat <- centers_flat %>%
    mutate(Cluster = paste0("C", row_number())) %>%
    tidyr::pivot_longer(-Cluster, names_to = "Variable", values_to = "Center")
  
  # Cluster sizes
  cluster_sizes <- tibble(
    Cluster = paste0("C", 1:length(res$model$size)),
    Size = res$model$size
  )
  
  list(
    Model = model_name,
    K = k,
    Avg_Silhouette = round(res$avg_silhouette, 4),
    Cohesion = round(res$cohesion, 4),
    Separation = round(res$separation, 4),
    BSS_TSS_Ratio = round(res$ratio, 4),
    Silhouette_By_Cluster = sil_by_cluster,
    Cluster_Sizes = cluster_sizes,
    Cluster_Centers = centers_flat
  )
}

# Collect all models
kmeans_summaries <- list(
  extract_kmeans_metrics(res1, "Full Variable Set", 3),
  extract_kmeans_metrics(res2, "Subset Variables", 3),
  extract_kmeans_metrics(res3, "Time-Based Variables", 2),
  extract_kmeans_metrics(res4, "Borrower Risk Profile", 2),
  extract_kmeans_metrics(res5, "Comprehensive Risk", 3),
  extract_kmeans_metrics(res51, "Comprehensive Risk (K=2)", 2)
)

# Print overview table
overview_table <- tibble(
  Model = sapply(kmeans_summaries, function(x) x$Model),
  K = sapply(kmeans_summaries, function(x) x$K),
  Avg_Silhouette = sapply(kmeans_summaries, function(x) x$Avg_Silhouette),
  Cohesion = sapply(kmeans_summaries, function(x) x$Cohesion),
  Separation = sapply(kmeans_summaries, function(x) x$Separation),
  BSS_TSS_Ratio = sapply(kmeans_summaries, function(x) x$BSS_TSS_Ratio)
)

print(overview_table)

# Print detailed cluster info for each model
for(i in seq_along(kmeans_summaries)) {
  cat("MODEL:", kmeans_summaries[[i]]$Model, "| K =", kmeans_summaries[[i]]$K, "\n")
  cat("\nAverage Silhouette (Overall):", kmeans_summaries[[i]]$Avg_Silhouette, "\n")
  cat("Cohesion:", kmeans_summaries[[i]]$Cohesion, "\n")
  cat("Separation:", kmeans_summaries[[i]]$Separation, "\n")
  cat("BSS/TSS Ratio:", kmeans_summaries[[i]]$BSS_TSS_Ratio, "\n\n")
  
  cat("Cluster Sizes:\n")
  print(kmeans_summaries[[i]]$Cluster_Sizes)
  
  cat("\nAverage Silhouette by Cluster:\n")
  print(kmeans_summaries[[i]]$Silhouette_By_Cluster)
  
  cat("\nCluster Centers:\n")
  print(kmeans_summaries[[i]]$Cluster_Centers)
  cat("\n#######################\n")
}


