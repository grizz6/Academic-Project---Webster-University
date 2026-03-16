# GRISHMA GAJUREL

#### Install the required libraries ####

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
# install.packages("VIM")
# install.packages("reshape2")
# install.packages("plotly")
# install.packages("gridExtra")
# install.packages("RColorBrewer")
# install.packages("ggcorrplot")
# install.packages("mice")
# install.packages("Hmisc")
# install.packages("class")
# install.packages("patchwork")
# install.packages("e1071")
# install.packages("neuralnet")
# install.packages("NbClust")
# install.packages("NeuralNetTools")
# install.packages("broom")


#### Load the libraries ####

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
library(VIM)
library(reshape2)
library(plotly)
library(gridExtra)
library(RColorBrewer)
library(ggcorrplot)
library(mice)
library(Hmisc)
library(class)
library(patchwork)
library(e1071)
library(neuralnet)
library(NbClust)
library(NeuralNetTools)
library(broom)



#### PROJECT 1 #####

set.seed(123) # for reproducibility 

# Load the dataset
data <- read.csv("used_device_data.csv")

# Display data structure
cat("\n=== DATA STRUCTURE ===\n")
str(data)

# Display summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
summary(data)

# Select only continuous variables
continuous_data <- data %>% 
  dplyr::select(where(is.numeric)) %>%    # as MASS also has select 
  dplyr::select(-release_year)

# Generate descriptive statistics only for continuous variables
desc_continuous <- psych::describe(continuous_data)

# View results
print(desc_continuous)

# Count missing values
missing_counts <- sapply(data, function(x) sum(is.na(x)))
missing_percentages <- round((missing_counts / nrow(data)) * 100, 2)

# Summary table
missing_summary <- data.frame(
  Variable = names(missing_counts),
  Missing_Count = missing_counts,
  Missing_Percentage = missing_percentages
)

print(missing_summary[missing_summary$Missing_Count > 0, ])

# Check for zero values in key numeric variables
cat("\n=== ZERO VALUES ANALYSIS ===\n")
numeric_cols <- c("rear_camera_mp", "front_camera_mp", "internal_memory", "ram", "battery")
zero_analysis <- data.frame(
  Variable = numeric_cols,
  Zero_Count = sapply(numeric_cols, function(x) sum(data[[x]] == 0, na.rm = TRUE)),
  Zero_Percentage = sapply(numeric_cols, function(x) round((sum(data[[x]] == 0, na.rm = TRUE) / nrow(data)) * 100, 2))
)
print(zero_analysis[zero_analysis$Zero_Count > 0, ])

# Create descriptive statistics for continuous variables
continuous_vars <- c("screen_size", "rear_camera_mp", "front_camera_mp", 
                     "internal_memory", "ram", "battery", "weight", 
                     "days_used", "normalized_used_price", "normalized_new_price")

# Detailed statistics using psych package
cat("DETAILED DESCRIPTIVE STATISTICS FOR CONTINUOUS VARIABLES:\n")
data %>%
  dplyr::select(screen_size, rear_camera_mp, front_camera_mp, internal_memory, ram, battery, weight,
                days_used, normalized_used_price) %>%
  tidyr::pivot_longer(everything()) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "black") +
  facet_wrap(~name, scales = "free") +
  labs(title = "Distributions of Key Variables") +
  theme_minimal() +
  theme(legend.position = "none")


# Brand distribution
brand_freq <- table(data$device_brand)
brand_pct <- round(prop.table(brand_freq) * 100, 2)
brand_dist <- data.frame(
  Brand = names(brand_freq),
  Frequency = as.numeric(brand_freq),
  Percentage = as.numeric(brand_pct)
)

# Arrange in descending order by Frequency
brand_dist <- brand_dist[order(-brand_dist$Frequency), ]

cat("BRAND DISTRIBUTION:\n")
print(brand_dist)

# Operating system distribution
os_freq <- table(data$os)
print(data.frame(OS = names(os_freq), 
                 Frequency = as.numeric(os_freq),
                 Percentage = round(as.numeric(os_freq)/sum(os_freq) * 100, 2)))

# Network system distribution
net5g_freq <- table(data$X5g)
net4g_freq <- table(data$X4g)

net_dist <- rbind(
  data.frame(Network = paste("4G", names(net4g_freq)),
             Frequency = as.numeric(net4g_freq),
             Percentage = round(as.numeric(net4g_freq) / sum(net4g_freq) * 100, 2)),
  data.frame(Network = paste("5G", names(net5g_freq)),
             Frequency = as.numeric(net5g_freq),
             Percentage = round(as.numeric(net5g_freq) / sum(net5g_freq) * 100, 2))
)

print(net_dist)

# Release year distribution
cat("\nRELEASE YEAR DISTRIBUTION:\n")
year_freq <- table(data$release_year)
print(data.frame(Year = names(year_freq), 
                 Frequency = as.numeric(year_freq),
                 Percentage = round(as.numeric(year_freq)/sum(year_freq) * 100, 2)))

# Correlation plot
numeric_df <- data[sapply(data, is.numeric)]
cor_matrix <- cor(numeric_df, use = "complete.obs")

ggcorrplot(cor_matrix, 
           method = "square",      
           type = "lower",         
           lab = TRUE,              
           lab_size = 3, 
           colors = c("yellow", "white", "orange"), 
           title = "Correlation Heatmap of Numeric Features",
           ggtheme = ggplot2::theme_minimal,
           tl.cex = 10,             
           tl.srt = 90)           

# Boxplot visualization (you may have to zoom on the plot section as sometimes it shows and sometime it doesn't)
req_vars <- c("screen_size","rear_camera_mp","front_camera_mp", "days_used", "normalized_used_price",
              "internal_memory","ram","battery","weight", "normalized_new_price")

df_long <- data %>%
  pivot_longer(cols = all_of(req_vars), names_to = "Variable", values_to = "Value")

ggplot(df_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, outlier.color = "black") +
  facet_wrap(~Variable, scales = "free", ncol = 4) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 10, face = "bold")) +
  labs(title = "Box Plots for Outlier Detection", x = "Variables", y = "Values")

# Visualize missing data patterns
if(sum(missing_counts) > 0) {
  aggr(
    data,
    sortVars = TRUE,        
    prop = FALSE,
    labels = colnames(data),       
    numbers = TRUE,        
    cex.axis = 0.45,              
    cex.numbers = 0.8,  
    gap = 1,                     
    col = c("steelblue", "maroon"),   
    ylab = c("Number of missings", "Missing Data Pattern")
  )
}


# Missing Data Imputation 

# Data assessment
missing_cols <- c("weight", "battery", "ram", "internal_memory", "front_camera_mp", "rear_scamera_mp")

# Removing rows where ram & Battery are missing/NA
initial_rows <- nrow(data)
data_cleaned <- data %>%
  filter(!(is.na(ram) & is.na(battery)))

final_rows <- nrow(data_cleaned)
removed_rows <- initial_rows - final_rows

# Function to impute missing values based on brand and specifications
impute_by_brand_specs <- function(data, column_name) {
  
  # Create a copy of the data for imputation
  data_temp <- data
  
  # Impute based on same brand and similar specifications
  for(i in which(is.na(data_temp[[column_name]]))) {
    current_brand <- data_temp$device_brand[i]
    current_year <- data_temp$release_year[i]
    
    # Look for similar devices
    similar_devices <- data_temp %>%
      filter(
        device_brand == current_brand,
        abs(release_year - current_year) <= 2,
        !is.na(.data[[column_name]])
      )
    
    if(nrow(similar_devices) > 0) {
      # Use median of similar devices
      data_temp[[column_name]][i] <- median(similar_devices[[column_name]], na.rm = TRUE)
    } else {
      # Use brand median
      brand_median <- data_temp %>%
        filter(device_brand == current_brand, !is.na(.data[[column_name]])) %>%
        pull(.data[[column_name]]) %>%
        median(na.rm = TRUE)
      
      if(!is.na(brand_median)) {
        data_temp[[column_name]][i] <- brand_median
      } else {
        # Use overall median
        overall_median <- median(data_temp[[column_name]], na.rm = TRUE)
        data_temp[[column_name]][i] <- overall_median
      }
    }
  }
  
  return(data_temp[[column_name]])
}

# Function to impute camera values with same logic
impute_camera_values <- function(data, camera_col) {
  data_temp <- data
  
  for(i in which(is.na(data_temp[[camera_col]]))) {
    current_brand <- data_temp$device_brand[i]
    current_year <- data_temp$release_year[i]
    current_price <- data_temp$normalized_new_price[i]
    
    # Find similar devices based on brand, year, and price range
    price_range <- current_price * 0.2  
    
    similar_devices <- data_temp %>%
      filter(
        device_brand == current_brand,
        abs(release_year - current_year) <= 2,
        abs(normalized_new_price - current_price) <= price_range,
        !is.na(.data[[camera_col]])
      )
    
    if(nrow(similar_devices) > 0) {
      data_temp[[camera_col]][i] <- median(similar_devices[[camera_col]], na.rm = TRUE)
    } else {
      brand_year_median <- data_temp %>%
        filter(
          device_brand == current_brand,
          abs(release_year - current_year) <= 3,
          !is.na(.data[[camera_col]])
        ) %>%
        pull(.data[[camera_col]]) %>%
        median(na.rm = TRUE)
      
      if(!is.na(brand_year_median)) {
        data_temp[[camera_col]][i] <- brand_year_median
      } else {
        # Use overall median
        overall_median <- median(data_temp[[camera_col]], na.rm = TRUE)
        data_temp[[camera_col]][i] <- overall_median
      }
    }
  }
  
  return(data_temp[[camera_col]])
}


# Make a copy of cleaned data for imputation
data_imputed <- data_cleaned

# Impute RAM
data_imputed$ram <- impute_by_brand_specs(data_imputed, "ram")

# Impute Battery
data_imputed$battery <- impute_by_brand_specs(data_imputed, "battery")

# Impute Internal Memory
data_imputed$internal_memory <- impute_by_brand_specs(data_imputed, "internal_memory")

# Impute Weight 
data_imputed$weight <- impute_by_brand_specs(data_imputed, "weight")

# Impute Camera values
data_imputed$front_camera_mp <- impute_camera_values(data_imputed, "front_camera_mp")
data_imputed$rear_camera_mp <- impute_camera_values(data_imputed, "rear_camera_mp")

# Check remaining missing values
final_missing <- data.frame(
  Variable = missing_cols,
  Missing_Count_Before = sapply(missing_cols, function(x) sum(is.na(data_cleaned[[x]]))),
  Missing_Count_After = sapply(missing_cols, function(x) sum(is.na(data_imputed[[x]])))
)

print(final_missing)


# IQR handling

df <- data_imputed
vars <- c("screen_size","rear_camera_mp","front_camera_mp",
          "internal_memory","ram","battery","weight", "normalized_new_price")

# Record before
before <- df[, vars]

iqr_vars <- c("screen_size", "battery", "weight")

for (v in iqr_vars) {
  Q1 <- quantile(df[[v]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[v]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  # Find outliers
  outlier_idx <- which(df[[v]] < lower | df[[v]] > upper)
  
  # Replace outliers with median
  if (length(outlier_idx) > 0) {
    replacement_val <- median(df[[v]][df[[v]] >= lower & df[[v]] <= upper], na.rm = TRUE)
    df[[v]][outlier_idx] <- replacement_val
  }
}

df$rear_camera_mp[df$rear_camera_mp >= 48] <- 48
df$front_camera_mp[df$front_camera_mp == 0] <- 0.3
df$internal_memory[df$internal_memory >= 512] <- 512
df$ram[df$ram >= 12] <- 12

# Record after
after <- df[, vars]

# Summary function
sumtab <- function(x) c(Min = min(x, na.rm=TRUE),
                        Q1 = quantile(x, .25, na.rm=TRUE),
                        Median = median(x, na.rm=TRUE),
                        Mean = mean(x, na.rm=TRUE),
                        Q3 = quantile(x, .75, na.rm=TRUE),
                        Max = max(x, na.rm=TRUE))

cat("\n--- BEFORE ---\n"); print(round(t(sapply(before, sumtab)), 3))

cat("\n--- AFTER ----\n"); print(round(t(sapply(after, sumtab)), 3))

# Final cleaned dataset
data_outlier_handled <- df

# Create Price Retention Ratio 
data_outlier_handled$PriceRetention <- data_outlier_handled$normalized_used_price / 
  data_outlier_handled$normalized_new_price

# Handle division by zero or NA
data_outlier_handled$PriceRetention[!is.finite(data_outlier_handled$PriceRetention)] <- NA

# Binary Classification
median_retention <- median(data_outlier_handled$PriceRetention, na.rm = TRUE)

data_outlier_handled$PriceRetention <- ifelse(
  data_outlier_handled$PriceRetention > median_retention, 
  "Premium Device", 
  "Non-Premium Device"
)

data_outlier_handled$PriceRetention <- factor(data_outlier_handled$PriceRetention)


# Check distribution
table(data_outlier_handled$PriceRetention)

# Checking the multicollinearity
ols_before <- lm(normalized_used_price ~ .,
                 data = data)
vif(ols_before)

# Ridge for handling multicollinearity

# Ridge on the 4 collinear predictors based on the output above
predictors <- c("weight", "battery", "release_year", "screen_size") 

x_all <- model.matrix(normalized_used_price ~ ., 
                      data = data_outlier_handled[, c(predictors, "normalized_used_price")])[,-1]
y_all <- data_outlier_handled$normalized_used_price


ridge_cv <- cv.glmnet(
  x_all, y_all,
  alpha = 0,                                      
  lambda = 10^seq(4, -2, length = 100),
  nfolds = 10,
  standardize = TRUE
)

ridge_best_lambda <- ridge_cv$lambda.min

# Ridge shrinkage path plot
plot(ridge_cv$glmnet.fit, xvar = "lambda")
abline(v = log(ridge_best_lambda), col = "red", lty = 2)

# Checking if the multicollinearity is handled or not
ols_model <- lm(normalized_used_price ~ weight + battery + release_year + screen_size,
                data = data_outlier_handled)
vif(ols_model)

# First split: Training (70%) vs Temp (30%)
train_index <- createDataPartition(data_outlier_handled$PriceRetention, p = 0.70, list = FALSE)
train_data <- data_outlier_handled[train_index, ]
temp_data <- data_outlier_handled[-train_index, ]

# Split the temp set into Validation (10%) and Test (20%)
val_index <- createDataPartition(temp_data$PriceRetention, p = 10/30, list = FALSE)
validation_data <- temp_data[val_index, ]
test_data <- temp_data[-val_index, ]

# Partition Summary 
partition_summary <- data.frame(
  Dataset = c("Training", "Validation", "Test"),
  Observations = c(nrow(train_data), nrow(validation_data), nrow(test_data)),
  Percentage = round(c(
    nrow(train_data) / nrow(data_outlier_handled),
    nrow(validation_data) / nrow(data_outlier_handled),
    nrow(test_data) / nrow(data_outlier_handled)
  ) * 100, 2)
)

print(partition_summary)

# Regression with numeric variables
set.seed(123) # for reproducibility 

#  Keeping numeric variables
numeric_vars <- sapply(train_data, is.numeric)
train_data_num <- train_data[, numeric_vars]
validation_data_num <- validation_data[, numeric_vars]
test_data_num <- test_data[, numeric_vars]

# Multilinear Regression (OLS) on normalized used price
ols_model <- lm(normalized_used_price ~ ., data = train_data_num)
summary(ols_model)

# Multilinear Regression (OLS) on normalized used price with significant predictors
ols_model_2 <- lm(normalized_used_price ~ screen_size + rear_camera_mp + front_camera_mp + internal_memory + battery + release_year,
                  data = train_data_num)
summary(ols_model_2)

# Multilinear Regression (OLS) on normalized used price with significant predictors
ols_model_3 <- lm(normalized_used_price ~ rear_camera_mp + front_camera_mp + battery + release_year,
                  data = train_data_num)
summary(ols_model_3)

# Multilinear Regression (OLS) on normalized used price with significant predictors and higher slope
ols_model_4 <- lm(normalized_used_price ~ screen_size + rear_camera_mp + front_camera_mp + battery + release_year + normalized_new_price,
                  data = train_data_num)
summary(ols_model_4)

# Multilinear Regression (OLS) on normalized used price with significant predictors and higher slope
ols_model_5 <- lm(normalized_used_price ~ front_camera_mp + battery + release_year + normalized_new_price,
                  data = train_data_num)
summary(ols_model_5)

# Multilinear Regression (OLS) on normalized used price with significant predictors and higher slope
ols_model_6 <- lm(normalized_used_price ~ screen_size + rear_camera_mp + front_camera_mp + battery + release_year,
                  data = train_data_num)
summary(ols_model_6)

# Multilinear Regression (OLS) on normalized used price with significant predictors and higher slope
ols_model_7 <- lm(normalized_used_price ~ rear_camera_mp + front_camera_mp + battery,
                  data = train_data_num)
summary(ols_model_7)

# Multilinear Regression (OLS) on normalized used price with significant predictors
ols_model_8 <- lm(normalized_used_price ~  rear_camera_mp + front_camera_mp + battery,
                  data = train_data_num)
summary(ols_model_8)

# Multilinear Regression (OLS) on normalized new price
ols_model_new <- lm(normalized_new_price ~ ., data = train_data_num)
summary(ols_model_new)

# Multilinear Regression (OLS) on normalized used price with significant predictors
ols_model_new2 <- lm(normalized_new_price ~ screen_size + rear_camera_mp + front_camera_mp + ram + internal_memory + weight + battery + release_year + days_used,
                     data = train_data_num)
summary(ols_model_new2)

# Multilinear Regression (OLS) on normalized used price with significant predictors
ols_model_new3 <- lm(normalized_new_price ~ rear_camera_mp + front_camera_mp + ram + internal_memory + weight + battery + release_year,
                     data = train_data_num)
summary(ols_model_new3)

# Full model evaluation 

# Store all models
model_list <- list(
  ols_model = ols_model,
  ols_model_2 = ols_model_2,
  ols_model_3 = ols_model_3,
  ols_model_4 = ols_model_4,
  ols_model_5 = ols_model_5,
  ols_model_6 = ols_model_6,
  ols_model_7 = ols_model_7,
  ols_model_8 = ols_model_8,
  ols_model_new = ols_model_new,
  ols_model_new2 = ols_model_new2,
  ols_model_new3 = ols_model_new3
)

# Metric functions
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

r2 <- function(actual, predicted) {
  1 - sum((actual - predicted)^2, na.rm = TRUE) /
    sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
}

model_p_value <- function(model) {
  fstat <- summary(model)$fstatistic
  pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
}

# Initialize results table
results <- data.frame()

# Loop through all models and evaluate
for (model_name in names(model_list)) {
  
  model <- model_list[[model_name]]
  
  # Identify response variable automatically
  response <- all.vars(formula(model))[1]
  
  # Predictions
  train_pred <- predict(model, train_data_num)
  val_pred   <- predict(model, validation_data_num)
  test_pred  <- predict(model, test_data_num)
  
  # Actual values
  y_train <- train_data_num[[response]]
  y_val   <- validation_data_num[[response]]
  y_test  <- test_data_num[[response]]
  
  # Compute metrics
  train_rmse <- rmse(y_train, train_pred)
  val_rmse   <- rmse(y_val, val_pred)
  test_rmse  <- rmse(y_test, test_pred)
  
  train_r2 <- r2(y_train, train_pred)
  val_r2   <- r2(y_val, val_pred)
  test_r2  <- r2(y_test, test_pred)
  
  multiple_r2 <- summary(model)$r.squared
  
  # Store in results table
  results <- rbind(results, data.frame(
    Model = model_name,
    Train_RMSE = train_rmse,
    Validation_RMSE = val_rmse,
    Test_RMSE = test_rmse,
    Train_R2 = train_r2,
    Validation_R2 = val_r2,
    Test_R2 = test_r2,
    Multiple_R2 = multiple_r2
  ))
}

# Round for clean viewing
results[, -1] <- round(results[, -1], 5)

# Print final comparison table
print(results)


# Set up 2x2 plotting area
par(mfrow = c(2, 2))

# Plot diagnostics
plot(ols_model)

# Adding title
mtext("OLS Model Diagnostics", outer = TRUE, line = -1.5, cex = 1.5)

# Plot
par(mfrow = c(1, 1))

# List of significant predictors
signif_vars <- c("screen_size", "rear_camera_mp", "front_camera_mp",
                 "battery", "internal_memory", "ram", "normalized_new_price")

# Create a list to store plots
plot_list <- list()

# Loop through each significant predictor
for (var in signif_vars) {
  p <- ggplot(data_outlier_handled, aes_string(x = var, y = "normalized_used_price")) +
    geom_point(alpha = 0.4, color = "black") +
    geom_smooth(method = "lm", se = TRUE, color = "red", lwd = 0.5) +
    labs(
      x = var,
      y = "normalized_used_price") +
    theme_minimal()
  
  plot_list[[var]] <- p
}

# Combine all plots into a matrix 
combined_plot <- wrap_plots(plot_list, ncol = 3) +
  plot_annotation(
    title = "Relationships Between Significant Predictors and Normalized Used Price"
  )

combined_plot


# Classification with ALL variables (KNN + Naive Bayes)
set.seed(123) # for reproducibility 

# Prepare classification
train_class <- train_data
validation_class <- validation_data
test_class <- test_data

train_class$PriceRetention <- factor(train_class$PriceRetention)
validation_class$PriceRetention <- factor(validation_class$PriceRetention,
                                          levels = levels(train_class$PriceRetention))
test_class$PriceRetention <- factor(test_class$PriceRetention,
                                    levels = levels(train_class$PriceRetention))

# Identify predictors 
predictors_all <- setdiff(names(train_class), c("PriceRetention", "normalized_used_price", "normalized_new_price"))

# scale numeric predictors
preproc <- preProcess(train_class[, predictors_all], method = c("center", "scale"))

train_class_scaled <- predict(preproc, train_class)
validation_class_scaled <- predict(preproc, validation_class)
test_class_scaled <- predict(preproc, test_class)

# Identify numeric predictors 
numeric_predictors <- names(train_class_scaled)[sapply(train_class_scaled, is.numeric)]
numeric_predictors <- setdiff(numeric_predictors, c("normalized_used_price", "normalized_new_price"))  # exclude numeric target

# Build KNN matrices
x_train_class <- as.matrix(train_class_scaled[, numeric_predictors])
y_train_class <- train_class_scaled$PriceRetention

x_val_class <- as.matrix(validation_class_scaled[, numeric_predictors])
y_val_class <- validation_class_scaled$PriceRetention

x_test_class <- as.matrix(test_class_scaled[, numeric_predictors])
y_test_class <- test_class_scaled$PriceRetention


# KNN Classification
k_values <- 1:30
knn_results <- data.frame(k = k_values, ValAccuracy = NA)

for (i in seq_along(k_values)) {
  knn_pred <- knn(train = x_train_class, test = x_val_class,
                  cl = y_train_class, k = k_values[i])
  knn_results$ValAccuracy[i] <- mean(knn_pred == y_val_class)
}

# Best k from validation
best_k <- knn_results$k[which.max(knn_results$ValAccuracy)]
cat("Best k chosen from validation:", best_k, "\n")

# Final predictions
knn_train_pred <- knn(x_train_class, x_train_class, y_train_class, k = best_k)
knn_val_pred   <- knn(x_train_class, x_val_class,   y_train_class, k = best_k)
knn_test_pred  <- knn(x_train_class, x_test_class,  y_train_class, k = best_k)

# Confusion matrixes
knn_conf_train <- confusionMatrix(factor(knn_train_pred, levels = levels(y_train_class)), y_train_class)
knn_conf_val   <- confusionMatrix(factor(knn_val_pred,   levels = levels(y_val_class)),   y_val_class)
knn_conf_test  <- confusionMatrix(factor(knn_test_pred,  levels = levels(y_test_class)),  y_test_class)

cat("\nKNN TRAIN PERFORMANCE:\n"); print(knn_conf_train)
cat("\nKNN VALIDATION PERFORMANCE:\n"); print(knn_conf_val)
cat("\nKNN TEST PERFORMANCE:\n"); print(knn_conf_test)


# Naive Bayes Classification
nb_model <- naiveBayes(PriceRetention ~ ., data = train_class_scaled[, c(predictors_all, "PriceRetention")])

nb_train_pred <- predict(nb_model, train_class_scaled)
nb_conf_train <- confusionMatrix(nb_train_pred, y_train_class)
nb_conf_train

# Predictions
nb_train_pred <- predict(nb_model, train_class_scaled)
nb_val_pred   <- predict(nb_model, validation_class_scaled)
nb_test_pred  <- predict(nb_model, test_class_scaled)

# Confusion Matrices
nb_conf_train <- confusionMatrix(nb_train_pred, y_train_class)
nb_conf_val   <- confusionMatrix(nb_val_pred,   y_val_class)
nb_conf_test  <- confusionMatrix(nb_test_pred,  y_test_class)

cat("\nNAIVE BAYES TRAIN PERFORMANCE:\n"); print(nb_conf_train)
cat("\nNAIVE BAYES VALIDATION PERFORMANCE:\n"); print(nb_conf_val)
cat("\nNAIVE BAYES TEST PERFORMANCE:\n"); print(nb_conf_test)

# Model Comparison
get_metrics <- function(conf) {
  acc  <- conf$overall["Accuracy"]
  sens <- conf$byClass["Sensitivity"]
  spec <- conf$byClass["Specificity"]
  f1   <- conf$byClass["F1"]
  
  data.frame(
    Accuracy = as.numeric(acc),
    Sensitivity = as.numeric(sens),
    Specificity = as.numeric(spec),
    F1_Score = as.numeric(f1)
  )
}

# KNN Table
knn_table <- rbind(
  Train = get_metrics(knn_conf_train),
  Validation = get_metrics(knn_conf_val),
  Test = get_metrics(knn_conf_test)
)

knn_table <- round(knn_table, 5)

# Naive Bayes table
nb_table <- rbind(
  Train = get_metrics(nb_conf_train),
  Validation = get_metrics(nb_conf_val),
  Test = get_metrics(nb_conf_test)
)

nb_table <- round(nb_table, 5)

# print final tables
cat("KNN PERFORMANCE TABLE\n")
print(knn_table)

cat("NAIVE BAYES PERFORMANCE TABLE\n")
print(nb_table)




#### PROJECT 2 #######

# Load the sft_mailset
sft_mail <- read.csv("Software_Mailing_List.csv")
summary(sft_mail)

# Descriptive statistics for numeric variables
num_vars <- c("sequence_number", "Freq", "last_update_days_ago", 
              "X1st_update_days_ago", "Spending")
print(psych::describe(sft_mail[num_vars]))

# Source column summaries
source_cols <- names(sft_mail)[grepl("^source_", names(sft_mail))]
source_summary <- tibble(
  Source = source_cols,
  Count = sapply(source_cols, function(x) sum(sft_mail[[x]] == 1, na.rm = TRUE)),
  Purchasers = sapply(source_cols, function(x) sum(sft_mail[[x]] == 1 & sft_mail$Purchase == 1, na.rm = TRUE))
) %>%
  mutate(Purchase_Rate = round(Purchasers / pmax(Count,1), 4)) %>%
  arrange(desc(Count))
print(source_summary)

# Visualization of Spending distribution among purchasers
sft_mail %>%
  filter(Purchase == 1) %>%
  ggplot(aes(x = Spending)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Spending Distribution (Purchasers)", x = "Spending", y = "Count")

# Visualization of Frequency by Purchase

# Select numeric variables
numeric_vars <- sft_mail %>%
  dplyr::select(where(is.numeric))

# Exclude Purchase (target) and binary 0/1 predictors 
numeric_cont <- numeric_vars %>%
  dplyr::select(where(~ length(unique(.)) > 2)) 

# Reshape to long format 
numeric_long <- numeric_cont %>%
  mutate(Purchase = sft_mail$Purchase) %>%
  pivot_longer(cols = -Purchase, names_to = "Variable", values_to = "Value")

# Boxplots for continuous numeric variables by Purchase 
ggplot(numeric_long, aes(x = Purchase, y = Value)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, outlier.color = "black") +
  facet_wrap(~Variable, scales = "free", ncol = 4) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 10, face = "bold")) +
  labs(title = "Box Plots for Outlier Detection", x = "Variables", y = "Values")

# Visualization of Purchase proportion by Web.order 
# Select only binary variables 
binary_vars <- sft_mail %>%
  dplyr::select(where(~ length(unique(.)) == 2))

# Reshape to long format 
binary_long <- binary_vars %>%
  mutate(Purchase = sft_mail$Purchase) %>%
  pivot_longer(cols = -Purchase, names_to = "Variable", values_to = "Value")

# Proportion plot for all binary variables 
ggplot(binary_long, aes(x = factor(Value), fill = factor(Purchase))) +
  geom_bar(position = "fill") +
  facet_wrap(~Variable, scales = "free", ncol = 4) +
  labs(title = "Purchase Proportion by Binary Variables",
       x = "Binary Value (0/1)",
       y = "Proportion") +
  theme_minimal()

# Correlation heatmap of numeric variables 
numeric_df <- sft_mail %>%
  dplyr::select(where(is.numeric))

# Exclude binary (0/1) variables
numeric_cont <- numeric_df %>%
  dplyr::select(where(~ length(unique(.)) >2))

# Build correlation matrix
cor_mat <- cor(numeric_cont, use = "complete.obs")

# Plot correlation heatmap
ggcorrplot(cor_mat, 
           method = "square",      
           type = "lower",         
           lab = TRUE,              
           lab_size = 3, 
           colors = c("white","lightblue", "steelblue"), 
           title = "Correlation Heatmap of Numeric Features",
           ggtheme = ggplot2::theme_minimal,
           tl.cex = 10,             
           tl.srt = 90)  

#  Business-related summaries 
# Average spending among purchasers
avg_spend <- sft_mail %>% 
  filter(Purchase == 1) %>% 
  summarise(Avg_Spending = mean(Spending, na.rm = TRUE))
print(avg_spend)


# Predictor Analysis 
# Keep all relevant numeric & categorical predictors, exclude IDs
sft_mail <- sft_mail %>% 
  dplyr::select(-sequence_number)

# Data Engineering & Transformation 
# Convert days to years
sft_mail <- sft_mail %>%
  mutate(
    last_update_years = last_update_days_ago / 365,
    X1st_update_years = X1st_update_days_ago / 365
  )

# Recode inconsistent cases: 0 purchase but spending > 0 -> set to 0-0
sft_mail <- sft_mail %>%
  mutate(
    Spending = ifelse(Purchase == 0 & Spending > 0, 0, Spending)
  )

# Checking the multicollinearity (Before Ridge)
ols_before <- lm(Purchase ~ X1st_update_days_ago + last_update_days_ago + Freq,
                 data = sft_mail)
vif(ols_before)

# Ridge Regression for Handling Multicollinearity
# Select predictors
predictors <- c("X1st_update_days_ago", "last_update_days_ago", "Freq")

# Prepare X and y
x_all <- model.matrix(Purchase ~ ., 
                      data = sft_mail[, c(predictors, "Purchase")])[,-1]
y_all <- sft_mail$Purchase

# Ridge regression with cross-validation
ridge_cv <- cv.glmnet(
  x_all, y_all,
  alpha = 0,                     
  lambda = 10^seq(4, -2, length = 100),
  nfolds = 10,
  family = "binomial",
  standardize = TRUE
)

# Best lambda
ridge_best_lambda <- ridge_cv$lambda.min

# Ridge Shrinkage Path Plot
plot(ridge_cv$glmnet.fit, xvar = "lambda")
abline(v = log(ridge_best_lambda), col = "red", lty = 2)

# Checking multicollinearity after Ridge
ols_model <- lm(Purchase ~ X1st_update_days_ago + last_update_days_ago + Freq,
                data = sft_mail)
vif(ols_model)

# Data Partitioning 
set.seed(123)  # reproducibility

# Training (60%) vs Temp (40%)
train_index <- createDataPartition(sft_mail$Purchase, p = 0.60, list = FALSE)
train_data <- sft_mail[train_index, ]
temp_data <- sft_mail[-train_index, ]

# Split the temp set into Validation (20%) and Test (20%)
val_index <- createDataPartition(temp_data$Purchase, p = 20/40, list = FALSE)
validation_data <- temp_data[val_index, ]
test_data <- temp_data[-val_index, ]

# Partition Summary 
partition_summary <- data.frame(
  Dataset = c("Training", "Validation", "Test"),
  Observations = c(nrow(train_data), nrow(validation_data), nrow(test_data)),
  Percentage = round(c(
    nrow(train_data) / nrow(sft_mail),
    nrow(validation_data) / nrow(sft_mail),
    nrow(test_data) / nrow(sft_mail)
  ) * 100, 2)
)

print(partition_summary)


# Logistic Regression 

# FUNCTION: Calculate Performance Metrics
calculate_metrics <- function(model, data, adjust_factor = 0.106, threshold = 0.053) {
  # Get predicted probabilities from the model
  raw_probs <- predict(model, newdata = data, type = "response")
  
  # Adjust probabilities to reflect true population purchase rate (5.3%)
  # EModel trained on 50/50 oversample. Scaling ensures real-world predictions.
  adjusted_probs <- raw_probs * adjust_factor
  
  # Convert probabilities to predicted classes using true response rate threshold
  predicted_class <- ifelse(adjusted_probs > threshold, 1, 0)
  
  # Actual responses
  actual <- data$Purchase
  
  # Confusion matrix metrics
  cm <- confusionMatrix(
    factor(predicted_class, levels = c(0, 1)), 
    factor(actual, levels = c(0, 1)),
    positive = "1"
  )
  
  return(c(
    Accuracy = as.numeric(cm$overall['Accuracy']),
    Sensitivity = as.numeric(cm$byClass['Sensitivity']),
    Specificity = as.numeric(cm$byClass['Specificity'])
  ))
}

# DATASETS
datasets <- list(
  Train = train_data,
  Test = test_data,
  Validation = validation_data
)

# MODEL FORMULAS
formulas_fixed <- list(
  Mixed = as.formula("Purchase ~ Freq + Web.order + Gender.male + US + Address_is_res + last_update_days_ago + X1st_update_days_ago"),
  Full_NoSpending = as.formula(paste(
    "Purchase ~",
    paste(setdiff(names(train_data), c("Purchase", "Spending", "last_update_years", "X1st_update_years")), collapse = " + ")
  )),
  SelectedWithSignificance = as.formula("Purchase ~ source_h + source_u + Freq + Web.order"),
  MixedSignificance = as.formula("Purchase ~ source_a + source_h + source_t + source_u + Freq + Web.order + Address_is_res"),
  MixedSignificance2 = as.formula("Purchase ~ source_a + source_h + source_u + Freq + Web.order + Address_is_res"),
  MixedSignificance3 = as.formula("Purchase ~ source_a + source_h + source_u + Freq + Web.order + Address_is_res + last_update_days_ago + X1st_update_days_ago"),
  MixedSignificance4 = as.formula("Purchase ~ source_h + source_u + Freq + Web.order + last_update_days_ago + X1st_update_days_ago"),
  MixedSignificance5 = as.formula("Purchase ~ source_a + source_h + source_t + source_u + Freq + Web.order + Address_is_res + 
                                  source_m + source_x + source_w"),
  MixedSignificance6 = as.formula("Purchase ~ source_a + source_h + source_t + source_u + Freq + Web.order + Address_is_res + 
                                  source_w"),
  SourceOnly = as.formula("Purchase ~ source_a + source_b + source_c + source_d + source_e + source_m + source_o + source_h +
                          source_r + source_s + source_t + source_u + source_p + source_x + source_w")
)

# Run GLM Models and Calculate Metrics
run_fixed_models_with_metrics <- function(formulas_list) {
  results <- data.frame()
  
  for (model_name in names(formulas_list)) {
    cat("\n Fitting", model_name, "Model (Fixed Effects Only) \n")
    
    # Fit model on training data
    model <- glm(
      formulas_list[[model_name]],
      data = train_data,
      family = binomial(link = "logit"),
      control = glm.control(maxit = 100, epsilon = 1e-8)
    )
    
    # Print model summary and ANOVA
    print(summary(model))
    print(Anova(model, type = "II", test = "LR"))
    
    # Calculate metrics for Train, Test, Validation
    for (data_name in names(datasets)) {
      data <- datasets[[data_name]]
      metrics <- calculate_metrics(model, data)
      
      results <- rbind(results, data.frame(
        Model = model_name,
        Dataset = data_name,
        Accuracy = round(metrics['Accuracy'], 4),
        Sensitivity = round(metrics['Sensitivity'], 4),
        Specificity = round(metrics['Specificity'], 4)
      ))
    }
  }
  
  return(results)
}

# RUN MODELS AND EVALUATE
all_metrics <- run_fixed_models_with_metrics(formulas_fixed)

# DISPLAY RESULTS
cat("\n\n")
cat("================================================================================\n")
cat("PERFORMANCE METRICS FOR ALL FIXED-EFFECT MODELS\n")
cat("================================================================================\n\n")
print(all_metrics, row.names = FALSE)

# Summary by model type
cat("\n\n")
cat("================================================================================\n")
cat("AVERAGE METRICS BY MODEL TYPE\n")
cat("================================================================================\n\n")
summary_by_model <- aggregate(cbind(Accuracy, Sensitivity, Specificity) ~ Model, 
                              data = all_metrics, FUN = mean)
print(summary_by_model, row.names = FALSE)

# ROC / AUC CURVES FOR TEST & VALIDATION 
for (model_name in names(formulas_fixed)) {
  cat("\n\n ROC / AUC for", model_name, "Model \n")
  
  model <- glm(
    formulas_fixed[[model_name]],
    data = train_data,
    family = binomial,
    control = glm.control(maxit = 100)
  )
  
  for (data_name in c("Test", "Validation")) {
    if (!is.null(datasets[[data_name]])) {
      data <- datasets[[data_name]]
      probs <- predict(model, newdata = data, type = "response")
      probs_adjusted <- probs * 0.106
      roc_obj <- roc(data$Purchase, probs_adjusted)
      auc_val <- auc(roc_obj)
      
      cat(data_name, "AUC:", round(auc_val, 4), "\n")
      plot(roc_obj, col = ifelse(data_name == "Test", "blue", "red"), lwd = 2,
           main = paste("ROC Curve -", model_name, "(", data_name, ")"))
      abline(a = 0, b = 1, lty = 2, col = "gray")
      text(0.6, 0.2, paste("AUC =", round(auc_val, 4)), col = ifelse(data_name == "Test", "blue", "red"))
    }
  }
}


# Neural Network

# Prepare Target Variable
train_data$Purchase <- as.numeric(as.factor(train_data$Purchase)) - 1
validation_data$Purchase <- as.numeric(as.factor(validation_data$Purchase)) - 1
test_data$Purchase <- as.numeric(as.factor(test_data$Purchase)) - 1

# Define Formulas
formulas <- list(
  Full_NoSpending = as.formula(paste(
    "Purchase ~",
    paste(setdiff(names(train_data), c("Purchase", "Spending", "last_update_years", "X1st_update_years")), collapse = " + ")
  )),
  MixedSignificance = as.formula("Purchase ~ source_a + source_h + source_t + source_u + Freq + Web.order + Address_is_res"),
  MixedSignificance2 = as.formula("Purchase ~ source_a + source_h + source_u + Freq + Web.order + Address_is_res"),
  MixedSignificance3 = as.formula("Purchase ~ source_a + source_h + source_u + Freq + Web.order + Address_is_res + last_update_days_ago + X1st_update_days_ago"),
  MixedSignificance4 = as.formula("Purchase ~ source_h + source_u + Freq + Web.order + last_update_days_ago + X1st_update_days_ago")
)

# Scale Numeric Predictors
outcome_var <- "Purchase"
numeric_predictors <- setdiff(names(train_data)[sapply(train_data, is.numeric)], outcome_var)

preproc <- preProcess(train_data[, numeric_predictors], method = c("center", "scale"))

train_data[, numeric_predictors] <- predict(preproc, train_data[, numeric_predictors])
validation_data[, numeric_predictors] <- predict(preproc, validation_data[, numeric_predictors])
test_data[, numeric_predictors] <- predict(preproc, test_data[, numeric_predictors])

# Step 4: Evaluation Function
evaluate_nn <- function(model, data, predictors, adjust_factor = 0.106, threshold = 0.053) {
  # Raw predictions
  raw_preds <- compute(model, data[, predictors])$net.result
  
  # Adjust probabilities to reflect true population purchase rate
  adjusted_preds <- raw_preds * adjust_factor
  
  # Predicted classes using population threshold
  pred_class <- ifelse(adjusted_preds > threshold, 1, 0)
  
  # Confusion matrix
  cm <- confusionMatrix(
    factor(pred_class, levels = c(0, 1)),
    factor(data$Purchase, levels = c(0, 1)),
    positive = "1"
  )
  
  # ROC / AUC using adjusted probabilities
  roc_obj <- roc(data$Purchase, as.numeric(adjusted_preds))
  auc_val <- auc(roc_obj)
  
  return(list(ConfusionMatrix = cm, ROC = roc_obj, AUC = auc_val, Predictions = adjusted_preds))
}

# Train on TRAIN Data, Evaluate on VALIDATION and TEST
set.seed(123)

datasets <- list(
  Train = train_data,
  Validation = validation_data,
  Test = test_data
)

results <- list()
summary_df <- data.frame()

# Layout for NN plots
par(mfrow = c(2, 3))

for (model_name in names(formulas)) {
  cat("\n==============================\n")
  cat("Training Neural Network for:", model_name, "\n")
  cat("==============================\n")
  
  formula <- formulas[[model_name]]
  predictors <- all.vars(formula)[-1]
  
  # Train neural networks on TRAIN data only
  nn1 <- neuralnet(formula, data = train_data, hidden = c(3),
                   linear.output = FALSE, stepmax = 5e6, threshold = 0.05)
  nn2 <- neuralnet(formula, data = train_data, hidden = c(5, 3),
                   linear.output = FALSE, stepmax = 5e6, threshold = 0.05)
  
  # Show the neural network outputs
  cat("\n NN1 (3) Output for", model_name, " \n")
  print(nn1$result.matrix)
  
  cat("\n NN2 (5,3) Output for", model_name, " \n")
  print(nn2$result.matrix)
  
  # Plot architectures
  plot(nn1, rep = "best", main = paste(model_name, "- NN1 (3)"))
  plot(nn2, rep = "best", main = paste(model_name, "- NN2 (5,3)"))
  
  results[[model_name]] <- list()
  
  # Evaluate only on Validation and Test sets
  for (data_name in names(datasets)) {
    data <- datasets[[data_name]]
    
    res1 <- evaluate_nn(nn1, data, predictors)
    res2 <- evaluate_nn(nn2, data, predictors)
    
    results[[model_name]][[data_name]] <- list(NN1 = res1, NN2 = res2)
    
    for (i in 1:2) {
      res <- get(paste0("res", i))
      cm <- res$ConfusionMatrix
      
      summary_df <- rbind(summary_df, data.frame(
        ModelType = model_name,
        Dataset = data_name,
        Architecture = paste0("NN", i),
        Accuracy = cm$overall["Accuracy"],
        Sensitivity = cm$byClass["Sensitivity"],
        Specificity = cm$byClass["Specificity"],
        AUC = as.numeric(res$AUC)
      ))
    }
  }
}

par(mfrow = c(1, 1))

# ROC Curves for Validation and Test
par(mfrow = c(1, 1))

for (model_name in names(results)) {
  # Extract ROC objects
  roc_val_nn1 <- results[[model_name]]$Validation$NN1$ROC
  roc_val_nn2 <- results[[model_name]]$Validation$NN2$ROC
  roc_test_nn1 <- results[[model_name]]$Test$NN1$ROC
  roc_test_nn2 <- results[[model_name]]$Test$NN2$ROC
  
  # Calculate AUC values
  auc_val_nn1 <- round(as.numeric(pROC::auc(roc_val_nn1)), 3)
  auc_val_nn2 <- round(as.numeric(pROC::auc(roc_val_nn2)), 3)
  auc_test_nn1 <- round(as.numeric(pROC::auc(roc_test_nn1)), 3)
  auc_test_nn2 <- round(as.numeric(pROC::auc(roc_test_nn2)), 3)
  
  # Create better model names
  model_display <- switch(model_name,
                          "Full_NoSpending" = "Full Model (No Spending)",
                          "MixedSignificance" = "Mixed Significance Model 1",
                          "MixedSignificance2" = "Mixed Significance Model 2",
                          "MixedSignificance3" = "Mixed Significance Model 3",
                          "MixedSignificance4" = "Mixed Significance Model 4",
                          model_name
  )
  
  # Plot
  plot(roc_val_nn1, col = "blue", lwd = 2, lty = 1,
       main = paste("ROC Curve -", model_display),
       xlab = "False Positive Rate (1 - Specificity)",
       ylab = "True Positive Rate (Sensitivity)")
  
  plot(roc_val_nn2, col = "red", lwd = 2, lty = 1, add = TRUE)
  plot(roc_test_nn1, col = "blue", lwd = 2, lty = 2, add = TRUE)
  plot(roc_test_nn2, col = "red", lwd = 2, lty = 2, add = TRUE)
  
  abline(0, 1, col = "gray", lty = 2)
  
  legend("bottomright", 
         legend = c(paste0("NN1 Val (AUC = ", auc_val_nn1, ")"),
                    paste0("NN2 Val (AUC = ", auc_val_nn2, ")"),
                    paste0("NN1 Test (AUC = ", auc_test_nn1, ")"),
                    paste0("NN2 Test (AUC = ", auc_test_nn2, ")")),
         col = c("blue", "red", "blue", "red"), 
         lty = c(1, 1, 2, 2), lwd = 2, cex = 0.7)
}

# Summary Table
cat("\n==============================\n")
cat("Summary of Neural Networks (Trained on TRAIN, Evaluated on VALIDATION/TEST)\n")
cat("==============================\n")
print(summary_df, row.names = FALSE)

# Calculate averages for each model and architecture combination
avg_summary_arch <- summary_df %>%
  group_by(ModelType, Architecture) %>%
  summarise(
    Avg_Accuracy = mean(Accuracy, na.rm = TRUE),
    Avg_Sensitivity = mean(Sensitivity, na.rm = TRUE),
    Avg_Specificity = mean(Specificity, na.rm = TRUE),
    Avg_AUC = mean(AUC, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Avg_AUC))

cat("\n==============================\n")
cat("Average Performance by Model and Architecture\n")
cat("==============================\n")
print(as.data.frame(avg_summary_arch), row.names = FALSE)


# CLUSTERING

# Scale numeric data 
sft_mail_scaled <- sft_mail %>%
  dplyr::select(where(is.numeric), -Purchase, -Spending) %>%
  scale()

# Elbow plot for K-means 
fviz_nbclust(sft_mail_scaled, kmeans, method = "wss") +
  ggtitle("Elbow Method for K-Means (WSS)")

# K-means clustering (k = 3) 
set.seed(123)
kmeans_result <- kmeans(sft_mail_scaled, centers = 3, nstart = 25)

# Hierarchical clustering (k = 3) 
dist_matrix <- dist(sft_mail_scaled)
hclust_result <- hclust(dist_matrix, method = "ward.D2")
hclust_clusters <- cutree(hclust_result, k = 3)

# K-means cluster visualization 
fviz_cluster(kmeans_result, data = sft_mail_scaled,
             geom = "point",
             main = "K-means Clustering (k = 3)")

# Silhouette plot for K-means 
sil_kmeans <- silhouette(kmeans_result$cluster, dist(sft_mail_scaled))
fviz_silhouette(sil_kmeans) +
  ggtitle("Silhouette Plot - K-means (k = 3)")

# Bar plot of K-means cluster centroids 
kmeans_centroids <- as.data.frame(kmeans_result$centers)
kmeans_centroids$Cluster <- factor(1:3)
centroids_long <- reshape2::melt(kmeans_centroids, id.vars = "Cluster")

ggplot(centroids_long, aes(x = variable, y = value, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  coord_flip() +
  labs(title = "K-means Cluster Centroids", x = "Variables", y = "Scaled Values")

# Hierarchical clustering dendrogram (takes about 3/4 mins to run)
fviz_dend(hclust_result, k = 3, rect = TRUE,
          main = "Hierarchical Clustering Dendrogram (k = 3)")

# Silhouette plot for Hierarchical clustering 
sil_hclust <- silhouette(hclust_clusters, dist(sft_mail_scaled))
fviz_silhouette(sil_hclust) +
  ggtitle("Silhouette Plot - Hierarchical Clustering (k = 3)")

# Bar plot of Hierarchical cluster centroids 
hclust_centroids <- aggregate(sft_mail_scaled, by = list(Cluster = hclust_clusters), mean)
hclust_centroids_long <- reshape2::melt(hclust_centroids, id.vars = "Cluster")

ggplot(hclust_centroids_long, aes(x = variable, y = value, fill = factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Hierarchical Cluster Centroids", x = "Variables", y = "Scaled Values")

# Compare K-means and Hierarchical Clustering (k = 3) 
#  K-means Cluster Summary 
kmeans_sizes <- as.data.frame(table(kmeans_result$cluster))
colnames(kmeans_sizes) <- c("Cluster", "Size")
kmeans_sizes$Cluster <- as.character(kmeans_sizes$Cluster)

# K-means silhouette width summary
kmeans_sil_df <- data.frame(sil_kmeans[, 1:3])
colnames(kmeans_sil_df) <- c("Cluster", "Neighbor", "Sil_Width")
kmeans_sil_df$Cluster <- as.character(kmeans_sil_df$Cluster)
kmeans_sil_summary <- kmeans_sil_df %>%
  group_by(Cluster) %>%
  summarise(Avg_Sil_Width = mean(Sil_Width)) %>%
  mutate(Method = "K-means")

# K-means centroid mean
kmeans_centroid_means <- kmeans_centroids %>%
  mutate(Cluster = as.character(1:3)) %>%
  dplyr::select(where(is.numeric), Cluster) %>%
  rowwise() %>%
  mutate(Centroid_Mean = mean(c_across(where(is.numeric)))) %>%
  ungroup() %>%
  dplyr::select(Cluster, Centroid_Mean)

# Merge K-means summaries
kmeans_summary <- kmeans_sizes %>%
  left_join(kmeans_sil_summary, by = "Cluster") %>%
  left_join(kmeans_centroid_means, by = "Cluster")

# Hierarchical Cluster Summary 
hclust_sizes <- as.data.frame(table(hclust_clusters))
colnames(hclust_sizes) <- c("Cluster", "Size")
hclust_sizes$Cluster <- as.character(hclust_sizes$Cluster)

# Hierarchical silhouette width summary
hclust_sil_df <- data.frame(sil_hclust[, 1:3])
colnames(hclust_sil_df) <- c("Cluster", "Neighbor", "Sil_Width")
hclust_sil_df$Cluster <- as.character(hclust_sil_df$Cluster)
hclust_sil_summary <- hclust_sil_df %>%
  group_by(Cluster) %>%
  summarise(Avg_Sil_Width = mean(Sil_Width)) %>%
  mutate(Method = "Hierarchical")

# Hierarchical centroid mean
hclust_centroid_means <- hclust_centroids %>%
  mutate(Cluster = as.character(1:3)) %>%
  dplyr::select(where(is.numeric), Cluster) %>%
  rowwise() %>%
  mutate(Centroid_Mean = mean(c_across(where(is.numeric)))) %>%
  ungroup() %>%
  dplyr::select(Cluster, Centroid_Mean)

# Merge Hierarchical summaries
hclust_summary <- hclust_sizes %>%
  left_join(hclust_sil_summary, by = "Cluster") %>%
  left_join(hclust_centroid_means, by = "Cluster")

# Combine both into one comparison table 
comparison_table <- bind_rows(kmeans_summary, hclust_summary) %>%
  arrange(Method, Cluster)

# Display results 
cat("\n===== CLUSTER COMPARISON TABLE (K = 3) =====\n")
print(comparison_table)

# Overall Average Silhouette Widths 
overall_sil_kmeans <- mean(kmeans_sil_df$Sil_Width)
overall_sil_hclust <- mean(hclust_sil_df$Sil_Width)

cat("\nAverage Silhouette Width (Overall):\n")
cat("  K-means      :", round(overall_sil_kmeans, 3), "\n")
cat("  Hierarchical :", round(overall_sil_hclust, 3), "\n")


#Compare K-means and Hierarchical Clustering Centroids & Centers 
#  K-means Cluster Summary 
kmeans_sizes <- as.data.frame(table(kmeans_result$cluster))
colnames(kmeans_sizes) <- c("Cluster", "Size")
kmeans_sizes$Cluster <- as.character(kmeans_sizes$Cluster)

# K-means centroid mean
kmeans_centroids_long <- kmeans_centroids %>%
  mutate(Cluster = as.character(1:3)) %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Centroid_Value") %>%
  mutate(Method = "K-means")

#  Hierarchical Cluster Summary 
hclust_sizes <- as.data.frame(table(hclust_clusters))
colnames(hclust_sizes) <- c("Cluster", "Size")
hclust_sizes$Cluster <- as.character(hclust_sizes$Cluster)

# Hierarchical silhouette width summary
hclust_sil_df <- data.frame(sil_hclust[, 1:3])
colnames(hclust_sil_df) <- c("Cluster", "Neighbor", "Sil_Width")
hclust_sil_df$Cluster <- as.character(hclust_sil_df$Cluster)
hclust_sil_summary <- hclust_sil_df %>%
  group_by(Cluster) %>%
  summarise(Avg_Sil_Width = mean(Sil_Width)) %>%
  mutate(Method = "Hierarchical")

# Hierarchical centroid mean (variable-level)
hclust_centroids_long <- hclust_centroids %>%
  mutate(Cluster = as.character(1:3)) %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Centroid_Value") %>%
  mutate(Method = "Hierarchical")

#  Merge centroid tables 
centroid_comparison <- bind_rows(kmeans_centroids_long, hclust_centroids_long) %>%
  arrange(Method, Cluster, Variable)

#  Merge main summaries 
kmeans_summary <- kmeans_sizes %>%
  left_join(kmeans_sil_summary, by = "Cluster")

hclust_summary <- hclust_sizes %>%
  left_join(hclust_sil_summary, by = "Cluster")

comparison_table <- bind_rows(kmeans_summary, hclust_summary) %>%
  arrange(Method, Cluster)

#  Display results 
cat("\n===== CLUSTER COMPARISON SUMMARY (k = 3) =====\n")
print(comparison_table)

centroid_wide <- centroid_comparison %>%
  mutate(Cluster_Label = paste(Method, "C", Cluster, sep = "_")) %>%
  dplyr::select(Variable, Cluster_Label, Centroid_Value) %>%
  pivot_wider(names_from = Cluster_Label, values_from = Centroid_Value) %>%
  arrange(Variable)

cat("\n===== COMPARATIVE CLUSTER CENTROIDS TABLE (WIDE FORMAT) =====\n")
print(centroid_wide, n = nrow(centroid_wide))

# K-MEANS & HIERARCHICAL CLUSTERING (k = 3)

#  Select Business-Relevant Variables according to the output from Log regression and NN 
seg_data <- sft_mail %>%
  dplyr::select(US, source_a, source_b, source_c, source_d, source_e, source_m, source_o, source_h, source_r, 
                source_s, source_t, source_u, source_p, source_x, source_w, Freq, Web.order, Address_is_res)

#  Scale the Data 
seg_scaled <- scale(seg_data)
dist_matrix <- dist(seg_scaled)

# K-MEANS CLUSTERING (k = 3)
set.seed(123)
kmeans_result <- kmeans(seg_scaled, centers = 3, nstart = 25)
kmeans_clusters <- kmeans_result$cluster

# Silhouette 
sil_kmeans <- silhouette(kmeans_clusters, dist_matrix)

# K-Means Cluster Plot 
fviz_cluster(kmeans_result, data = seg_scaled, geom = "point", ellipse.type = "norm") +
  ggtitle("K-Means Clustering (k = 3)")

# K-Means Silhouette Plot 
fviz_silhouette(sil_kmeans) +
  ggtitle("Silhouette Plot — K-Means (k = 3)")

# K-Means Silhouette Summary 
kmeans_sil_df <- data.frame(sil_kmeans[, 1:3])
colnames(kmeans_sil_df) <- c("Cluster", "Neighbor", "Sil_Width")
kmeans_sil_df$Cluster <- as.character(kmeans_sil_df$Cluster)

kmeans_sil_summary <- kmeans_sil_df %>%
  group_by(Cluster) %>%
  summarise(Avg_Sil_Width = round(mean(Sil_Width), 3)) %>%
  mutate(Method = "K-Means")

#  K-Means Cluster Centroids 
kmeans_centroids <- as.data.frame(kmeans_result$centers)
kmeans_centroids$Cluster <- factor(1:nrow(kmeans_centroids))
kmeans_centroids_long <- kmeans_centroids %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Centroid_Value") %>%
  mutate(Method = "K-Means")

#  K-Means Cluster Width (SD) 
kmeans_spread <- aggregate(as.data.frame(seg_scaled), by = list(Cluster = kmeans_clusters), FUN = sd)

# HIERARCHICAL CLUSTERING (k = 3)
hclust_result <- hclust(dist_matrix, method = "ward.D2")
hclust_clusters <- cutree(hclust_result, k = 3)

# Silhouette 
sil_hclust <- silhouette(hclust_clusters, dist_matrix)

# Dendrogram (takes about 3/4 mins to run)
fviz_dend(hclust_result, k = 3, rect = TRUE, rect_border = "black", rect_fill = TRUE) +
  ggtitle("Hierarchical Clustering Dendrogram (k = 3)")

# Hierarchical Cluster Plot 
fviz_cluster(list(data = seg_scaled, cluster = hclust_clusters),
             geom = "point", ellipse.type = "norm") +
  ggtitle("Hierarchical Cluster Plot (k = 3)")

# Hierarchical Silhouette Plot 
fviz_silhouette(sil_hclust) +
  ggtitle("Silhouette Plot — Hierarchical (k = 3)")

# Hierarchical Silhouette Summary 
hclust_sil_df <- data.frame(sil_hclust[, 1:3])
colnames(hclust_sil_df) <- c("Cluster", "Neighbor", "Sil_Width")
hclust_sil_df$Cluster <- as.character(hclust_sil_df$Cluster)

hclust_sil_summary <- hclust_sil_df %>%
  group_by(Cluster) %>%
  summarise(Avg_Sil_Width = round(mean(Sil_Width), 3)) %>%
  mutate(Method = "Hierarchical")

# Hierarchical Cluster Centroids 
hclust_centroids <- aggregate(seg_scaled, by = list(Cluster = hclust_clusters), mean)
hclust_centroids_long <- hclust_centroids %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Centroid_Value") %>%
  mutate(Method = "Hierarchical")

# Hierarchical Cluster Width (SD) 
hclust_spread <- aggregate(as.data.frame(seg_scaled), by = list(Cluster = hclust_clusters), FUN = sd)

# Cluster Sizes 
kmeans_sizes <- as.data.frame(table(kmeans_clusters))
colnames(kmeans_sizes) <- c("Cluster", "Size")
kmeans_sizes$Cluster <- as.character(kmeans_sizes$Cluster)

hclust_sizes <- as.data.frame(table(hclust_clusters))
colnames(hclust_sizes) <- c("Cluster", "Size")
hclust_sizes$Cluster <- as.character(hclust_sizes$Cluster)

# Combine summaries 
kmeans_summary <- kmeans_sizes %>% left_join(kmeans_sil_summary, by = "Cluster")
hclust_summary <- hclust_sizes %>% left_join(hclust_sil_summary, by = "Cluster")

comparison_table <- bind_rows(kmeans_summary %>% mutate(Method = "K-Means"),
                              hclust_summary %>% mutate(Method = "Hierarchical")) %>%
  arrange(Method, Cluster)

# Display cluster summary 
cat("\n===== CLUSTER COMPARISON TABLE (k = 3) =====\n")
print(comparison_table)

# Comparative Centroids 
# Ensure Cluster columns are character
kmeans_centroids_long$Cluster <- as.character(kmeans_centroids_long$Cluster)
hclust_centroids_long$Cluster <- as.character(hclust_centroids_long$Cluster)

# Bind and pivot
centroid_comparison <- bind_rows(kmeans_centroids_long, hclust_centroids_long) %>%
  mutate(Cluster_Label = paste(Method, "C", Cluster, sep = "_")) %>%
  dplyr::select(Variable, Cluster_Label, Centroid_Value) %>%
  pivot_wider(names_from = Cluster_Label, values_from = Centroid_Value) %>%
  arrange(Variable)

cat("\n===== COMPARATIVE CLUSTER CENTROIDS (WIDE) =====\n")
print(centroid_comparison, n = nrow(centroid_comparison))

# Overall Average Silhouette Widths 
overall_sil_kmeans <- mean(kmeans_sil_df$Sil_Width)
overall_sil_hclust <- mean(hclust_sil_df$Sil_Width)

cat("\nAverage Silhouette Width (Overall):\n")
cat("  K-Means      :", round(overall_sil_kmeans, 3), "\n")
cat("  Hierarchical :", round(overall_sil_hclust, 3), "\n")



#### PROJECT 3 #####

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


