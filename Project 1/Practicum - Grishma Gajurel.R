# GRISHMA GAJUREL

# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(VIM)
library(caret)
library(corrplot)
library(reshape2)
library(plotly)
library(gridExtra)
library(RColorBrewer)
library(GGally)
library(ggcorrplot)
library(car)
library(psych)
library(mice)
library(Hmisc)
library(glmnet)
library(MASS)
library(factoextra)
library(class)
library(patchwork)
library(e1071)

set.seed(123) # for reproducibility 

# Load the dataset
# Note: Adjust the file path as needed
data <- read.csv("~/Documents/Analytics Practicum/Project 1/used_device_data.csv")

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


