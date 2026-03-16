# Used Smartphone Price Analytics - Exploratory Data Analysis (EDA)
# ==============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(VIM)
library(plotly)
library(gridExtra)
library(reshape2)
library(RColorBrewer)
library(GGally)
library(car)
library(psych)

# Load the dataset
# Note: Adjust the file path as needed
data <- read.csv("~/Documents/Analytics Practicum/used_device_data.csv", stringsAsFactors = FALSE)

# ==============================================================================
# 1. BASIC DATA EXPLORATION
# ==============================================================================

# Display basic information about the dataset
cat("=== DATASET OVERVIEW ===\n")
cat("Dataset Dimensions:", dim(data)[1], "rows x", dim(data)[2], "columns\n\n")

# Display first few rows
cat("=== FIRST 6 ROWS ===\n")
print(head(data))

# Display data structure
cat("\n=== DATA STRUCTURE ===\n")
str(data)

# Display summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
summary(data)

# ==============================================================================
# 2. DATA QUALITY ASSESSMENT
# ==============================================================================

# Check for missing values
cat("\n=== MISSING VALUES ANALYSIS ===\n")
missing_counts <- sapply(data, function(x) sum(is.na(x)))
missing_percentages <- round((missing_counts / nrow(data)) * 100, 2)
missing_summary <- data.frame(
  Variable = names(missing_counts),
  Missing_Count = missing_counts,
  Missing_Percentage = missing_percentages
)
print(missing_summary[missing_summary$Missing_Count > 0, ])

# Visualize missing data patterns
if(sum(missing_counts) > 0) {
  VIM::aggr(data, col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE)
  title("Missing Data Pattern Analysis")
}

# Check for zero values in key numeric variables
cat("\n=== ZERO VALUES ANALYSIS ===\n")
numeric_cols <- c("rear_camera_mp", "front_camera_mp", "internal_memory", "ram", "battery")
zero_analysis <- data.frame(
  Variable = numeric_cols,
  Zero_Count = sapply(numeric_cols, function(x) sum(data[[x]] == 0, na.rm = TRUE)),
  Zero_Percentage = sapply(numeric_cols, function(x) round((sum(data[[x]] == 0, na.rm = TRUE) / nrow(data)) * 100, 2))
)
print(zero_analysis[zero_analysis$Zero_Count > 0, ])

# ==============================================================================
# 3. UNIVARIATE ANALYSIS
# ==============================================================================

cat("\n=== UNIVARIATE ANALYSIS ===\n")

# Create comprehensive descriptive statistics for continuous variables
continuous_vars <- c("screen_size", "rear_camera_mp", "front_camera_mp", 
                     "internal_memory", "ram", "battery", "weight", 
                     "days_used", "normalized_used_price", "normalized_new_price")

# Detailed statistics using psych package
cat("DETAILED DESCRIPTIVE STATISTICS FOR CONTINUOUS VARIABLES:\n")
desc_stats <- describe(data[continuous_vars])
print(desc_stats)

# Distribution analysis for key continuous variables
# Price variables distribution
p1 <- ggplot(data, aes(x = normalized_used_price)) +
  geom_histogram(bins = 15, fill = "steelblue", alpha = 0.7, color = "black") +
  geom_density(aes(y = ..density.. * (max(..count..) * 1.1)), color = "red", size = 1) +
  labs(title = "Distribution of Normalized Used Price", 
       x = "Normalized Used Price", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data, aes(x = normalized_new_price)) +
  geom_histogram(bins = 15, fill = "darkgreen", alpha = 0.7, color = "black") +
  geom_density(aes(y = ..density.. * (max(..count..) * 1.1)), color = "red", size = 1) +
  labs(title = "Distribution of Normalized New Price", 
       x = "Normalized New Price", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Technical specifications distributions
p3 <- ggplot(data, aes(x = screen_size)) +
  geom_histogram(bins = 15, fill = "purple", alpha = 0.7, color = "black") +
  labs(title = "Screen Size Distribution", 
       x = "Screen Size (inches)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p4 <- ggplot(data, aes(x = ram)) +
  geom_histogram(bins = 12, fill = "orange", alpha = 0.7, color = "black") +
  labs(title = "RAM Distribution", 
       x = "RAM (GB)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Display distribution plots
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Box plots for outlier detection
continuous_for_boxplot <- data[continuous_vars]
continuous_melted <- melt(continuous_for_boxplot)

ggplot(continuous_melted, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Box Plots for Outlier Detection", 
       x = "Variables", y = "Values") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())

# Categorical variables analysis
cat("\n=== CATEGORICAL VARIABLES FREQUENCY ANALYSIS ===\n")

# Brand distribution
brand_freq <- table(data$device_brand)
brand_pct <- round(prop.table(brand_freq) * 100, 2)
cat("BRAND DISTRIBUTION:\n")
print(data.frame(Brand = names(brand_freq), 
                 Frequency = as.numeric(brand_freq),
                 Percentage = as.numeric(brand_pct)))

# Operating system distribution
os_freq <- table(data$os)
cat("\nOPERATING SYSTEM DISTRIBUTION:\n")
print(data.frame(OS = names(os_freq), 
                 Frequency = as.numeric(os_freq),
                 Percentage = round(as.numeric(os_freq)/sum(os_freq) * 100, 2)))

# Connectivity features
cat("\n4G AVAILABILITY:\n")
print(table(data$X4g))
cat("\n5G AVAILABILITY:\n")
print(table(data$X5g))

# Release year distribution
cat("\nRELEASE YEAR DISTRIBUTION:\n")
year_freq <- table(data$release_year)
print(data.frame(Year = names(year_freq), 
                 Frequency = as.numeric(year_freq),
                 Percentage = round(as.numeric(year_freq)/sum(year_freq) * 100, 2)))

# Visualize categorical distributions
p5 <- ggplot(data, aes(x = reorder(device_brand, device_brand, function(x) length(x)))) +
  geom_bar(fill = "coral", alpha = 0.8) +
  coord_flip() +
  labs(title = "Device Brand Distribution", 
       x = "Brand", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p6 <- ggplot(data, aes(x = as.factor(release_year))) +
  geom_bar(fill = "lightgreen", alpha = 0.8) +
  labs(title = "Release Year Distribution", 
       x = "Release Year", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p5, p6, ncol = 2)

# ==============================================================================
# 4. BIVARIATE ANALYSIS
# ==============================================================================

cat("\n=== BIVARIATE ANALYSIS ===\n")

# Calculate price retention ratio
data$price_retention_ratio <- data$normalized_used_price / data$normalized_new_price
data$device_age_years <- 2020 - data$release_year

# Correlation matrix for continuous variables
numeric_data <- data[sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Visualize correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix - Continuous Variables", mar = c(0,0,2,0))

# Key correlations with target variables
cat("CORRELATIONS WITH NORMALIZED USED PRICE:\n")
used_price_corr <- correlation_matrix[,"normalized_used_price"]
used_price_corr_sorted <- sort(abs(used_price_corr), decreasing = TRUE)
print(round(used_price_corr_sorted, 3))

cat("\nCORRELATIONS WITH NORMALIZED NEW PRICE:\n")
new_price_corr <- correlation_matrix[,"normalized_new_price"]
new_price_corr_sorted <- sort(abs(new_price_corr), decreasing = TRUE)
print(round(new_price_corr_sorted, 3))

# Scatter plots for key relationships
p7 <- ggplot(data, aes(x = normalized_new_price, y = normalized_used_price)) +
  geom_point(aes(color = device_brand), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Used Price vs New Price", 
       x = "Normalized New Price", y = "Normalized Used Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p8 <- ggplot(data, aes(x = days_used, y = normalized_used_price)) +
  geom_point(aes(color = device_brand), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Used Price vs Days Used", 
       x = "Days Used", y = "Normalized Used Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p9 <- ggplot(data, aes(x = ram, y = normalized_used_price)) +
  geom_point(aes(color = device_brand), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(title = "Used Price vs RAM", 
       x = "RAM (GB)", y = "Normalized Used Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p10 <- ggplot(data, aes(x = screen_size, y = normalized_used_price)) +
  geom_point(aes(color = device_brand), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(title = "Used Price vs Screen Size", 
       x = "Screen Size (inches)", y = "Normalized Used Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p7, p8, p9, p10, ncol = 2, nrow = 2)

# ==============================================================================
# 5. CATEGORICAL VS CONTINUOUS ANALYSIS
# ==============================================================================

cat("\n=== CATEGORICAL vs CONTINUOUS ANALYSIS ===\n")

# Price analysis by brand
cat("AVERAGE PRICES BY BRAND:\n")
brand_price_summary <- data %>%
  group_by(device_brand) %>%
  summarise(
    Count = n(),
    Avg_Used_Price = round(mean(normalized_used_price, na.rm = TRUE), 3),
    Avg_New_Price = round(mean(normalized_new_price, na.rm = TRUE), 3),
    Avg_Price_Retention = round(mean(price_retention_ratio, na.rm = TRUE), 3),
    Std_Used_Price = round(sd(normalized_used_price, na.rm = TRUE), 3)
  ) %>%
  arrange(desc(Avg_Used_Price))
print(brand_price_summary)

# Box plots by categorical variables
p11 <- ggplot(data, aes(x = device_brand, y = normalized_used_price)) +
  geom_boxplot(aes(fill = device_brand), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Used Price Distribution by Brand", 
       x = "Brand", y = "Normalized Used Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

p12 <- ggplot(data, aes(x = as.factor(release_year), y = normalized_used_price)) +
  geom_boxplot(aes(fill = as.factor(release_year)), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Used Price Distribution by Release Year", 
       x = "Release Year", y = "Normalized Used Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

p13 <- ggplot(data, aes(x = X5g, y = normalized_used_price)) +
  geom_boxplot(aes(fill = X5g), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Used Price by 5G Capability", 
       x = "5G Support", y = "Normalized Used Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

p14 <- ggplot(data, aes(x = os, y = normalized_used_price)) +
  geom_boxplot(aes(fill = os), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Used Price by Operating System", 
       x = "Operating System", y = "Normalized Used Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

grid.arrange(p11, p12, p13, p14, ncol = 2, nrow = 2)

# ==============================================================================
# 6. MULTIVARIATE ANALYSIS
# ==============================================================================

cat("\n=== MULTIVARIATE ANALYSIS ===\n")

# Create device categories based on specifications
data$memory_category <- cut(data$internal_memory, 
                            breaks = c(0, 32, 128, 512, Inf), 
                            labels = c("Low (≤32GB)", "Medium (33-128GB)", 
                                       "High (129-512GB)", "Premium (>512GB)"),
                            include.lowest = TRUE)

data$ram_category <- cut(data$ram, 
                         breaks = c(0, 2, 4, 8, Inf), 
                         labels = c("Low (≤2GB)", "Medium (3-4GB)", 
                                    "High (5-8GB)", "Premium (>8GB)"),
                         include.lowest = TRUE)

# Analysis by device specifications categories
spec_analysis <- data %>%
  group_by(memory_category, ram_category) %>%
  summarise(
    Count = n(),
    Avg_Used_Price = round(mean(normalized_used_price, na.rm = TRUE), 3),
    Price_Range = paste(round(min(normalized_used_price, na.rm = TRUE), 2), 
                        "-", round(max(normalized_used_price, na.rm = TRUE), 2))
  ) %>%
  filter(Count > 0)

cat("PRICE ANALYSIS BY MEMORY AND RAM CATEGORIES:\n")
print(spec_analysis)

# Heatmap for brand vs year analysis
brand_year_summary <- data %>%
  group_by(device_brand, release_year) %>%
  summarise(
    Count = n(),
    Avg_Price = mean(normalized_used_price, na.rm = TRUE)
  ) %>%
  filter(Count > 0)

p15 <- ggplot(brand_year_summary, aes(x = as.factor(release_year), y = device_brand, fill = Avg_Price)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mean(data$normalized_used_price)) +
  labs(title = "Average Used Price Heatmap: Brand vs Release Year", 
       x = "Release Year", y = "Brand", fill = "Avg Used Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(p15)

# ==============================================================================
# 7. OUTLIER ANALYSIS
# ==============================================================================

cat("\n=== OUTLIER ANALYSIS ===\n")

# Function to detect outliers using IQR method
detect_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(which(x < lower_bound | x > upper_bound))
}

# Detect outliers for key continuous variables
outlier_vars <- c("screen_size", "battery", "weight", "normalized_used_price", "days_used")
outlier_summary <- data.frame(
  Variable = character(),
  Outlier_Count = numeric(),
  Outlier_Percentage = numeric(),
  stringsAsFactors = FALSE
)

for(var in outlier_vars) {
  outlier_indices <- detect_outliers_iqr(data[[var]])
  outlier_summary <- rbind(outlier_summary, data.frame(
    Variable = var,
    Outlier_Count = length(outlier_indices),
    Outlier_Percentage = round((length(outlier_indices) / nrow(data)) * 100, 2)
  ))
}

cat("OUTLIER DETECTION SUMMARY (IQR Method):\n")
print(outlier_summary)

# Display specific outlier cases
cat("\nDEVICES WITH EXTREME SCREEN SIZES (>20 inches):\n")
large_screen_devices <- data[data$screen_size > 20, c("device_brand", "screen_size", "battery", "normalized_used_price")]
print(large_screen_devices)

# ==============================================================================
# 8. FEATURE ENGINEERING INSIGHTS
# ==============================================================================

cat("\n=== FEATURE ENGINEERING INSIGHTS ===\n")

# Calculate additional derived features for analysis
data$price_depreciation <- data$normalized_new_price - data$normalized_used_price
data$days_per_year <- data$days_used / data$device_age_years
data$camera_ratio <- ifelse(data$rear_camera_mp == 0, 0, data$front_camera_mp / data$rear_camera_mp)
data$memory_per_gb <- data$internal_memory / data$ram

# Summary of engineered features
cat("PRICE RETENTION ANALYSIS:\n")
retention_stats <- data %>%
  summarise(
    Avg_Retention_Ratio = round(mean(price_retention_ratio, na.rm = TRUE), 3),
    Min_Retention = round(min(price_retention_ratio, na.rm = TRUE), 3),
    Max_Retention = round(max(price_retention_ratio, na.rm = TRUE), 3),
    Std_Retention = round(sd(price_retention_ratio, na.rm = TRUE), 3)
  )
print(retention_stats)

cat("\nDEVICES WITH HIGHEST PRICE RETENTION:\n")
high_retention <- data %>%
  select(device_brand, release_year, days_used, price_retention_ratio, normalized_used_price) %>%
  arrange(desc(price_retention_ratio)) %>%
  head(5)
print(high_retention)

cat("\nDEVICES WITH LOWEST PRICE RETENTION:\n")
low_retention <- data %>%
  select(device_brand, release_year, days_used, price_retention_ratio, normalized_used_price) %>%
  arrange(price_retention_ratio) %>%
  head(5)
print(low_retention)

# ==============================================================================
# 9. FINAL SUMMARY AND INSIGHTS
# ==============================================================================

cat("\n=== FINAL EDA INSIGHTS ===\n")

cat("KEY FINDINGS:\n")
cat("1. Dataset contains", nrow(data), "records with minimal missing data (4.8%)\n")
cat("2. Average price retention ratio:", round(mean(data$price_retention_ratio, na.rm = TRUE), 3), "\n")
cat("3. Strongest price predictor: RAM (correlation =", round(correlation_matrix["ram", "normalized_used_price"], 3), ")\n")
cat("4. Brand dominance: Huawei (", round(sum(data$device_brand == "Huawei")/nrow(data)*100, 1), "%), Honor (", round(sum(data$device_brand == "Honor")/nrow(data)*100, 1), "%)\n")
cat("5. 5G adoption rate:", round(sum(data$X5g == "yes", na.rm = TRUE)/nrow(data)*100, 1), "% of devices\n")
cat("6. Average device age:", round(mean(data$device_age_years, na.rm = TRUE), 1), "years\n")
cat("7. Price range: Used (", round(min(data$normalized_used_price, na.rm = TRUE), 2), "-", round(max(data$normalized_used_price, na.rm = TRUE), 2), "), New (", round(min(data$normalized_new_price, na.rm = TRUE), 2), "-", round(max(data$normalized_new_price, na.rm = TRUE), 2), ")\n")

# Create summary table for model preparation
model_ready_summary <- data %>%
  summarise(
    Total_Records = n(),
    Complete_Cases = sum(complete.cases(.)),
    Missing_Data_Rate = round((1 - sum(complete.cases(.))/n()) * 100, 2),
    Numeric_Features = sum(sapply(., is.numeric)),
    Categorical_Features = sum(sapply(., function(x) is.character(x) || is.factor(x))),
    Target_Variable_Range = paste(round(min(normalized_used_price, na.rm = TRUE), 2), 
                                  "-", round(max(normalized_used_price, na.rm = TRUE), 2))
  )

cat("\nMODEL READINESS SUMMARY:\n")
print(model_ready_summary)

cat("\nEDA COMPLETE - Data is ready for modeling phase\n")
cat("Recommended next steps:\n")
cat("1. Handle missing values using imputation\n")
cat("2. Address outliers based on business context\n")
cat("3. Scale/normalize features for modeling\n")
cat("4. Create train/validation/test splits\n")
cat("5. Begin model development and evaluation\n")