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
library(neuralnet)
library(patchwork)
library(e1071)
library(lme4)
library(broom)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(pROC)
library(NbClust)
library(NeuralNetTools)


# Load the sft_mailset
# Note: Adjust the file path as needed
sft_mail <- read.csv("~/Documents/Analytics Practicum/Project 2/Software_Mailing_List.csv", stringsAsFactors = FALSE)

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


# END CODE


