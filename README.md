# Academic-Project---Webster-University

R-based analytics practicum projects completed at Webster University. All scripts are written in R and cover data cleaning, EDA, and machine learning across three business datasets, unified in a combined Final Project script.

---

## Projects

### Project 1 — Used Device Price Analysis
**Data:** `used_device_data.csv`
EDA and predictive modeling on a used-smartphone dataset: missing-value imputation, outlier handling, correlation analysis, and regression/classification to predict normalized device prices.

### Project 2 — Software Mailing List (NorthPoint)
**Data:** `Software_Mailing_List.csv`
Customer analytics on a mailing list: purchase behavior, spending distributions, and source-channel performance, plus clustering and classification to predict purchase likelihood.

### Project 3 — Mortgage Payback Prediction
**Data:** `Mortgage.csv`
Binary classification on mortgage loan data to predict default risk, using Random Forest, GBM, ridge (`glmnet`), and logistic regression, with ROSE oversampling for class imbalance.

### Final Project — Combined Practicum
Consolidates all three datasets/analyses into one end-to-end script, adding neural network models and clustering comparisons.

---

## How it's done

1. **Setup** — every dataset is loaded with `read.csv()`, then inspected with `str()`/`summary()`.
2. **Cleaning** — missing values handled via `mice`/`Amelia`/`VIM`-style imputation, outliers checked with boxplots and IQR logic, categorical variables releveled where needed.
3. **Class imbalance (Project 3)** — the mortgage default class is rebalanced with `ROSE` (Random Over-Sampling Examples) before model fitting, since defaults are a minority class.
4. **Model fitting** — a mix of linear, tree-based, and regularized models (see Algorithms below), each evaluated with a train/validation/test split.
5. **Model comparison** — ROC/AUC (`pROC`), confusion matrices, and for clustering, silhouette scores, are used to compare candidate models/cluster counts side by side.
6. **Visualization** — `ggplot2`, `GGally`, `corrplot`, `ggcorrplot`, `factoextra`, `NeuralNetTools` for correlation plots, cluster plots, and neural-net architecture diagrams.

## Code & libraries used

`tidyverse`, `dplyr`, `tidyr`, `caret`, `randomForest`, `xgboost`, `gbm`, `glmnet`, `MASS`, `ROSE`, `class`, `e1071`, `neuralnet`, `NeuralNetTools`, `cluster`, `factoextra`, `NbClust`, `pROC`, `car`, `VIM`, `mice`, `Amelia`, `corrplot`, `GGally`, `ggcorrplot`, `reshape2`, `broom`, `patchwork`.

## The algorithms

- **Ridge regression** (`glmnet::cv.glmnet`, cross-validated λ) — regularized linear regression used on the device-price data to shrink correlated coefficients and reduce overfitting.
- **Logistic regression** (`glm(..., family = binomial)`) — baseline classifier for purchase likelihood (Project 2) and default risk (Project 3).
- **Random Forest** (`randomForest`, incl. a bagging variant with `mtry = ncol(x_train)`) — ensemble of decision trees trained on bootstrap samples, used for default-risk classification; the bagging variant uses all predictors at each split (equivalent to bootstrap aggregation) as a comparison baseline against the true random-forest run (`mtry = 15`).
- **Gradient Boosting Machine** (`gbm`) — sequentially fits trees to residual errors of prior trees; evaluated at multiple tree-count/threshold combinations on train/validation/test splits via a custom `evaluate_gbm()` helper.
- **Neural networks** (`neuralnet`, hidden layers `c(3)` and `c(5, 3)`) — small feed-forward nets compared against the linear/tree baselines for the same prediction tasks.
- **K-means clustering** (`kmeans`, `centers = 3`, `nstart = 25`) — partitions mailing-list customers into 3 segments based on scaled purchase/spending features; cluster count chosen via the elbow method (`fviz_nbclust(..., method = "wss")`) and validated with silhouette width.
- **Hierarchical clustering** (`hclust`, Ward's method `"ward.D2"`) — agglomerative clustering run alongside k-means as a comparison, merging clusters to minimize within-cluster variance at each step.
