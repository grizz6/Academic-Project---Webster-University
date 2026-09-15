# Analytics Practicum — Webster University

## About

Three end-to-end predictive analytics projects in R from my Analytics Practicum course at Webster University. Each one takes a raw business dataset through cleaning, exploratory analysis, multicollinearity checks, a train/validation/test split, several competing models, and a side-by-side comparison of results.

| Project | Question | Main techniques |
|---|---|---|
| **1. Used Devices** | What drives a used phone's resale price, and can we tell which phones hold their value? | OLS regression, ridge, KNN, Naive Bayes |
| **2. NorthPoint Software Mailing List** | Which catalog customers will buy, and what customer segments exist? | Logistic regression, neural networks, k-means, hierarchical clustering |
| **3. Mortgage Payback** | Which mortgages will default? | Logistic regression, mixed-effects logistic regression, bagging/random forest, gradient boosting, k-means |

---

## Repository structure

```
├── Project 1/
│   ├── Practicum - Grishma Gajurel.R   # full analysis
│   ├── Practicum.R                     # earlier EDA-only draft
│   ├── used_device_data.csv
│   └── used_device_data.xlsx
├── Project 2/
│   ├── NorthPoint - Grishma Gajurel.r
│   └── Software_Mailing_List.csv
├── Project 3/
│   ├── Mortage Payback - Grishma Gajurel.R
│   └── Mortgage.csv.zip                # unzip to Mortgage.csv before running
└── Final Project/
    └── Final Practicum - Grishma Gajurel.R   # Projects 1–3 combined into one script
```

**Running the scripts:** Projects 1 and 2 read their CSVs from `~/Documents/Analytics Practicum/...`, so update the `read.csv()` path first. Project 3 and the Final Project read files by name from the working directory. The hierarchical-clustering dendrogram in Project 2 takes a few minutes to draw.

---

## Project 1 — Used Device Pricing

**Data:** `used_device_data.csv`: brand, OS, screen size, 4G/5G, camera megapixels, memory, RAM, battery, weight, release year, days used, plus normalized new and used prices.

1. **EDA:** descriptive stats (`psych`), brand/OS/network/release-year distributions, correlation plot, boxplots, missing-data pattern plot (`VIM::aggr`).
2. **Missing values:** drops rows missing both RAM and battery, then fills remaining gaps with a **custom imputer** that uses the median of phones from the same brand with similar specs (and brand + release year for camera values).
3. **Outliers:** IQR fences per numeric column, with outliers replaced by the median of the in-range values.
4. **New target:** `PriceRetention = normalized_used_price / normalized_new_price`, then split at the median into a two-class label (retains value well vs. not).
5. **Multicollinearity:** checks VIF on a full OLS model, then fits a cross-validated **ridge regression** (`cv.glmnet`) on the four collinear predictors.
6. **Split:** 70% train / 10% validation / 20% test (`caret::createDataPartition`).
7. **Regression:** a series of multiple linear regression models on `normalized_used_price`, each narrowing to more significant predictors, compared on train/validation/test error with diagnostic plots.
8. **Classification:** **KNN** (k chosen by validation accuracy) and **Naive Bayes** on the price-retention label, evaluated with confusion matrices.

## Project 2 — NorthPoint Software Mailing List

**Data:** `Software_Mailing_List.csv`: which of 15 source lists a customer came from, purchase frequency, days since first and last update, web order, gender, residential address, and the targets `Purchase` (0/1) and `Spending`.

1. **EDA:** spending among purchasers, frequency by purchase, boxplots and proportion plots by purchase outcome, correlation heatmap.
2. **Feature prep:** converts "days ago" fields to years and fixes rows with no purchase but nonzero spending.
3. **Multicollinearity:** VIF check, then a binomial **ridge** model (`cv.glmnet`).
4. **Split:** 60% train / 20% validation / 20% test.
5. **Logistic regression:** several formulas, compared on accuracy, precision, recall, and F1, with ROC/AUC curves (`pROC`) for validation and test.
6. **Neural networks:** `neuralnet` models with one hidden layer of 3 nodes and two hidden layers of 5 and 3, trained on scaled predictors, drawn with `NeuralNetTools`, and compared with ROC/AUC.
7. **Clustering:** scaled features, elbow plot, then **k-means** (k = 3, 25 starts) and **hierarchical clustering** (Ward's D2), compared with silhouette plots and cluster-centroid bar charts.

## Project 3 — Mortgage Payback

**Data:** `Mortgage.csv`: loan-level panel data with time stamps (observation, origination, first, maturity), balance, loan-to-value, interest rate, house price index, GDP, unemployment, property type, investor flag, FICO score at origination, and status (active / default / payoff). About 67 MB unzipped.

1. **EDA:** summary stats, missing-value counts, correlation matrix, boxplots by status, pairwise scatterplots.
2. **Multicollinearity:** **ridge regression** (`glmnet`, alpha = 0) with a trace plot and cross-validated λ.
3. **Cleaning & features:** removes impossible zero-balance and zero-LTV rows unless the loan was paid off, derives `loan_age`, `time_to_maturity`, and `observation_lag`, and **keeps the last record per loan** so each loan appears once.
4. **Split:** 60% train / 20% validation / 20% test on `default_time`.
5. **Logistic regression:** an all-predictor model and a reduced model.
6. **Mixed-effects logistic regression:** three `lme4::glmer` models with a random intercept for `time`, tested with Type III Chi-square ANOVA.
7. **Bagging / random forest:** `randomForest` with 200 trees, using all predictors at each split (bagging) vs. 15 per split.
8. **Gradient boosting:** `gbm` with 300 trees and interaction depth 3, evaluated by a custom `evaluate_gbm()` helper.
9. **Threshold tuning:** every classifier is scored at a 0.50 and a 0.35 cutoff (a lower cutoff catches more defaults, the rarer class), with precision, recall, F1, and AUC tables.
10. **Clustering:** elbow plot and k-means on the numeric loan features.

---

## The algorithms

- **Ordinary least squares regression** (`lm`): fits the linear combination of predictors that minimizes squared error, used for used-phone price.
- **Ridge regression** (`glmnet`, alpha = 0): adds a penalty on coefficient size that shrinks correlated predictors toward each other. Used in all three projects to deal with multicollinearity, with λ chosen by cross-validation.
- **Logistic regression** (`glm`, binomial): models the log-odds of a yes/no outcome (purchase, default).
- **Mixed-effects logistic regression** (`glmer`): logistic regression plus a random intercept, letting baseline default rates vary by time period.
- **K-nearest neighbors** (`class::knn`): classifies a phone by majority vote of its k closest phones in scaled feature space.
- **Naive Bayes** (`e1071::naiveBayes`): applies Bayes' theorem assuming predictors are independent given the class.
- **Neural networks** (`neuralnet`): small feed-forward networks trained with backpropagation.
- **Bagging and random forest** (`randomForest`): many decision trees on bootstrap samples, averaged. Looking at all predictors at each split is bagging; sampling a subset (`mtry = 15`) is a random forest, which decorrelates the trees.
- **Gradient boosting** (`gbm`): builds trees one after another, each fitted to the errors of the trees before it.
- **K-means** (`kmeans`): partitions observations into k groups by minimizing within-cluster variance, with k chosen by the elbow method.
- **Hierarchical clustering** (`hclust`, Ward's D2): merges the closest clusters step by step, choosing each merge to add the least within-cluster variance.
- **Evaluation:** confusion matrices, accuracy / precision / recall / F1, ROC curves and AUC (`pROC`), silhouette width (`cluster`).

## Built with

R: `dplyr`, `tidyr`, `tidyverse`, `ggplot2`, `GGally`, `corrplot`, `ggcorrplot`, `plotly`, `patchwork`, `gridExtra`, `VIM`, `psych`, `Hmisc`, `car`, `caret`, `glmnet`, `MASS`, `class`, `e1071`, `lme4`, `randomForest`, `gbm`, `neuralnet`, `NeuralNetTools`, `cluster`, `factoextra`, `NbClust`, `pROC`, `broom`, `moments`.

## License

MIT. See [`LICENSE`](LICENSE).
