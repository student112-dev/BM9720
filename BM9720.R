
# ============================================================================
# BM9720 - Analytics for Competitive Advantage
# Full R Script for Churn Analysis
# ============================================================================

# 1. Load Required Libraries -------------------------------------------------
if(!require(readr)) install.packages("readr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(corrplot)) install.packages("corrplot")
if(!require(Amelia)) install.packages("Amelia")

library(readr)
library(ggplot2)
library(corrplot)
library(Amelia)

# 2. Load Dataset ------------------------------------------------------------
# (Please ensure churn-bigml-20.csv is present in your working directory)

churn_data <- read_csv("churn-bigml-20.csv")

# 3. Data Overview ------------------------------------------------------------
# View structure and summary of dataset
str(churn_data)
summary(churn_data)

# 4. Data Cleaning ------------------------------------------------------------
# Check missing values
colSums(is.na(churn_data))

# Visualize missing values
missmap(churn_data, main = "Missing values map", col = c("yellow", "black"), legend = TRUE)

# Remove rows with missing values
churn_data <- na.omit(churn_data)

# 5. Exploratory Data Analysis (EDA) ------------------------------------------

# 5.1 Target Variable Distribution
ggplot(churn_data, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  labs(title = "Distribution of Churn Variable", x = "Churn", y = "Count") +
  theme_minimal()

# 5.2 Numerical Variables Distribution
numeric_vars <- churn_data %>% dplyr::select(where(is.numeric))

for (col in colnames(numeric_vars)) {
  print(
    ggplot(churn_data, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
      theme_minimal()
  )
}

# 5.3 Correlation Heatmap
cor_matrix <- cor(numeric_vars)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# 6. Feature Engineering ------------------------------------------------------

# Convert 'Churn' into factor
churn_data$Churn <- as.factor(churn_data$Churn)

# 7. Data Splitting ------------------------------------------------------------

set.seed(123)
train_indices <- sample(1:nrow(churn_data), 0.7 * nrow(churn_data))
train_data <- churn_data[train_indices, ]
test_data <- churn_data[-train_indices, ]

# 8. Model Building - Logistic Regression -------------------------------------

model_logistic <- glm(Churn ~ ., data = train_data, family = binomial)
summary(model_logistic)

# 9. Model Evaluation ---------------------------------------------------------

# Predict on test data
predictions <- predict(model_logistic, test_data, type = "response")

# Convert probabilities into classes
predicted_class <- ifelse(predictions > 0.5, "True", "False")

# Confusion Matrix
table(Predicted = predicted_class, Actual = test_data$Churn)

# 10. Feature Importance (Optional) -------------------------------------------

# Extract model coefficients
importance <- summary(model_logistic)$coefficients
importance

# 11. Conclusion (Short) ------------------------------------------------------
# Based on the logistic regression, customer service calls, international plan, and day minutes seem important factors predicting churn.

# ============================================================================
# End of Script
# ============================================================================
