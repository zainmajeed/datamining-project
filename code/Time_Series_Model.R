# Install necessary packages (if not already installed)
install.packages("caret") 
install.packages("randomForest")
install.packages("ggplot2")
install.packages("xgboost")
install.packages("readxl")
install.packages("dplyr")
install.packages("tibble")

# Load necessary libraries
library(ggplot2)
library(caret)
library(randomForest)
library(readxl)
library(dplyr)
library(xgboost)
library(tibble)

# Load the dataset
data <- read_excel("D:\\MLDM\\semester 2\\DataMining Project\\datamining-project\\data\\processed_data.xlsx")

# Correct the Year column format
data$Year <- format(data$Year, "%Y")

# Convert all columns to numeric format
data_numeric <- data %>% mutate_all(as.numeric)

# Normalize the numeric data using Z-score normalization
data_numeric <- data_numeric %>%
  mutate(across(where(is.numeric), ~ ( . - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

# Display the first few rows of the normalized data
head(data_numeric)

# Splitting the dataset into training (80%) and testing (20%) sets
set.seed(123)  # Set seed for reproducibility
trainIndex <- createDataPartition(data_numeric$Annual_GDP_Growth, p = 0.8, list = FALSE)
trainData <- data_numeric[trainIndex, ]
testData <- data_numeric[-trainIndex, ]

# Train a Random Forest model
rf_model <- randomForest(Annual_GDP_Growth ~ ., data = trainData, importance = TRUE, ntree = 500)

# Extract and display feature importance
importance <- varImp(rf_model, scale = FALSE)
print(importance)

# Sort the feature importance scores in descending order
sorted_importance <- importance[order(-importance$Overall), ]
print(sorted_importance)

# Remove less relevant features based on domain knowledge
data_numeric <- data_numeric[, !(names(data_numeric) %in% c("Average_Earnings_Index", "Nominal_Earnings", "War_Indicator", "Total_Employment_in_Heads", "Price_Cost_Ratio"))]

# View the cleaned dataset
View(data_numeric)

# Handle missing values by replacing them with the median
data_selected <- data_numeric %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Display structure of the cleaned dataset
print(str(data_selected))

# Create lag features for time series analysis
data_selected <- data_selected %>%
  mutate(GDP_at_Market_Prices_lag1 = lag(GDP_at_Market_Prices, 1),
         GDP_at_Market_Prices_lag2 = lag(GDP_at_Market_Prices, 2))

# Display structure after adding lag features
print(str(data_selected))

# Split the dataset again into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data_selected$Annual_GDP_Growth, p = 0.8, list = FALSE)
trainData <- data_selected[trainIndex, ]
testData <- data_selected[-trainIndex, ]

# Prepare matrix format for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(trainData[,-which(names(trainData) == "Annual_GDP_Growth")]),
                            label = trainData$Annual_GDP_Growth)
test_matrix <- xgb.DMatrix(data = as.matrix(testData[,-which(names(testData) == "Annual_GDP_Growth")]),
                           label = testData$Annual_GDP_Growth)

# Define XGBoost parameters
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.7,
  colsample_bytree = 0.7
)

# Train an XGBoost model
xgb_model <- xgb.train(params = params, data = train_matrix, nrounds = 100)

# Make predictions using the trained XGBoost model
predictions <- predict(xgb_model, test_matrix, iteration_range = c(1, 100))
performance <- postResample(predictions, testData$Annual_GDP_Growth)
print(performance)

# Define the grid of hyperparameters for tuning
grid_search_results <- tryCatch({
  caret::train(
    x = as.matrix(trainData[,-which(names(trainData) == "Annual_GDP_Growth")]),
    y = trainData$Annual_GDP_Growth,
    method = "xgbTree",
    trControl = trainControl("cv", number = 5),
    tuneGrid = expand.grid(
      nrounds = c(100, 200),
      eta = c(0.01, 0.1, 0.3),
      max_depth = c(3, 6, 9),
      gamma = c(0, 0.1, 0.2),
      colsample_bytree = c(0.5, 0.7, 1),
      min_child_weight = c(1, 3, 5),
      subsample = c(0.5, 0.7, 1)
    )
  )
}, error = function(e) {
  print("Grid search failed. Check parameters and data format.")
  return(NULL)
})

if (!is.null(grid_search_results)) {
  best_params <- grid_search_results$bestTune
  print(best_params)
}

# Handle missing values in training and testing data
trainData <- trainData %>%
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
testData <- testData %>%
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Train a new Random Forest model with selected features
rf_model <- randomForest(Annual_GDP_Growth ~ ., data = trainData, ntree = 500)

# Make predictions using both models if grid search was successful
if (!is.null(grid_search_results)) {
  predictions_xgb <- predict(grid_search_results, as.matrix(testData[,-which(names(testData) == "Annual_GDP_Growth")]))
  predictions_rf <- predict(rf_model, testData)
  
  # Ensemble predictions by averaging
  ensemble_predictions <- (predictions_xgb + predictions_rf) / 2
  
  # Evaluate the ensemble model's performance
  ensemble_performance <- postResample(ensemble_predictions, testData$Annual_GDP_Growth)
  print(ensemble_performance)
} else {
  print("Skipping ensemble evaluation due to failed grid search.")
}

