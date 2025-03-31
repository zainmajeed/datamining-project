# Data Mining Project: Economic Impact Analysis (1900-1953)

## 📌 Project Overview
This project analyzes the economic impact of war periods (WWI: 1914-1918, WWII: 1939-1945) on GDP growth, unemployment rates, and wages using machine learning techniques. The dataset consists of economic indicators from **1900 to 1953**, extracted and processed for predictive modeling.

## 📂 Dataset
- **Raw Data:** `Combined_Data_1900_1953.xlsx`
- **Processed Data:** `processed_data.xlsx`
- **Target Variable:** `Annual_GDP_Growth`

## 🛠 Installation
To run this project, install the necessary R packages:
```r
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("reshape2")
install.packages("tidyr")
install.packages("gridExtra")
install.packages("caret")
install.packages("randomForest")
install.packages("xgboost")
install.packages("readxl")
install.packages("tibble")
```

## 📊 Data Preprocessing
1. **Handling Missing Values:**
   - Numeric columns: Replaced missing values with column mean/median.
2. **Feature Engineering:**
   - War Period categorization (WWI, WWII, No_War).
   - Lag features for GDP time series modeling.
3. **Normalization:**
   - Standardized GDP-related variables using **Z-score normalization**.

## 🤖 Machine Learning Models
1. **Random Forest** (for feature importance analysis and predictions)
2. **XGBoost** (for improved regression modeling)
3. **Ensemble Learning:**
   - Averaging predictions from Random Forest and XGBoost.

## 📈 Model Performance Evaluation
- **Feature Importance Analysis** (via `randomForest::varImp`)
- **Performance Metrics:**
   - RMSE (Root Mean Square Error)
   - R² (R-Squared Score)
   - MAE (Mean Absolute Error)

## 🚀 How to Run the Project
1. Load the dataset using:
   ```r
   data <- read_excel("data/processed_data.xlsx")
   ```
2. Preprocess data (handle missing values, normalize, create lag features).
3. Train models:
   ```r
   rf_model <- randomForest(Annual_GDP_Growth ~ ., data = trainData, ntree = 500)
   xgb_model <- xgb.train(params = params, data = train_matrix, nrounds = 100)
   ```
4. Evaluate performance:
   ```r
   predictions <- predict(xgb_model, test_matrix)
   performance <- postResample(predictions, testData$Annual_GDP_Growth)
   print(performance)
   ```
5. Run feature importance analysis and visualization.

## 📜 Authors
- **Muhammad Zain Majeed**

## 📌 Future Improvements
- Improve feature selection using **recursive feature elimination**.
- Experiment with **deep learning models** for time-series predictions.
- Add more **economic variables** to enhance model robustness.

📢 **For any questions, feel free to reach out!** 🚀
