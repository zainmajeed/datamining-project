# Install required packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("reshape2")
install.packages("tidyr")
install.packages("gridExtra")
install.packages("readxl")
install.packages("writexl")

# Load the libraries
library(dplyr)
library(ggplot2)
library(GGally)
library(reshape2)
library(tidyr)
library(gridExtra)
library(readxl)
library(writexl)

# Load dataset 
data <- read_excel("D:\\MLDM\\semester 2\\DataMining Project\\datamining-project\\data\\Combined_Data_1900_1953.xlsx", sheet = "Combined Data")

# Display basic information about the dataset
head(data)   # View first few rows
summary(data) # Summary statistics
str(data)    # Structure of the dataset

# Function to calculate missing value count and percentage
missing_values_summary <- function(df) {
  missing_counts <- colSums(is.na(df))
  total_rows <- nrow(df)
  missing_percentages <- (missing_counts / total_rows) * 100
  missing_summary <- data.frame(Column = names(df), Missing_Count = missing_counts, Missing_Percentage = missing_percentages)
  return(missing_summary)
}

# Get missing value summary
missing_summary <- missing_values_summary(data)
print(missing_summary)

# Identify columns with missing values
na_count <- colSums(is.na(data))
na_columns <- names(na_count)[na_count > 0]
print(na_columns)

# Find positions of NA values in the dataframe
na_positions <- which(is.na(data), arr.ind = TRUE)
print(na_positions)

# Replace NA with 0 in selected columns
columns_to_replace <- c(6, 13, 27, 28, 30)
data[columns_to_replace] <- lapply(data[columns_to_replace], function(x) replace(x, is.na(x), 0))

# Declaring War_Indicator: 1 for war year, 0 for non-war year
war_years <- c(1914:1918, 1939:1945)
data$War_Indicator <- ifelse(data$Year %in% war_years, 1, 0)

# Convert character columns to numeric where possible
data[] <- lapply(data, function(x) if (is.character(x)) as.numeric(as.character(x)) else x)

# Function to plot histograms for all numeric columns
plot_all_histograms <- function(df) {
  numeric_columns <- df %>% select(where(is.numeric))
  plots <- lapply(names(numeric_columns), function(col) {
    ggplot(df, aes_string(x = col)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      theme_minimal() +
      labs(title = paste("Distribution of", col), x = col, y = "Frequency")
  })
  do.call(grid.arrange, c(plots, ncol = 2))
}

plot_all_histograms(data)

# Function to identify outliers using IQR
identify_outliers <- function(df) {
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      outliers <- df[[col]][df[[col]] < lower_bound | df[[col]] > upper_bound]
      cat("\nOutliers in", col, ":\n")
      print(outliers)
    }
  }
}
identify_outliers(data)

# GDP Visualization
ggplot(data, aes(x = Year, y = GDP_at_Market_Prices)) +
  geom_line() +
  labs(title = "GDP at Market Prices Over Time", x = "Year", y = "GDP") +
  theme_minimal()

# Unemployment Rate Visualization
ggplot(data, aes(x = Year, y = Unemployment_Rate)) +
  geom_line() +
  labs(title = "Unemployment Rate Over Time", x = "Year", y = "Unemployment Rate")

# Public Sector Net Lending/Borrowing Visualization
ggplot(data, aes(x = Year, y = Public_Sector_Net_Lending_Borrowing)) +
  geom_bar(stat = "identity") +
  labs(title = "Public Sector Net Lending/Borrowing", x = "Year", y = "Net Lending/Borrowing")

# Reshape data for boxplot visualization
data_long <- pivot_longer(data, cols = where(is.numeric), names_to = "Category", values_to = "Value")
ggplot(data_long, aes(x = Category, y = Value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Category", y = "Value", title = "Boxplot for All Numeric Columns") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation matrix
correlation_matrix <- cor(data %>% select(where(is.numeric)), use = "complete.obs")
melted_cormat <- melt(round(correlation_matrix, 2))
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap")

# GDP growth trend with war indicator
ggplot(data, aes(x = Year, y = Annual_GDP_Growth, color = factor(War_Indicator))) +
  geom_line() +
  geom_point() +
  labs(title = "GDP Growth Over War and Non-War Years", x = "Year", y = "Annual GDP Growth (%)") +
  theme_minimal()

# Average Unemployment Rate & GDP Growth during War & Non-War Years
avg_unemployment <- data %>% group_by(War_Indicator) %>% summarise(Average_Unemployment_Rate = mean(Unemployment_Rate, na.rm = TRUE))
avg_gdp_growth <- data %>% group_by(War_Indicator) %>% summarise(Average_GDP_Growth = mean(Annual_GDP_Growth, na.rm = TRUE))
print(avg_unemployment)
print(avg_gdp_growth)

# Save processed data as an Excel file
write_xlsx(data, "processed_data.xlsx")
