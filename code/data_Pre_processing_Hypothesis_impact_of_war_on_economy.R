install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("reshape2")
install.packages("tidyr")
install.packages("gridExtra")
# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)

# Load dataset
data <- read_excel("Combined_Data_1900_1953.xlsx", sheet = "Combined Data")

# View structure of the dataset
str(data)

# Convert Year column to integer
data$Year <- as.integer(data$Year)

# Handle missing values by replacing them with column means (if numeric)
data <- data %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Normalize GDP-related variables
data <- data %>% 
  mutate(across(contains("GDP"), ~ scale(.)))

# Create a categorical variable for war periods (WWI: 1914-1918, WWII: 1939-1945)
data <- data %>% 
  mutate(War_Period = case_when(
    Year >= 1914 & Year <= 1918 ~ "WWI",
    Year >= 1939 & Year <= 1945 ~ "WWII",
    TRUE ~ "No_War"
  ))

# Convert War_Period to factor
data$War_Period <- as.factor(data$War_Period)

# Filter relevant columns for economic impact analysis
data_filtered <- data %>% 
  select(Year, War_Period, GDP_at_Market_Prices, Annual_GDP_Growth, Unemployment_Rate, Average_Weekly_Wages)

# View processed data
head(data_filtered)

# Save processed data to a new CSV file
write.csv(data_filtered, "Processed_Data.csv", row.names = FALSE)
