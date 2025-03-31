# Install and load necessary packages
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(openxlsx)
library(readxl)
library(dplyr)

# Load data from Excel sheets
file_path <- "D:\\MLDM\\semester 2\\DataMining Project\\datamining-project\\data\\raw_data.xlsx"
sheets <- list(
  real_gdp = "A2. Real GDP(A)",
  nominal_gdp = "A3. Nominal GDP (A)",
  public_borrowing = "A16. Public Sector Borrowing",
  national_debt = "A18. The National Debt",
  employment = "A28. Employment & unemployment",
  wages_prices = "A26. Wages and prices"
)

data_list <- lapply(sheets, function(sheet) {
  tryCatch(read_excel(file_path, sheet = sheet),
           error = function(e) {
             message(paste("Error loading sheet:", sheet))
             return(NULL)
           })
})

# Filter out failed loads
data_list <- Filter(Negate(is.null), data_list)

# Column selection and renaming
columns_info <- list(
  real_gdp = list(select = c(1,3,4,5,6,10,20), rename = c("Year", "GDP_Factor_Cost", "GDP_Basic_Prices", "GDP_Market_Prices", "Annual_GDP_Growth", "Component_Series", "Price_Cost_Ratio")),
  nominal_gdp = list(select = c(1,2,4,5,6,7,8,9,20,21,25), rename = c("Year", "Nominal_GDP_Factor_Cost", "Nominal_GDP_Basic_Prices", "Nominal_GDP_Market_Prices", "Annual_Growth_Basic", "Annual_Growth_Market", "Annual_Growth_Factor_Cost", "Component_Series_Irish_GDP", "ONS_GDP_Current_Market", "ONS_GDP_Current_Factor", "Share_Irish_GDP")),
  public_borrowing = list(select = c(1,2,3,7,9,19,22,24), rename = c("Year", "Current_Spending", "Gross_Fixed_Capital", "Debt_Interest_Payments", "Total_Expenditure", "Total_Receipts", "Net_Borrowing", "Primary_Surplus_Deficit")),
  national_debt = list(select = c(1,2,5,7,8), rename = c("Year", "Funded_Debt", "Unfunded_Debt", "Total_Debt", "National_Debt")),
  employment = list(select = c(1,2,4,6,7,8,9), rename = c("Year", "Total_Employment", "Self_Employed", "Public_Sector", "Unemployed", "Workforce_Participation", "Unemployment_Rate")),
  wages_prices = list(select = c(1,2,4,5,6,7,9,10,11,12,13,14,16,19,20,29,32), rename = c("Year", "Nominal_Earnings", "Avg_Earnings_Index", "Weekly_Wages", "Spliced_Weekly_Earnings", "Consumer_Prices", "Cost_of_Living", "Retail_Price_Index", "Composite_CPI", "CPI_Alt_Measure", "Producer_Prices", "Spliced_Producer_Price_Index", "Consumption_Deflator", "Investment_Deflator", "Gov_Consumption_Deflator", "Private_Public_Consumption_Deflator", "Domestic_Demand_Deflator", "GDP_Deflator", "Export_Deflator", "Import_Deflator", "Oil_Price", "US_CPI", "US_GDP_Deflator"))
)

# Process each dataset
data_list <- lapply(names(data_list), function(name) {
  df <- data_list[[name]]
  if (!is.null(df)) {
    df <- df %>%
      select(all_of(columns_info[[name]]$select)) %>%
      setNames(columns_info[[name]]$rename)
  }
  df
})
names(data_list) <- names(columns_info)

# Remove metadata rows
metadata_rows <- c(real_gdp = 16, nominal_gdp = 16, public_borrowing = 7, national_debt = 5, employment = 4, wages_prices = 3)
data_list <- lapply(names(data_list), function(name) {
  df <- data_list[[name]]
  df <- df[-(1:metadata_rows[name]), ]
})
names(data_list) <- names(columns_info)

# Function to process and save data
process_and_save_data <- function(data_list, file_name, start_year, end_year) {
  wb <- createWorkbook()
  addWorksheet(wb, "Combined Data")
  
  periods <- list(
    pre_wwi = 1900:1913,
    wwi = 1914:1918,
    interwar = 1919:1938,
    wwii = 1939:1945,
    post_wwii = 1946:1953
  )
  
  combined_data <- data.frame(Year = start_year:end_year)
  
  for (ws_data in data_list) {
    ws_data <- ws_data %>% filter(Year >= start_year & Year <= end_year)
    ws_data$Period <- case_when(
      ws_data$Year %in% periods$pre_wwi ~ 'Pre-WWI',
      ws_data$Year %in% periods$wwi ~ 'WWI',
      ws_data$Year %in% periods$interwar ~ 'Interwar',
      ws_data$Year %in% periods$wwii ~ 'WWII',
      ws_data$Year %in% periods$post_wwii ~ 'Post-WWII',
      TRUE ~ NA_character_
    )
    combined_data <- full_join(combined_data, ws_data, by = "Year")
  }
  
  writeData(wb, "Combined Data", combined_data)
  saveWorkbook(wb, file = file_name, overwrite = TRUE)
}

# Process and save combined data
process_and_save_data(data_list, "Combined_Data_1900_1953_updated.xlsx", 1900, 1953)