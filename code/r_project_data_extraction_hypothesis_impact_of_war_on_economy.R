# Install necessary packages if not installed
if (!require("openxlsx")) install.packages("openxlsx", dependencies = TRUE)
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)

# Load libraries
library(openxlsx)
library(readxl)

# Define file path
file_path <- "raw_data.xlsx"

# Check if the file exists
if (!file.exists(file_path)) stop("Error: File not found. Please check the file path.")

# Function to load and process data
load_and_process_sheet <- function(sheet_name, selected_cols, new_colnames, remove_rows) {
  # Read the Excel sheet
  data <- read_excel(file_path, sheet = sheet_name)
  
  # Check if columns exist
  missing_cols <- setdiff(selected_cols, colnames(data))
  if (length(missing_cols) > 0) stop(paste("Error: Missing columns in", sheet_name, "->", paste(missing_cols, collapse = ", ")))
  
  # Select required columns
  selected_data <- data[, selected_cols]
  colnames(selected_data) <- new_colnames
  
  # Remove metadata rows
  if (nrow(selected_data) > remove_rows) {
    selected_data <- selected_data[-(1:remove_rows), ]
  } else {
    warning(paste("Warning: Not enough rows to remove in", sheet_name))
  }
  
  # Convert Year to numeric
  selected_data$Year <- as.numeric(selected_data$Year)
  
  return(selected_data)
}

# Load datasets
selected_data_real_gdp <- load_and_process_sheet(
  "A2. Real GDP(A)", c(1,3,4,5,6,10,20),
  c("Year", "GDP_at_Factor_Cost", "GDP_at_Basic_Prices", "GDP_at_Market_Prices", 
    "Annual_GDP_Growth", "Component_Series", "Price_Cost_Ratio"), 16)

selected_data_nominal_gdp <- load_and_process_sheet(
  "A3. Nominal GDP (A)", c(1,2,4,5,6,7,8,9,20,21,25),
  c("Year", "Nominal_GDP_Factor_Cost", "Nominal_GDP_Basic_Prices", "Nominal_GDP_Market_Prices",
    "Annual_Growth_Basic", "Annual_Growth_Market", "Annual_Growth_Factor_Cost",
    "Component_Series_Irish_GDP", "ONS_GDP_Current_Market", "ONS_GDP_Current_Factor",
    "Share_Irish_GDP"), 16)

selected_data_public_borrowing <- load_and_process_sheet(
  "A16. Public Sector Borrowing", c(1,2,3,7,9,19,22,24),
  c("Year", "Current_Spending", "Gross_Fixed_Capital", "Public_Debt_Interest",
    "Total_Expenditure", "Total_Receipts", "Net_Lending_Borrowing", "Primary_Surplus_Deficit"), 7)

selected_data_national_debt <- load_and_process_sheet(
  "A18. The National Debt", c(1,2,5,7,8),
  c("Year", "Funded_Debt", "Unfunded_Debt", "Total_Debt",
    "Total_National_Debt"), 5)

selected_data_employment <- load_and_process_sheet(
  "A28. Employment & unemployment", c(1,2,4,6,7,8,9),
  c("Year", "Total_Employment", "Self_Employed", "Public_Sector", "Unemployed",
    "Workforce_Participation", "Unemployment_Rate"), 4)

selected_data_wages_prices <- load_and_process_sheet(
  "A26. Wages and prices", c(1,2,4,5,6,7,9,10,11,12,13,14,16,19,20,29,32),
  c("Year", "Nominal_Earnings", "Average_Earnings_Index", "Weekly_Wages",
    "Spliced_Weekly_Earnings", "Consumer_Prices", "Cost_of_Living", "Retail_Price_Index",
    "Composite_CPI", "CPI_Alternative", "Producer_Prices", "Wholesale_Price_Index",
    "Consumption_Deflator", "Investment_Deflator", "Govt_Consumption_Deflator",
    "Consumption_Deflator", "Domestic_Demand_Deflator", "GDP_Deflator",
    "Export_Deflator", "Import_Deflator", "Oil_Price", "US_CPI", "US_GDP_Deflator"), 3)

# Define the function to process and save data
process_and_save_data <- function(data_list, file_name, start_year, end_year) {
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add a new sheet
  addWorksheet(wb, sheetName = "Combined Data")
  
  # Define periods
  periods <- list(
    pre_wwi = 1900:1913,
    wwi = 1914:1918,
    interwar = 1919:1938,
    wwii = 1939:1945,
    post_wwii = 1946:1953
  )
  
  # Initialize a dataframe to store combined data
  combined_data <- data.frame(Year = start_year:end_year)
  
  # Loop through each dataset
  for (i in seq_along(data_list)) {
    ws_data <- data_list[[i]]
    
    # Ensure dataset has Year column
    if (!"Year" %in% colnames(ws_data)) stop(paste("Error: Year column missing in dataset", i))
    
    # Filter data by Year range
    ws_data <- ws_data[ws_data$Year >= start_year & ws_data$Year <= end_year, ]
    
    # Warn if dataset is empty
    if (nrow(ws_data) == 0) warning(paste("Warning: No data for years", start_year, "to", end_year, "in dataset", i))
    
    # Assign periods
    ws_data$Period <- with(ws_data, ifelse(Year %in% periods$pre_wwi, 'Pre-WWI',
                                           ifelse(Year %in% periods$wwi, 'WWI',
                                                  ifelse(Year %in% periods$interwar, 'Interwar',
                                                         ifelse(Year %in% periods$wwii, 'WWII',
                                                                ifelse(Year %in% periods$post_wwii, 'Post-WWII', NA))))))
    
    # Merge with combined data
    combined_data <- merge(combined_data, ws_data, by = "Year", all.x = TRUE)
  }
  
  # Replace NA values with 0
  combined_data[is.na(combined_data)] <- 0
  
  # Write the combined data to the sheet
  writeData(wb, sheet = "Combined Data", combined_data)
  
  # Save the workbook
  saveWorkbook(wb, file = file_name, overwrite = TRUE)
  
  # Success message
  message("File successfully saved: ", file_name)
}

# List of datasets
data_list <- list(selected_data_real_gdp, selected_data_nominal_gdp, selected_data_public_borrowing, 
                  selected_data_national_debt, selected_data_employment, selected_data_wages_prices)

# Run the function to process and save data
process_and_save_data(data_list, "Combined_Data_1900_1953_updated.xlsx", 1900, 1953)
