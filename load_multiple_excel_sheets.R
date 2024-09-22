# ============================================
# Script: load_multiple_excel_sheets.R
# Author: DipanKA Tanu
# Description: 
# This script reads all sheets from an Excel file and stores 
# each sheet as a data frame in a list using either the readxl 
# or openxlsx package. The list is named based on the sheet names.
# ============================================

# ==== Load Necessary Libraries ====
# Check if readxl is installed; if not, install it
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

# ==== Function to Load Multiple Excel Sheets Using readxl ====
load_excel_sheets <- function(file_path) {
  # Get the names of all sheets in the Excel file
  sheet_names <- excel_sheets(file_path)
  
  # Use lapply to read each sheet and store as a list of data frames
  sheet_list <- lapply(sheet_names, function(sheet) read_excel(file_path, sheet = sheet))
  
  # Assign the sheet names to the list elements for easy access
  names(sheet_list) <- sheet_names
  
  # Return the list of data frames
  return(sheet_list)
}

# ==== Alternative Function to Load Excel Sheets Using openxlsx ====
load_excel_sheets_openxlsx <- function(file_path) {
  # Check if openxlsx is installed; if not, install it
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    install.packages("openxlsx")
  }
  library(openxlsx)
  
  # Get the names of all sheets in the Excel file
  sheet_names <- getSheetNames(file_path)
  
  # Use lapply to read each sheet and store as a list of data frames
  sheet_list <- lapply(sheet_names, function(sheet) read.xlsx(file_path, sheet = sheet))
  
  # Assign the sheet names to the list elements for easy access
  names(sheet_list) <- sheet_names
  
  # Return the list of data frames
  return(sheet_list)
}

# ==== Example Usage ====
# Example file path - replace with your Excel file path
file_path <- "your_excel_file.xlsx"

# Call the function to load all sheets using readxl
sheets_readxl <- load_excel_sheets(file_path)
print("Sheets loaded using readxl:")
print(names(sheets_readxl))  # Print sheet names

# Call the function to load all sheets using openxlsx
sheets_openxlsx <- load_excel_sheets_openxlsx(file_path)
print("Sheets loaded using openxlsx:")
print(names(sheets_openxlsx))  # Print sheet names

# ==== Save Loaded Sheets to CSV Files (Optional) ====
# This step is optional; it demonstrates how you can save each sheet as a CSV file
save_sheets_to_csv <- function(sheets, output_folder = "output_sheets") {
  # Create a folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Save each sheet as a CSV file
  lapply(names(sheets), function(sheet_name) {
    write.csv(sheets[[sheet_name]], file = file.path(output_folder, paste0(sheet_name, ".csv")), row.names = FALSE)
  })
}

# Uncomment the line below to save sheets to CSV files
# save_sheets_to_csv(sheets_readxl, "output_folder_readxl")
# save_sheets_to_csv(sheets_openxlsx, "output_folder_openxlsx")

# ============================================
# End of Script
# ============================================
