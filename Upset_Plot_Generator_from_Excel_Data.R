# Load necessary libraries
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("UpSetR", quietly = TRUE)) {
  install.packages("UpSetR")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(readxl)
library(UpSetR)
library(dplyr)

# Function to read Excel file and convert it to an adjacency matrix
prepare_adjacency_matrix <- function(file_path) {
  # Load the Excel file
  data <- read_excel(file_path)
  
  # Get all unique proteins from all columns
  unique_proteins <- unique(unlist(data))
  
  # Initialize an empty matrix with rows as proteins and columns as categories
  adjacency_matrix <- matrix(0, nrow = length(unique_proteins), ncol = ncol(data))
  rownames(adjacency_matrix) <- unique_proteins
  colnames(adjacency_matrix) <- colnames(data)
  
  # Fill the matrix with 1 where a protein is present in a category
  for (i in 1:ncol(data)) {
    adjacency_matrix[unique_proteins %in% data[[i]], i] <- 1
  }
  
  return(as.data.frame(adjacency_matrix))
}

# Function to create an upset plot
create_upset_plot <- function(adjacency_matrix) {
  # Convert matrix to appropriate format for upset plot
  adjacency_matrix <- adjacency_matrix %>% as.data.frame()
  
  # Create the upset plot
  upset(adjacency_matrix, sets = colnames(adjacency_matrix), order.by = "freq")
}

setwd('path to the file/Your file')
# Example usage
file_path <- "Your file.xlsx"  # Replace with your actual file path
# Prepare adjacency matrix
adjacency_matrix <- prepare_adjacency_matrix(file_path)

# Print adjacency matrix (Optional)
print(adjacency_matrix)

# Create and display the upset plot
create_upset_plot(adjacency_matrix)

# End of script
