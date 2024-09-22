# Load necessary libraries
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(ggplot2)
library(dplyr)
library(readr)

# Function to read multiple CSV files and prepare data for plotting
read_and_prepare_data <- function(file_paths) {
  # Initialize an empty list to store data from each CSV file
  all_data <- list()
  
  # Loop over each file path and read the data
  for (i in 1:length(file_paths)) {
    # Read the CSV file
    data <- read_csv(file_paths[i])
    
    # Add an additional column indicating the source of the data
    data <- data %>%
      mutate(Source = paste0("File_", i))  # Customize the name as needed
    
    # Append the data to the list
    all_data[[i]] <- data
  }
  
  # Combine all data into one dataframe
  combined_data <- bind_rows(all_data)
  
  return(combined_data)
}

# Example usage
# Specify the paths to the CSV files
setwd('D:/Work Folder/PostDoc/Github Codes')
file_paths <- c("file1.csv", "file2.csv", "file3.csv")  # Replace with your actual file paths

# Read and prepare the data
combined_data <- read_and_prepare_data(file_paths)

# Plot and overlay the distributions using ggplot2
# Assuming the variable of interest is in a column named 'Value'
ggplot(combined_data, aes(x = Value, fill = Source)) +
  geom_density(alpha = 0.5) +  # Adjust transparency with alpha
  labs(
    title = "Overlayed Distribution of CSV Files",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  # Change color palette as needed
