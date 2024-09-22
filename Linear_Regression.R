# Load necessary libraries
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(ggplot2)
library(readr)

# Function to load demo data
load_demo_data <- function(file_path) {
  # Load the dataset
  data <- read_csv(file_path)
  
  return(data)
}

# Function to perform linear regression
perform_linear_regression <- function(data, x_var, y_var) {
  # Build the linear regression model
  formula <- as.formula(paste(y_var, "~", x_var))
  model <- lm(formula, data = data)
  
  # Print the summary of the model
  print(summary(model))
  
  return(model)
}

# Function to visualize the linear regression result
plot_linear_regression <- function(data, model, x_var, y_var) {
  #scatter plot with regression line
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = paste("Linear Regression of", y_var, "on", x_var),
         x = x_var, y = y_var) +
    theme_minimal()
  
  print(plot)
}

# Function to plot diagnostic plots for the linear regression model
plot_diagnostics <- function(model) {
  par(mfrow = c(2, 2))
  plot(model)
}

# Example usage
#  path to  demo data CSV file
file_path <- "C:/Users/dipan/OneDrive/Documents/demodata_linear_regression.csv"  # Replace this with the actual path to the demo file

# Load the demo data
data <- load_demo_data(file_path)

# Perform linear regression on demo data
# Here, 'X' is the predictor variable and 'Y' is the response variable
x_var <- "X"
y_var <- "Y"
model <- perform_linear_regression(data, x_var, y_var)

plot_linear_regression(data, model, x_var, y_var)

plot_diagnostics(model)

# End of script
