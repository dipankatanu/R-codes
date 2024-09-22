# Load necessary libraries
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

# Create a dataset for different cell types in a tissue sample
data <- data.frame(
  CellType = c('T Cells', 'B Cells', 'Macrophages', 'Dendritic Cells', 'NK Cells'),
  Proportion = c(40, 20, 15, 10, 15)  # Proportions in percentages
)

# Create a doughnut plot using plotly
p <- plot_ly(
  data, 
  labels = ~CellType, 
  values = ~Proportion, 
  type = 'pie',
  hole = 0.5,  # This creates the doughnut shape
  textinfo = 'label+percent',  # Show label and percentage
  hoverinfo = 'label+percent+value',  # Detailed hover info
  marker = list(
    colors = c('#FF6347', '#4682B4', '#32CD32', '#FFD700', '#8A2BE2')
  )
) %>%
  layout(
    title = 'Proportions of Cell Types in Tissue Sample',
    showlegend = TRUE
  )

# Display the plot
p
