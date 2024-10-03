
library(ggplot2)
library(dplyr)

# Example 
data <- data.frame(
  Gene = rep(c("TP53", "EGFR", "KRAS","UBQLN1"), each = 100),
  Expression = c(rnorm(100, mean = 2), rnorm(100, mean = 4), rnorm(100, mean = 6),rnorm(100, mean = 7)),
  Group = rep(c("Control", "Treatment"), each = 500, times = 4)
)

# Create the violin plot with grid lines
ggplot(data, aes(x = Group, y = Expression, fill = Group)) +
  geom_violin(trim = FALSE, scale = "width", color = "black") + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9), alpha = 0.2) +  # Optional boxplot overlay
  facet_grid(Gene ~ .) +  # Separate each gene in its own row
  geom_hline(yintercept = seq(min(data$Expression), max(data$Expression), by = 2), linetype = "dashed", color = "gray") +  # Horizontal lines for each expression level
  geom_vline(xintercept = c(1.5), linetype = "dashed", color = "gray") +  # Vertical lines between Control and Treatment
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#FF9999", "#66B2FF")) +  
  labs(title = "Concatenated Stacked Violin Plot with Lines", 
       x = "Condition", y = "Expression Level")
