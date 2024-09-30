# Load necessary libraries
library(dplyr)   # For data manipulation
library(ggplot2) # For plotting
library(car)     # For Levene's test

# Raw Data Entry
data <- data.frame(
  Surface = factor(rep(c("Grass", "Concrete"), each = 20)),
  Time = factor(rep(c(10, 20, 30, 40, 50), times = 8)),  # Convert Time to factor
  Ant_Abundance = c(10, 12, 11, 13, 15, 17, 16, 18, 
                    20, 22, 21, 23, 25, 26, 24, 27, 
                    30, 31, 29, 32, 
                    5, 4, 6, 5, 8, 7, 9, 8, 
                    10, 9, 11, 10, 12, 11, 13, 12, 
                    15, 14, 16, 15)
)

# Perform Two-Way ANOVA
anova_result <- aov(Ant_Abundance ~ Surface * Time, data = data)

# View the ANOVA table
summary(anova_result)

# Levene's Test for Homogeneity of Variance
levene_test_result <- leveneTest(Ant_Abundance ~ Surface * Time, data = data)
print(levene_test_result)

# Create summary data for plotting
data_summary <- data %>%
  group_by(Surface, Time) %>%
  summarise(
    mean_abundance = mean(Ant_Abundance),
    se_abundance = sd(Ant_Abundance) / sqrt(n())
  )

# Create a bar chart with error bars
ggplot(data_summary, aes(x = Time, y = mean_abundance, fill = Surface)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean_abundance - se_abundance, 
                    ymax = mean_abundance + se_abundance), 
                position = position_dodge(0.7), width = 0.2, color = "black") +
  labs(title = "Mean Ant Abundance by Surface Type and Time",
       x = "Time (units)",  # Add units if applicable
       y = "Mean Ant Abundance (n)") +  # Include units for y-axis
  theme_minimal() +
  theme(
    text = element_text(size = 12, color = "black"),  # Consistent text size
    plot.title = element_text(size = 14, face = "bold"),  # Bold title
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Black border
    axis.ticks = element_line(color = "black"),  # Black tick marks
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "top",  # Place legend at the top
    legend.title = element_blank()  # Remove legend title
  ) +
  scale_fill_manual(values = c("Grass" = "lightgreen", "Concrete" = "lightgray"))  # Custom color palette
