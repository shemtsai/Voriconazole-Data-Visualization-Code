# Load required packages
library(readxl)  # To read Excel files
library(ggplot2) # To create the scatterplot

# Step 1: Open Excel file and load data
file_path <- file.choose()  # Prompt to select file
data <- read_excel(file_path)

# Step 2: Filter out rows where VoriDose (column E) is 0
data <- data[data$VoriDose != 0, ]

# Step 3: Replace any VoriLvlValue < 0.5 with 0
data$VoriLvlValue[data$VoriLvlValue < 0.5] <- 0

# Ensure VoriLvlValue is numeric
data$VoriLvlValue <- as.numeric(data$VoriLvlValue)

# Step 4: Create a scatterplot of VoriLvlValue (y-axis) vs Vori mg/kg/dose (x-axis)
ggplot(data, aes(x = `Vori mg/kg/dose`, y = VoriLvlValue, color = `Pre/Post Liver TPX`)) +
  geom_point(size = 3) +  # Increase point size
  stat_ellipse(level = 0.95) +  # Add ellipses with 95% confidence level
  labs(
    title = "Scatterplot of Voriconazole Serum Concentration vs Dose",
    x = "Vori mg/kg/dose",
    y = "Vori Serum Concentration (VoriLvlValue)",
    color = "Liver Transplant Status",
    caption = "Ellipses represent 95% confidence regions for Pre and Post Liver Transplant groups."
  ) +
  scale_y_continuous(breaks = seq(0, max(data$VoriLvlValue, na.rm = TRUE), by = 1)) +  # Set y-axis breaks to integers
  theme_minimal(base_size = 16) +  # Increase base font size for all elements
  theme(
    plot.title = element_text(size = 22, face = "bold"),  # Bigger title
    axis.title.x = element_text(size = 18, face = "bold"),  # Bigger x-axis title
    axis.title.y = element_text(size = 18, face = "bold"),  # Bigger y-axis title
    axis.text.x = element_text(size = 16),  # Bigger x-axis labels
    axis.text.y = element_text(size = 16),  # Bigger y-axis labels
    legend.title = element_text(size = 18, face = "bold"),  # Bigger legend title
    legend.text = element_text(size = 16),  # Bigger legend text
    plot.caption = element_text(size = 14, hjust = 0)  # Caption formatting
  )

# Step 5: Optionally save the plot to a file
# ggsave("scatterplot_voriconazole.png", width = 8, height = 6)
