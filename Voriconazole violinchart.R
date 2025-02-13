# Load required packages
library(readxl)  # To read Excel files
library(ggplot2) # To create the plot
library(dplyr)   # For data manipulation

# Step 1: Open Excel file and load data
file_path <- file.choose()  # Prompt to select file
data <- read_excel(file_path)

# Step 2: Filter out rows where VoriDose (column E) is 0
data <- data[data$VoriDose != 0, ]

# Step 3: Replace any VoriLvlValue < 0.5 with 0
data$VoriLvlValue[data$VoriLvlValue < 0.5] <- 0

# Ensure VoriLvlValue is numeric
data$VoriLvlValue <- as.numeric(data$VoriLvlValue)

# Step 4: Create dose bins using cut() function (adjustable breaks)
data$DoseBin <- cut(data$`Vori mg/kg/dose`, 
                    breaks = c(-Inf, 2, Inf), 
                    labels = c("<2", ">2"),
                    right = FALSE)

# Step 5: Reorder `Pre/Post Liver TPX` so that "Pre" comes before "Post"
data$`Pre/Post Liver TPX` <- factor(data$`Pre/Post Liver TPX`, levels = c("Pre", "Post"))

# Step 6: Create a violin plot with DoseBins on the x-axis and Pre/Post liver transplant status as fill
ggplot(data, aes(x = DoseBin, y = VoriLvlValue, fill = `Pre/Post Liver TPX`)) +
  # Background shading for therapeutic range
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1), fill = "red", alpha = 0.005) +  # Below 1 is red
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4, ymax = Inf), fill = "red", alpha = 0.005) +  # Above 4 is red
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 1, ymax = 4), fill = "green", alpha = 0.005) +  # Therapeutic range

  # Violin plot with thicker outline
  geom_violin(trim = FALSE, position = position_dodge(width = 0.8), size = 1.2, color = "black") +  

  # Custom colors
  scale_fill_manual(values = c("Pre" = "blue", "Post" = "orange")) +  
  scale_color_manual(values = c("Pre" = "blue", "Post" = "orange")) +  # Match jitter point colors
  
  # Labels and title
  labs(title = "Violin Plot of Voriconazole Serum concentration versus mg/kg/dose",
       x = "Voriconazole mg/kg/dose",
       y = "Voriconazole Serum Concentration (mcg/mL)",
       fill = "Liver Transplant Status (Pre/Post)") +

  # Adjust font sizes for better readability
  theme_minimal(base_size = 16) +  
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  ) +
  # Ensure y-axis ticks are integers with a step size of 1
  scale_y_continuous(breaks = seq(0, max(data$VoriLvlValue, na.rm = TRUE), by = 1))
