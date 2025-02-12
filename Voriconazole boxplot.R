# Step 1: Load necessary libraries
library(dplyr)
library(ggplot2)
library(readxl)  # For reading Excel files

# Step 2: Load the Excel file
file_path <- file.choose()  # Choose your file
data <- read_excel(file_path)  # Load data from the Excel file

# Step 3: Convert columns to numeric where necessary
data$VoriDose <- as.numeric(data$VoriDose)  # Convert VoriDose to numeric
data <- data %>% filter(VoriDose != 0)  # Remove rows with VoriDose = 0

data$VoriLvlValue <- as.numeric(data$`VoriLvlValue`)  # Convert Voriconazole serum concentration to numeric
data$VoriLvlValue[is.na(data$VoriLvlValue) | data$VoriLvlValue < 0.5] <- 0  # Handle invalid values

data$Days_After_Transplant <- as.numeric(data$`Days level collected post transplant`)  # Convert to numeric
data$mg_kg_dose <- as.numeric(data$`Vori mg/kg/dose`)  # Convert mg/kg/dose to numeric
data$mg_kg_dose[is.na(data$mg_kg_dose)] <- 0  # Handle missing values

# Step 4: Bin mg/kg/day dosing into categories
data$mg_kg_dose_bin <- cut(data$mg_kg_dose, 
                           breaks = c(-Inf, 2, 3, Inf), 
                           labels = c("≤2", "2-3", "≥3"))

# Step 5: Define goal range (1–4 mg/L)
data$Within_Goal <- ifelse(data$VoriLvlValue >= 1 & data$VoriLvlValue <= 4, "Within Goal", "Outside Goal")

# Step 6: Create subsets for the pairwise comparisons
data_comparison_1 <- data %>% filter(mg_kg_dose_bin %in% c("≤2", "2-3"))
data_comparison_2 <- data %>% filter(mg_kg_dose_bin %in% c("2-3", "≥3"))

# Step 7: Calculate the contingency tables for each pairwise comparison
goal_table_1 <- table(data_comparison_1$mg_kg_dose_bin, data_comparison_1$Within_Goal)
goal_table_2 <- table(data_comparison_2$mg_kg_dose_bin, data_comparison_2$Within_Goal)

# Step 8: Perform Chi-square or Fisher's Exact Test for each pairwise comparison
# For comparison 1 (≤2 vs 2-3)
if (min(goal_table_1) < 5) {
  p_value_1 <- fisher.test(goal_table_1)$p.value
} else {
  p_value_1 <- chisq.test(goal_table_1)$p.value
}

# For comparison 2 (2-3 vs ≥3)
if (min(goal_table_2) < 5) {
  p_value_2 <- fisher.test(goal_table_2)$p.value
} else {
  p_value_2 <- chisq.test(goal_table_2)$p.value
}

# Step 9: Print the p-values for each pairwise comparison
cat("p-value for ≤2 vs 2-3 mg/kg/day dosing:", p_value_1, "\n")
cat("p-value for 2-3 vs ≥3 mg/kg/day dosing:", p_value_2, "\n")

# Step 10: Create the boxplot and add p-values and comparison brackets
ggplot(data, aes(x = mg_kg_dose_bin, y = VoriLvlValue)) + 
    # Add background shading for red and green regions
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1), fill = "red", alpha = 0.005) +  # Below 1 is red
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4, ymax = Inf), fill = "red", alpha = 0.005) +  # Above 5.5 is red
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 1, ymax = 4), fill = "green", alpha = 0.005) +  # Between 1 and 4 is green
    # Create boxplots with specific fill color (black for each bin)
    geom_boxplot(aes(fill = factor(mg_kg_dose_bin)), size = 1.25, outlier.shape = NA) +  # Remove boxplot outliers
    # Modify jitter to color outliers red
    geom_jitter(aes(color = ifelse(VoriLvlValue > 4 | VoriLvlValue < 1, "Extreme Outlier", "Within Goal")), 
                width = 0.1, height = 0.1, size = 2) +
    # Add p-values
    annotate("text", x = 1.5, y = max(data$VoriLvlValue, na.rm = TRUE) + 1.5, 
             label = paste("p =", round(p_value_1, 2)), size = 6, fontface = "bold") +  # p-value for ≤2 vs 2-3
    annotate("text", x = 2.5, y = max(data$VoriLvlValue, na.rm = TRUE) + 1.5, 
             label = paste("p =", round(p_value_2, 2)), size = 6, fontface = "bold") +  # p-value for 2-3 vs ≥3
    # Add comparison brackets (|------|) and vertical lines
    annotate("segment", x = 1, xend = 2, y = max(data$VoriLvlValue, na.rm = TRUE) + 1, 
             yend = max(data$VoriLvlValue, na.rm = TRUE) + 1, size = 1.5) +
    annotate("segment", x = 2, xend = 3, y = max(data$VoriLvlValue, na.rm = TRUE) + 1, 
             yend = max(data$VoriLvlValue, na.rm = TRUE) + 1, size = 1.5) +
    annotate("segment", x = 1, xend = 1, y = max(data$VoriLvlValue, na.rm = TRUE) + 1.1, 
             yend = max(data$VoriLvlValue, na.rm = TRUE) + 0.9, size = 1.5) +  # Vertical line at ≤2
    annotate("segment", x = 2, xend = 2, y = max(data$VoriLvlValue, na.rm = TRUE) + 1.1, 
             yend = max(data$VoriLvlValue, na.rm = TRUE) + 0.9, size = 1.5) +  # Vertical line at 2-3
    annotate("segment", x = 3, xend = 3, y = max(data$VoriLvlValue, na.rm = TRUE) + 1.1, 
             yend = max(data$VoriLvlValue, na.rm = TRUE) + 0.9, size = 1.5) +  # Vertical line at ≥3
    xlab("mg/kg/dose") +
    ylab("Voriconazole Serum Concentration (mg/dL)") +
    ggtitle("Boxplot of Voriconazole Serum Concentration by mg/kg/dose") +
    theme_minimal() +  # Add minimal theme for background color
    scale_fill_brewer(palette = "Set3") +
    scale_color_manual(values = c("Within Goal" = "black", "Extreme Outlier" = "red")) +
    scale_y_continuous(breaks = seq(0, max(data$VoriLvlValue, na.rm = TRUE) + 2, by = 1)) +
    labs(fill = "mg/kg/dose", color = "Goal Range") +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    )
