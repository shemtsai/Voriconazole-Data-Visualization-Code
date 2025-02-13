# Voriconazole Data Visualization and Analysis

This repository contains R scripts to visualize and analyze Voriconazole serum concentration data for patients undergoing liver transplantation. The scripts focus on different aspects of the data, including dose-response relationships, the therapeutic range, and statistical analysis of dosing bins. Three visualizations are provided:

1. **Violin Plot**: Displays Voriconazole serum concentration across different dose bins with the inclusion of pre- and post-liver transplant status.
2. **Scatterplot**: Displays individual data points for Voriconazole serum concentrations with a focus on pre- and post-transplant patients.
3. **Boxplot with p-values**: Compares Voriconazole serum concentrations across different dosing categories, including statistical significance testing for each pair of dosing bins.

## Purpose

This repository aims to provide tools for the following:
- **Visualize** Voriconazole serum concentrations and dosing regimens in patients post-liver transplant.
- **Understand** the relationship between Voriconazole dose and serum concentration.
- **Examine** the distribution of serum concentration against therapeutic goals (1-4 µg/mL).

## Required Libraries

These R packages are required for data processing and visualization:

- `readxl` - For reading Excel files.
- `ggplot2` - For creating plots (violin, scatter, boxplot).
- `dplyr` - For data manipulation.

You can install the required packages using:

```r
install.packages(c("readxl", "ggplot2", "dplyr"))
