# CO2 Dataset - Comprehensive Exploratory Data Analysis
# =======================================================

# Load required libraries
library(tidyverse)
library(gtsummary)
library(gt)
library(corrplot)
library(GGally)

# Check if optional packages are available
packages_to_check <- c("psych", "plotly")
for(pkg in packages_to_check) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Package", pkg, "not available. Some analyses will be skipped.\n")
  }
}

# Load the CO2 dataset
data(CO2)
cat("CO2 Dataset Loaded Successfully!\n")

# =======================================================
# 1. BASIC DATA EXPLORATION
# =======================================================

cat("\n=== BASIC DATA EXPLORATION ===\n")

# Dataset structure
cat("\nDataset Structure:\n")
str(CO2)

# Dataset dimensions
cat("\nDataset Dimensions:\n")
cat("Rows:", nrow(CO2), "\n")
cat("Columns:", ncol(CO2), "\n")

# First few rows
cat("\nFirst 10 rows:\n")
print(head(CO2, 10))

# Last few rows
cat("\nLast 10 rows:\n")
print(tail(CO2, 10))

# Variable names and types
cat("\nVariable Information:\n")
print(sapply(CO2, class))

# =======================================================
# 2. DATA QUALITY ASSESSMENT
# =======================================================

cat("\n=== DATA QUALITY ASSESSMENT ===\n")

# Missing values
cat("\nMissing Values:\n")
missing_summary <- CO2 %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  mutate(missing_percent = round(missing_count / nrow(CO2) * 100, 2))

print(missing_summary)

# Check for duplicates
cat("\nDuplicate rows:", sum(duplicated(CO2)), "\n")

# Summary statistics
cat("\nSummary Statistics:\n")
print(summary(CO2))

# =======================================================
# 3. DESCRIPTIVE STATISTICS
# =======================================================

cat("\n=== DESCRIPTIVE STATISTICS ===\n")

# Detailed descriptive statistics for numerical variables
cat("\nDetailed Statistics for Numerical Variables:\n")
numerical_vars <- CO2 %>% select_if(is.numeric)
print(summary(numerical_vars))

# If psych package is available, use describe function
if (requireNamespace("psych", quietly = TRUE)) {
  cat("\nAdvanced Descriptive Statistics:\n")
  print(psych::describe(numerical_vars))
}

# Frequency tables for categorical variables
cat("\nFrequency Tables for Categorical Variables:\n")

# Plant distribution
cat("\nPlant Distribution:\n")
plant_freq <- table(CO2$Plant)
print(plant_freq)
print(prop.table(plant_freq))

# Type distribution
cat("\nType Distribution:\n")
type_freq <- table(CO2$Type)
print(type_freq)
print(prop.table(type_freq))

# Treatment distribution
cat("\nTreatment Distribution:\n")
treatment_freq <- table(CO2$Treatment)
print(treatment_freq)
print(prop.table(treatment_freq))

# Cross-tabulation
cat("\nCross-tabulation: Type vs Treatment:\n")
cross_tab <- table(CO2$Type, CO2$Treatment)
print(cross_tab)
print(prop.table(cross_tab))

# =======================================================
# 4. VISUALIZATION - UNIVARIATE ANALYSIS
# =======================================================

cat("\n=== CREATING UNIVARIATE VISUALIZATIONS ===\n")

# Distribution of CO2 uptake
p1 <- ggplot(CO2, aes(x = uptake)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "black") +
  labs(title = "Distribution of CO2 Uptake", 
       x = "CO2 Uptake (umol/m^2 sec)", 
       y = "Frequency") +
  theme_minimal()

print(p1)

# Box plot of CO2 uptake
p2 <- ggplot(CO2, aes(y = uptake)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Box Plot of CO2 Uptake", 
       y = "CO2 Uptake (umol/m^2 sec)") +
  theme_minimal()

print(p2)

# Distribution of concentration
p3 <- ggplot(CO2, aes(x = conc)) +
  geom_histogram(bins = 15, fill = "coral", alpha = 0.7, color = "black") +
  labs(title = "Distribution of CO2 Concentration", 
       x = "CO2 Concentration (mL/L)", 
       y = "Frequency") +
  theme_minimal()

print(p3)

# Bar plots for categorical variables
p4 <- ggplot(CO2, aes(x = Type, fill = Type)) +
  geom_bar(alpha = 0.7) +
  labs(title = "Distribution of Plant Type", 
       x = "Plant Type", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

print(p4)

p5 <- ggplot(CO2, aes(x = Treatment, fill = Treatment)) +
  geom_bar(alpha = 0.7) +
  labs(title = "Distribution of Treatment", 
       x = "Treatment", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

print(p5)

# =======================================================
# 5. BIVARIATE ANALYSIS
# =======================================================

cat("\n=== BIVARIATE ANALYSIS ===\n")

# Scatter plot: Concentration vs Uptake
p6 <- ggplot(CO2, aes(x = conc, y = uptake)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "CO2 Concentration vs Uptake", 
       x = "CO2 Concentration (mL/L)", 
       y = "CO2 Uptake (umol/m^2 sec)") +
  theme_minimal()

print(p6)

# Scatter plot colored by Type
p7 <- ggplot(CO2, aes(x = conc, y = uptake, color = Type)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "CO2 Concentration vs Uptake by Plant Type", 
       x = "CO2 Concentration (mL/L)", 
       y = "CO2 Uptake (umol/m^2 sec)",
       color = "Plant Type") +
  theme_minimal()

print(p7)

# Scatter plot colored by Treatment
p8 <- ggplot(CO2, aes(x = conc, y = uptake, color = Treatment)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "CO2 Concentration vs Uptake by Treatment", 
       x = "CO2 Concentration (mL/L)", 
       y = "CO2 Uptake (umol/m^2 sec)",
       color = "Treatment") +
  theme_minimal()

print(p8)

# Box plots for group comparisons
p9 <- ggplot(CO2, aes(x = Type, y = uptake, fill = Type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "CO2 Uptake by Plant Type", 
       x = "Plant Type", 
       y = "CO2 Uptake (umol/m^2 sec)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p9)

p10 <- ggplot(CO2, aes(x = Treatment, y = uptake, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "CO2 Uptake by Treatment", 
       x = "Treatment", 
       y = "CO2 Uptake (umol/m^2 sec)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p10)

# =======================================================
# 6. MULTIVARIATE ANALYSIS
# =======================================================

cat("\n=== MULTIVARIATE ANALYSIS ===\n")

# Faceted scatter plots
p11 <- ggplot(CO2, aes(x = conc, y = uptake, color = Treatment)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Type) +
  labs(title = "CO2 Concentration vs Uptake by Type and Treatment", 
       x = "CO2 Concentration (mL/L)", 
       y = "CO2 Uptake (umol/m^2 sec)",
       color = "Treatment") +
  theme_minimal()

print(p11)

# Interaction plot
p12 <- CO2 %>%
  group_by(Type, Treatment, conc) %>%
  summarise(mean_uptake = mean(uptake), .groups = "drop") %>%
  ggplot(aes(x = conc, y = mean_uptake, color = Treatment, linetype = Type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Mean CO2 Uptake by Concentration, Type, and Treatment", 
       x = "CO2 Concentration (mL/L)", 
       y = "Mean CO2 Uptake (umol/m^2 sec)",
       color = "Treatment",
       linetype = "Plant Type") +
  theme_minimal()

print(p12)

# =======================================================
# 7. STATISTICAL SUMMARIES
# =======================================================

cat("\n=== STATISTICAL SUMMARIES ===\n")

# Summary table using gtsummary
summary_table <- CO2 %>%
  select(-Plant) %>%  # Remove Plant ID for cleaner summary
  tbl_summary(
    by = Type,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_p() %>%
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Plant Type**") %>%
  bold_labels()

print(summary_table)

# Cross-tabulation table
cross_table <- CO2 %>%
  tbl_cross(
    row = Type,
    col = Treatment,
    percent = "cell"
  ) %>%
  add_p() %>%
  bold_labels()

print(cross_table)

# =======================================================
# 8. CORRELATION ANALYSIS
# =======================================================

cat("\n=== CORRELATION ANALYSIS ===\n")

# Correlation matrix for numerical variables
numerical_data <- CO2 %>% select_if(is.numeric)
correlation_matrix <- cor(numerical_data)

cat("\nCorrelation Matrix:\n")
print(correlation_matrix)

# Correlation plot
corrplot(correlation_matrix, 
         method = "circle", 
         type = "upper",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         title = "Correlation Matrix of Numerical Variables")

# =======================================================
# 9. ADVANCED VISUALIZATIONS
# =======================================================

cat("\n=== ADVANCED VISUALIZATIONS ===\n")

# Pairs plot
pairs_plot <- GGally::ggpairs(
  CO2, 
  columns = c("conc", "uptake"),
  aes(color = Type, alpha = 0.5),
  title = "Pairs Plot of CO2 Variables"
)

print(pairs_plot)

# Violin plots
p13 <- ggplot(CO2, aes(x = Type, y = uptake, fill = Treatment)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.8)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), alpha = 0.8) +
  labs(title = "Distribution of CO2 Uptake by Type and Treatment", 
       x = "Plant Type", 
       y = "CO2 Uptake (umol/m^2 sec)",
       fill = "Treatment") +
  theme_minimal()

print(p13)

# =======================================================
# 10. KEY FINDINGS SUMMARY
# =======================================================

cat("\n=== KEY FINDINGS SUMMARY ===\n")

# Calculate key statistics
cat("\n--- Dataset Overview ---\n")
cat("Total observations:", nrow(CO2), "\n")
cat("Number of plants:", length(unique(CO2$Plant)), "\n")
cat("Plant types:", paste(unique(CO2$Type), collapse = ", "), "\n")
cat("Treatments:", paste(unique(CO2$Treatment), collapse = ", "), "\n")

cat("\n--- CO2 Uptake Statistics ---\n")
uptake_stats <- CO2 %>%
  summarise(
    mean_uptake = mean(uptake),
    median_uptake = median(uptake),
    sd_uptake = sd(uptake),
    min_uptake = min(uptake),
    max_uptake = max(uptake)
  )

cat("Mean uptake:", round(uptake_stats$mean_uptake, 2), "umol/m^2 sec\n")
cat("Median uptake:", round(uptake_stats$median_uptake, 2), "umol/m^2 sec\n")
cat("Standard deviation:", round(uptake_stats$sd_uptake, 2), "\n")
cat("Range:", round(uptake_stats$min_uptake, 2), "to", round(uptake_stats$max_uptake, 2), "\n")

cat("\n--- Group Comparisons ---\n")
type_comparison <- CO2 %>%
  group_by(Type) %>%
  summarise(
    n = n(),
    mean_uptake = mean(uptake),
    sd_uptake = sd(uptake),
    .groups = "drop"
  )

treatment_comparison <- CO2 %>%
  group_by(Treatment) %>%
  summarise(
    n = n(),
    mean_uptake = mean(uptake),
    sd_uptake = sd(uptake),
    .groups = "drop"
  )

cat("Uptake by Plant Type:\n")
print(type_comparison)

cat("\nUptake by Treatment:\n")
print(treatment_comparison)

# Correlation insights
cat("\n--- Correlation Insights ---\n")
conc_uptake_cor <- cor(CO2$conc, CO2$uptake)
cat("Correlation between concentration and uptake:", round(conc_uptake_cor, 3), "\n")

if (abs(conc_uptake_cor) > 0.7) {
  cat("Strong correlation detected!\n")
} else if (abs(conc_uptake_cor) > 0.3) {
  cat("Moderate correlation detected.\n")
} else {
  cat("Weak correlation detected.\n")
}

cat("\n=== EDA COMPLETED SUCCESSFULLY ===\n")
cat("All visualizations and analyses have been generated.\n")
cat("Review the plots and statistical summaries above for insights.\n")