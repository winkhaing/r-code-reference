# Check if 'pacman' package is installed
if (!requireNamespace("pacman", quietly = TRUE)) {
  # Install 'pacman' if not installed
  install.packages("pacman")
}

# Load the 'pacman' library
library(pacman)
pacman::p_load(table1, readr, tidyverse, flextable)

rm(list=ls())

framingham_data <- read_csv("data/Framingham.csv")

View(framingham_data)

## Using label command ----
label(framingham_data$age) <- "Age (year)"
label(framingham_data$bmi) <- "BMI"

framingham_data$sex <- factor(framingham_data$sex,
                                levels = c(1, 2),
                                labels = c("Male", "Female")
)

framingham_data$sex

f_des1 <- table1(~ age + bmi | sex, data=framingham_data)
f_des1
f_table1 <- t1flex(f_des1)
f_table1 %>%  save_as_docx(path="f_tabl1.docx")


framingham_data <- framingham_data %>%
  mutate(
    age_gp = case_when(
      age < 13 ~ "Child",
      age >= 13 & age < 18 ~ "Teenager",
      age >= 18 & age < 35 ~ "Young Adult",
      age >= 35 & age < 60 ~ "Middle Aged",
      age >= 60 ~ "Senior Citizen",
      TRUE ~ NA_character_  # Default case
    )
  )

# Re-categorize 'BMI' into 'BMI_gp'
framingham_data <- framingham_data %>%
  mutate(
    bmi_gp = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 24.9 ~ "Normal weight",
      bmi >= 24.9 & bmi < 29.9 ~ "Overweight",
      bmi >= 29.9 & bmi < 34.9 ~ "Obesity I",
      bmi >= 34.9 & bmi < 39.9 ~ "Obesity II",
      bmi >= 39.9 ~ "Obesity III",
      TRUE ~ NA_character_  # Default case
    )
  )

f_des2 <- table1(~ sex + bmi | age_gp, data=framingham_data)
f_des2

f_des3 <- table1(~ sex + age_gp + bmi_gp, data=framingham_data)
f_des3

