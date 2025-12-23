library(tidyverse)
library(gtsummary)
library(flextable)
library(gt)
library(table1)
data(CO2)
head(CO2)

# basic summary table
CO2 %>% select(!c(Plant,conc)) %>% tbl_summary()

# summary split by a categorical variable
CO2 %>% select(!c(Plant,conc)) %>% tbl_summary(by = Type)

# summary split by a categorical variable with p-value
CO2 %>% select(!c(Plant,conc)) %>% 
  tbl_summary(by = Type) %>% add_p()

# include overall, extra heading, custom stats
CO2 %>% select(!c(Plant,conc)) %>%
  tbl_summary(
    by = Type,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ), digits = all_continuous() ~ 2) %>% 
  add_p() %>% add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Location**") %>%
  bold_labels()

# crosstabs
CO2_tab <- CO2 %>%
  tbl_cross(
    row = Type,
    col = Treatment,
    percent = "cell"
  ) %>%
  add_p() %>%
  bold_labels()
CO2_tabhow

CO2_tab |>
  as_gt() |> 
    gt::gtsave(filename = "CO2_table.docx")




# Load required packages
library(dplyr)
library(gtsummary)
library(officer)
library(flextable)

# Step 1: Clean and prepare the data
df_clean <- df %>%
  filter(
    !is.na(day_of_defervescence),
    !is.na(DHF1997),
    !is.na(MinOfPlatelet),
    day_of_defervescence >= -3,
    day_of_defervescence <= 3
  ) %>%
  mutate(
    plt_cat = ifelse(MinOfPlatelet < 100, "<100", ">=100"),
    DHF1997 = factor(DHF1997, levels = c("No", "Yes")),
    plt_cat = factor(plt_cat, levels = c("<100", ">=100"))
  )

# Step 2: Initialize Word document
doc <- read_docx()

# Step 3: Loop through days -2 to 2
for (d in -2:2) {
  doc <- doc %>% body_add_par(paste("Day", d), style = "heading 2")
  
  data_day <- df_clean %>% filter(day_of_defervescence == d)
  
  # Row %
  tbl_row <- data_day %>%
    tbl_cross(row = plt_cat, col = DHF1997, percent = "row", margin = "row") %>%
    as_flextable()
  
  doc <- doc %>%
    body_add_par("Row Percent", style = "heading 3") %>%
    body_add_flextable(tbl_row)
  
  # Column %
  tbl_col <- data_day %>%
    tbl_cross(row = plt_cat, col = DHF1997, percent = "column", margin = "column") %>%
    as_flextable()
  
  doc <- doc %>%
    body_add_par("Column Percent", style = "heading 3") %>%
    body_add_flextable(tbl_col) %>%
    body_add_par("", style = "Normal")
}

# Step 4: Save the Word file
print(doc, target = "defervescence_tbl_cross.docx")

