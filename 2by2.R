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
CO2_tab

CO2_tab |>
  as_gt() |> 
    gt::gtsave(filename = "CO2_table.docx")
