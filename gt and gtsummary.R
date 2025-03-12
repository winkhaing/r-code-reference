if (!require("pak")) install.packages("pak")
pak::pkg_install(c("gt", "gtsummary", "dplyr", "tidyr"), upgrade = TRUE)

# Required packages
library(gt)
library(gtsummary)
library(dplyr)
library(tidyr)

# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed
# `sp500` table data
sp500 |>
  dplyr::filter(date >= start_date & date <= end_date) |>
  dplyr::select(-adj_close) |>
  gt() |>
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) |>
  fmt_currency() |>
  fmt_date(columns = date, date_style = "wd_m_day_year") |>
  fmt_number(columns = volume, suffixing = TRUE)




# Example 1: gt package - Creating a custom formatted table
mtcars_gt <- mtcars %>%
  head(5) %>%
  gt() %>%
  tab_header(
    title = "Motor Trend Car Data",
    subtitle = "Top 5 Cars"
  ) %>%
  fmt_number(
    columns = c(mpg, disp, wt),
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightblue")
    ),
    locations = cells_column_labels()
  )
mtcars_gt 
# Example 2: gtsummary - Creating statistical summary tables
# Descriptive statistics
mtcars_summary <- mtcars %>%
  select(mpg, cyl, wt) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_n() %>%
  modify_header(label = "**Variable**")
mtcars_summary 
# Example 3: gtsummary - Regression model summary
model <- lm(mpg ~ wt + cyl, data = mtcars)
model_summary <- tbl_regression(model,
                                label = list(
                                  wt ~ "Weight",
                                  cyl ~ "Cylinders"
                                )
) %>%
  add_glance_source_note()
model_summary
# Example 4: gtsummary - Comparing groups
mtcars_compare <- mtcars %>%
  mutate(vs = factor(vs, labels = c("V-shaped", "Straight"))) %>%
  select(mpg, wt, vs) %>%
  tbl_summary(
    by = vs,
    missing = "no"
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n()
mtcars_compare 

tab_rtf <-
  gtcars |>
  dplyr::select(mfr, model) |>
  dplyr::slice(1:2) |>
  gt() |>
  tab_header(
    title = md("Data listing from **gtcars**"),
    subtitle = md("`gtcars` is an R dataset")
  ) |> gtsave("tab_2.docx")

tab_1 <-
  gtcars |>
  dplyr::select(model, year, hp, trq) |>
  dplyr::slice(1:5) |>
  gt(rowname_col = "model") |>
  tab_stubhead(label = "car")
tab_1 |> gtsave("tab_1.docx")


## Comparision gt vs gtsummary -------------------------------------------------


comparison_data <- data.frame(
  Feature = c(
    "Primary Purpose",
    "Table Types",
    "Learning Curve",
    "Statistical Features",
    "Customization Level",
    "Data Input",
    "Common Use Cases",
    "Dependencies",
    "Output Format",
    "Formula Interface"
  ),
  gt = c(
    "General table creation and formatting",
    "Any type of data table",
    "Steeper - requires more code for basic tables",
    "Basic, requires manual implementation",
    "High - complete control over every aspect",
    "Data frames, matrices, tibbles",
    "Custom reports, data presentation, publication tables",
    "Standalone package",
    "HTML, RTF, PDF, Word",
    "No built-in formula interface"
  ),
  gtsummary = c(
    "Statistical and clinical summary tables",
    "Summary statistics, regression results, comparison tables",
    "Gentler - pre-built functions for common tables",
    "Rich statistical functions built-in",
    "Medium - preset formats with some customization",
    "Data frames with focus on statistical data",
    "Clinical research, statistical reports, regression summaries",
    "Built on top of gt package",
    "Same as gt, optimized for statistical reporting",
    "Supports formula interface for models"
  )
) 

comparison_table <- comparison_data %>%
  gt() %>%
  tab_header(
    title = "Comparison of gt vs gtsummary Packages",
    subtitle = "Feature-by-Feature Comparison"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f0f0f0"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#e6e6ff"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = gt)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#ffe6e6"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = gtsummary)
  ) %>%
  cols_width(
    Feature ~ px(200),
    gt ~ px(300),
    gtsummary ~ px(300)
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = Feature)
  )

comparison_table
