library(tidyverse)
library(gt)
library(flextable)

hawaiian_sales <- gt::pizzaplace |>
  filter(name == 'hawaiian') |>
  mutate(
    month = month(
    date, label = TRUE, abbr = FALSE,
    locale = 'en_US.UTF-8' # English month names
    ),
    quarter = paste0('Q', quarter(date))
  ) |>
  summarise(
    sales = n(),
    revenue = sum(price),
    .by = c(month, quarter)
  )
hawaiian_sales
flextable(hawaiian_sales)

hawaiian_sales |>
  as_grouped_data(groups='quarter') |>
  flextable() |>
  set_header_labels(
    month = 'Month',
    quarter = 'Quarter',
    sales = 'Sales',
    revenue = 'Revenue'
  ) |>
  add_header_lines(
    'Hawaiian Pizza Sales in 2015'
  ) |>
  add_header_lines(
    'Based on the fake `pizzaplace` data from `{gt}`'
  ) |>
  colformat_double(
    j = 'revenue',
    digits=2,
    prefix= '$'
  )

order_of_months_w_total <- c(
  levels(hawaiian_sales$month),
  'Total'
)
totals <- hawaiian_sales |>
  summarise(
    month = factor(
      'Total',
      levels = order_of_months_w_total
    ),
    sales = n(),
    revenue = sum(revenue),
    .by = quarter
  )
combined_data <- hawaiian_sales |>
  mutate(
    month = factor(
      month,
      levels = order_of_months_w_total,
      ordered = FALSE
    )
  ) |>
  bind_rows(totals) |>
  arrange(quarter, month)
combined_data

table_with_totals <- combined_data |>
  as_grouped_data(groups = 'quarter') |>
  flextable() |>
  set_header_labels (
    month = 'Month',
    quarter = 'Quarter',
    sales = 'Sales',
    revenue = 'Revenue'
  ) |>
  add_header_lines(
    'Hawaiian Pizza Sales in 2015'
  ) |>
  add_header_lines(
    'Based on the fake `pizzaplace` data from `{gt}`'
  ) |>
  colformat_double(
    j = 'revenue',
    digits = 2,
    prefix = '$'
  )
table_with_totals

styled_table <- table_with_totals |>
  bg(
    i =~!is.na(quarter),
    bg = '#E7EDF3'
  ) |>
  bold(
    i = ~(month == 'Total')
  )
styled_table

styled_table |>
  save_as_docx(
    path = 'word_export.docx'
  )

styled_table |>
  save_as_pptx(
    path = 'word_export.pptx'
  )
