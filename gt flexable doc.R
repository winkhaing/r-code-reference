library(tidyverse)
library(gtsummary)
library(officer)
library(flextable)

mtcars_table <- mtcars %>%
  select(mpg, cyl, vs, am) %>%
  dplyr::mutate(am = factor(am, labels = c("Manual", "Automatic"))) %>%
  # create a new variable to display N in table
  tbl_summary(by = am) %>%
  # this is a gtsummary function that allows you to edit the header
  modify_header(all_stat_cols() ~ "**{level}** N = {N}") %>%
  as_flex_table()

mtcars_table |>
  save_as_docx(
    path = 'mtcars_table.docx'
  )


tf <- tempfile(fileext = ".docx")

library(officer)
ft1 <- flextable(head(iris))
save_as_docx(ft1, path = tf)


ft2 <- flextable(head(mtcars))
sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 8.3, height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar()
)
save_as_docx(
  `iris table` = ft1, `mtcars table` = ft2,
  path = tf, pr_section = sect_properties
)
