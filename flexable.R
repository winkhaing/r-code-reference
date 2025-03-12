library(dplyr)

dat <- mutate(mtcars, cyl = factor(cyl))

ft <- group_by(dat, cyl) %>% 
  summarise(
    across(
      all_of(c("disp", "mpg")),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      )
    )
  ) %>% 
  flextable() %>% 
  separate_header() %>% 
  theme_vanilla() %>% 
  align(align = "center", part = "all") %>% 
  colformat_double(digits = 2) %>% 
  labelizor(labels = c(cyl = "Number of cylinders",
                       disp = "Displacement",
                       mpg = "Miles/(US) gallon",
                       mean = "µ", sd = "σ"
  ), part = "all") %>% 
  autofit() %>% width(j = 1, width = .8) %>% 
  add_header_lines("used dataset: mtcars")
ft

install.packages("tables")
library(tables)
tab <- tabular(
  (Species + 1) ~ (n = 1) + Format(digits = 2) *
    (Sepal.Length + Sepal.Width) * (mean + sd),
  data = iris
)
as_flextable(tab)

Sex <- factor(sample(c("Male", "Female"), 100, rep=TRUE))
Status <- factor(sample(c("low", "medium", "high"), 100, rep=TRUE))
z <- rnorm(100)+5
fmt <- function(x) {
  s <- format(x, digits=2)
  even <- ((1:length(s)) %% 2) == 0
  s[even] <- sprintf("(%s)", s[even])
  s
}
tab <- tabular( Justify(c)*Heading()*z*Sex*Heading(Statistic)*Format(fmt())*(mean+sd) 
                ~ Status )
tab
tab[1:2, c(2,3,1)]

dat <- group_by(warpbreaks, wool, tension) %>%
  summarise(breaks = mean(breaks), .groups = "drop")

cft_1 <- tabulator(
  x = dat,
  rows = "wool",
  columns = "tension",
  `μ` = as_paragraph(breaks),
  `n` = as_paragraph(length(breaks))
)

ft_1 <- as_flextable(cft_1)
ft_1

proc_freq(mtcars, "gear", "vs")
proc_freq(mtcars, "gear", "vs", weight = "wt")

library(tables)
set_flextable_defaults(theme_fun = theme_vader)
tab <- tabular( (cut*color + 1) ~ (n=1) + Format(digits=2)*
                  (price + x)*(mean + sd), data=ggplot2::diamonds)
print(tab[1:10,])

as_flextable(tab, spread_first_col = TRUE,
             row_title = as_paragraph(
               colorize(as_b(.row_title), color = "#ff0000"))) |> 
  labelizor(part = "header", 
            labels = c("mean" = "µ", sd = "σ"))

set_flextable_defaults(theme_fun = theme_booktabs)

subdat <- ggplot2::diamonds %>%
  filter(
    cut %in% c("Fair", "Good", "Very Good"),
    clarity %in% c("I1", "SI1", "VS2"),
    color %in% c("F", "G", "H")
  )

dat <- group_by(subdat, cut, color, clarity) %>%
  summarise(
    across(
      .cols = c(z, y),
      .fns = list(
        mean = ~ mean(.x),
        sd = ~ sd(.x)
      )
    ),
    .groups = "drop"
  )
head(dat)

tabulator(
  x = dat, rows = c("cut", "color"),
  columns = "clarity",
  `y stats` = as_paragraph(
    fmt_avg_dev(y_mean, y_sd)
  )
) |>
  as_flextable()

ft <- tabulator(
  x = dat, rows = c("cut", "color"),
  columns = "clarity",
  `y stats` = as_paragraph(fmt_avg_dev(y_mean, y_sd)),
  `z stats` = as_paragraph(fmt_avg_dev(z_mean, z_sd))
) |>
  as_flextable()
ft

ft <- color(ft, i = ~ cut %in% "Very Good", color = "blue")
ft <- add_header_lines(x = ft, "blah blah blah blah blah blah blah blah blah blah")
ft

nstat <- group_by(subdat, cut) %>% tally()
nstat

tab <- tabulator(
  x = dat, rows = c("cut", "color"),
  columns = "clarity",
  hidden_data = nstat,
  row_compose = list(
    cut = as_paragraph(
      as_chunk(cut),
      as_chunk(n, formatter = fmt_header_n)
    )
  ),
  `y stats` = as_paragraph(fmt_avg_dev(y_mean, y_sd)),
  `z stats` = as_paragraph(fmt_avg_dev(z_mean, z_sd))
)
as_flextable(tab)

tab <- tabulator(
  x = dat, rows = c("cut", "color"),
  columns = "clarity",
  datasup_last = nstat,
  `y stats` = as_paragraph(fmt_avg_dev(y_mean, y_sd)),
  `z stats` = as_paragraph(fmt_avg_dev(z_mean, z_sd))
)
as_flextable(tab)

ft <- as_flextable(tab, separate_with = "cut")
ft

ft <- as_flextable(tab, separate_with = "cut", sep_w = 0)
ft

ft <- as_flextable(tab,
                   spread_first_col = TRUE,
                   rows_alignment = "right"
) |>
  align(i = ~ !is.na(cut), align = "left")
ft

ft_merge <- flextable(head(mtcars), cwidth = .5)
ft_merge <- merge_at(ft_merge, i = 1:2, j = 1:2)
ft_merge



library(flextable)
library(dplyr)

# Create the data frame
data <- data.frame(
  Characteristic = c("Age (years)", "Mean (SD)", "Median (IQR)", "Range"),
  Overall = c("", "45.7 (15.4)", "46.5 (32.8, 55.0)", "22.0, 82.0"),
  No = c("", "45.3 (15.2)", "46.0 (34.5, 53.5)", "22.0, 82.0"),
  Mild = c("", "35.6 (10.0)", "36.0 (29.0, 39.0)", "24.0, 50.0"),
  Significant = c("", "49.6 (16.3)", "52.0 (41.8, 64.0)", "22.0, 74.0"),
  p_value = c("0.2", "", "", "")
)

# Create the flextable
ft <- flextable(data) %>%
  add_header_row(
    values = c("Characteristic", "Plasma Leakage", "p-value"),
    colwidths = c(1, 4, 1)
  ) %>%
  add_header_row(
    values = c("", paste0(c("Overall", "No", "Mild", "Significant"), "(N = ", c("48", "27", "5", "16"), ")¹"), "²"),
    top = FALSE
  ) %>%
  merge_at(i = 1, j = 2:5, part = "header") %>%
  merge_at(i = 1:2, j = 1, part = "header") %>%
  merge_at(i = 1:2, j = 6, part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  width(j = 1, width = 2) %>%
  width(j = 2:6, width = 1.5) %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  border_outer() %>%
  border_inner() %>%
  add_footer_lines("¹ Values are N (%) unless otherwise specified") %>%
  add_footer_lines("² p-value calculated using chi-square test or Fisher's exact test for categorical variables and one-way ANOVA or Kruskal-Wallis test for continuous variables")

# Display the table
ft
