install.packages("epikit")
library("knitr")
library("magrittr")
library("epikit")

set.seed(1)
x <- sample(0:100, 20, replace = TRUE)
y <- ifelse(x < 2, sample(48, 20, replace = TRUE), NA)
df <- data.frame(
  age_years = age_categories(x, upper = 80), 
  age_months = age_categories(y, upper = 16, by = 6)
)
df %>% 
  group_age_categories(years = age_years, months = age_months)

attack_rate(10, 50)
case_fatality_rate(2, 50)
mortality_rate(40, 50000)

install.packages("outbreaks")
library("outbreaks")
library("dplyr")
case_fatality_rate_df(ebola_sim_clean$linelist, 
                      outcome == "Death", 
                      group = gender,
                      add_total = TRUE,
                      mergeCI = TRUE
)

fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
df  <- data.frame(v = names(coef(fit)), e = coef(fit), confint(fit), row.names = NULL)
names(df) <- c("variable", "estimate", "lower", "upper")
print(df)

unite_ci(df, "slope (CI)", estimate, lower, upper, m100 = FALSE, percent = FALSE)
merge_ci_df(df, e = 2)

find_breaks(100) # four breaks from 1 to 100
find_breaks(100, snap = 20) # four breaks, snap to the nearest 20
find_breaks(100, snap = 20, ceiling = TRUE) # include the highest number

# get population counts based on proportion, stratified
gen_population(groups = c("0-4","5-14","15-29","30-44","45+"), 
               strata = c("Male", "Female"), 
               proportions = c(0.079, 0.134, 0.139, 0.082, 0.067))

# get population counts based on counts, stratified - type out counts
# for each group and strata
gen_population(groups = c("0-4","5-14","15-29","30-44","45+"), 
               strata = c("Male", "Female"), 
               counts = c(20, 10, 30, 40, 0, 0, 40, 30, 20, 20))

df <- data.frame(
  `a n` = 1:6,
  `a prop` = round((1:6) / 6, 2),
  `a deff` = round(pi, 2),
  `b n` = 6:1,
  `b prop` = round((6:1) / 6, 2),
  `b deff` = round(pi * 2, 2),
  check.names = FALSE
)
knitr::kable(df)

df %>%
  rename_redundant("%" = "prop", "Design Effect" = "deff") %>%
  augment_redundant(" (n)" = " n$") %>%
  knitr::kable()

