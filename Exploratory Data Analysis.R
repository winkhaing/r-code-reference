# Reference
# https://cosimameyer.com/post/exploratory-data-analysis-in-r/

# Data
install.packages("palmerpenguins")

# EDA libraries
install.packages("DataExplorer")
install.packages("dlookr")
install.packages("gtsummary")
install.packages("Hmisc")
install.packages("naniar")
install.packages("overviewR")
install.packages("skimr")
install.packages("SmartEDA")
install.packages("summarytools")

library(palmerpenguins)
data(package = 'palmerpenguins')
dim(penguins)
head(penguins)
tail(penguins)
str(penguins)
summary(penguins)
library(DataExplorer)
create_report(penguins)

library(dlookr)
diagnose(penguins)
diagnose_numeric(penguins)
diagnose_category(penguins)

library(Hmisc)
describe(penguins)

library(naniar)
penguins %>% 
  miss_var_summary()
penguins %>%
  dplyr::group_by(year) %>%
  miss_var_summary()

library(overviewR)
overview_tab(dat = penguins, id = species, time = year)
overview_plot(dat = penguins, id = species, time = year)
overview_na(penguins)

library(gtsummary)
tbl_summary(penguins)

library(skimr)
skim(penguins)

library("SmartEDA")
ExpData(data=penguins,type=1)

library(palmerpenguins)
library(summarytools)
view(dfSummary(penguins))
