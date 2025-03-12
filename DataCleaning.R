library("rio")

# Reading data into R
url <- "https://github.com/lamethods/data/raw/main/1_moodleLAcourse/"
events <- import(paste0(url, "Events.xlsx"), setclass = "tibble")
results <- import(paste0(url, "Results.xlsx"), setclass = "tibble")
demographics <- import(paste0(url, "Demographics.xlsx"), setclass = "tibble")
library("dplyr")

# Grouping and summarizing data
demographics |>
  group_by(Gender) |> 
  count(Location)
events_summary <- events |>
  group_by(user) |> 
  tally() |>
  rename(Frequency.Total = n)
events_summary
events |>
  group_by(user, Action) |>
  count(Action)

# Selecting variables
demographics |> 
  select(Employment)
demographics |> 
  pull(Employment) |>
  head()
demographics |> 
  select(user:Origin)
demographics |> 
  select(!Gender)
demographics |> 
  select(c(user, Surname))
cols_a <- c("user", "Name", "Surname")
cols_b <- c("Surname", "Origin")
demographics |> 
  select(all_of(cols_a))
demographics |> 
  select(all_of(cols_a) & all_of(cols_b))
demographics |> 
  select(all_of(cols_a) | all_of(cols_b))
results |> 
  select(starts_with("Grade"))
results |> 
  select(contains("Data"))
results |> 
  select(where(is.character))
results |> 
  select(where(is.double))

# Filtering observations
demographics |> 
  filter(Origin == "Bosnia") |> 
  select(Name, Surname)
demographics |>
  filter(Gender == "F" & Location == "Remote")
demographics |>
  filter(Gender == "F") |> 
  filter(Location == "Remote")
results |> 
  filter(Final_grade > 8)
events_summary |>
  filter(Frequency.Total > 100 & Frequency.Total < 500)

#Transforming variables
demographics |> 
  mutate(Location = factor(Location))
demographics |> 
  mutate(
    Gender = factor(Gender),
    Location = factor(Location),
    Employment = factor(Employment)
  )
demographics |> 
  mutate(across(c(Gender, Location, Employment), factor))

## working with date
library("lubridate")
demographics |> 
  mutate(
    Birthdate = as.Date(Birthdate, format = "%d.%m.%Y"),
    Age = year(as.period(interval(start = Birthdate, end = date("2023-03-12")))),
    FullName = paste0(Surname, ", ", Name) 
  ) |> 
  select(Age, FullName)

results <- results |>
  mutate(
    AchievingGroup = factor(
      case_when(
        ntile(Final_grade, 2) == 1 ~ "Low achiever",
        ntile(Final_grade, 2) == 2 ~ "High achiever"
      )
    )
  )
events_summary <- events_summary |> 
  mutate(
    ActivityGroup = factor(
      case_when(
        ntile(Frequency.Total, 3) == 1 ~ "Low activity",
        ntile(Frequency.Total, 3) == 2 ~ "Moderate activity",
        ntile(Frequency.Total, 3) == 3 ~ "High activity"
      )
    )
  )

# Rearranging data

demographics |> 
  arrange(Surname, Name)
demographics |>
  arrange(desc(Surname), desc(Name))
demographics |>
  relocate(user, .after = Employment)

# Reshaping data
library("tidyr")
events_types <- events |>
  group_by(user, Action) |>
  count(Action) |> 
  pivot_wider(
    names_from = "Action", 
    names_prefix = "Frequency.",
    values_from = "n",
    values_fill = 0
  )
events_types

# Joining data

left_join(demographics, events_summary, by = "user")
all_combined <- demographics |> 
  left_join(events_types, by = "user") |>
  left_join(events_summary, by = "user") |> 
  left_join(results, by = "user")
all_combined


# Missing data

library("mice")
set.seed(44)
events_types <- events_types |>
  rename(
    "Ethics" = "Frequency.Ethics",
    "Social" = "Frequency.Social",
    "Practicals" = "Frequency.Practicals"
  )
ampute_list <- events_types |>
  ungroup(user) |>
  select(Ethics:Practicals)|>
  as.data.frame() |>
  ampute(prop = 0.3)
events_types_mis <- ampute_list$amp |>
  as_tibble()
events_types_mis[2, "Practicals"] <- NA
md.pattern(events_types_mis, rotate.names = TRUE)
events_types_mis |>
  drop_na()
imp <- mice(events_types_mis, method = "mean", m = 1, maxit = 1 , print = FALSE)
complete(imp) |> 
  head()
fit <- with(imp, lm(Ethics ~ 1))
summary(fit)
imp2 <- mice(events_types_mis, method = "pmm", m = 10, maxit = 100, print = FALSE)
fit2 <- with(imp2, lm(Ethics ~ Practicals))
pool_fit <- pool(fit2)
## Multiple imputation
summary(pool_fit)
## Complete cases
summary(lm(Ethics ~ Practicals, events_types_mis))["coefficients"]
## Without missingness
summary(lm(Ethics ~ Practicals, events_types))["coefficients"] 

#Correcting erroneous data
library("stringr")
demographics |>
  filter(str_detect(string = Surname, pattern = "sen$")) |>
  pull(Surname)
demographics |>
  mutate(Surname = str_replace(
    string = Surname, pattern = "sen$", replacement = "ssen")
  ) |>
  filter(str_detect(string = Surname, pattern = "sen$")) |>
  pull(Surname)

demographics <- demographics |>
  mutate(Surname = str_replace(
    string = Surname, pattern = "sen$", replacement = "ssen")
  )