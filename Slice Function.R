library(tidyverse)

# create some dummy data
data<-data.frame(person=rep(c("John","Sue", "Bob"),c(3,2,1)), 
                 metric=c(10,20,30,15,25,5))
data

# subset by max metric for each person
data %>% group_by(person) %>% slice_max(metric)

# subset by min metric for each person
data %>% group_by(person) %>% slice_min(metric)

# random sample 4 rows
data %>% slice_sample(n=4)

# first 2 rows
data %>% slice_head(n=2)

# first 2 from each group
data %>% group_by(person) %>% slice_head(n=2)
