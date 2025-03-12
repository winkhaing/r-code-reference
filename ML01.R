rm(list=ls())

# load the pacman library 
if(!require('pacman')) { 
  install.packages('pacman') 
  library('pacman') 
} 
  
# loading the p_load function 
p_load(requests, install = FALSE) 


install.packages("AmesHousing")
install.packages("p_load")
ames <- AmesHousing::make_ames()

AmesHousing::make_ames

# Helper packages
library(dplyr)     # for data manipulation
library(ggplot2)   # for awesome graphics

# Modeling process packages
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training
library(h2o)       # for resampling and model training

# h2o set-up 
h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o
