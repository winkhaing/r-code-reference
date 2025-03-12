install.packages("sjPlot")

library(devtools)
devtools::install_github("strengejacke/sjPlot")
# install.packages("remotes")
remotes::install_github("resplab/predtools")
library(predtools)
library(magrittr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
data(dev_data)
data(val_data)
reg <- glm(y~sex+age+severity+comorbidity,data=dev_data,family=binomial(link="logit"))
summary(reg)
dev_data$pred <- predict.glm(reg, type = 'response')
val_data$pred <- predict.glm(reg, newdata = val_data, type = 'response')

calibration_plot(data = dev_data, obs = "y", pred = "pred", title = "Calibration plot for development data", y_lim = c(0, 0.7), x_lim=c(0, 0.7))
calibration_plot(data = val_data, obs = "y", pred = "pred", y_lim = c(0, 1), x_lim=c(0, 1),
                 title = "Calibration plot for validation data", group = "sex")
