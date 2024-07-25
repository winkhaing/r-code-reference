# install and load these packages
library(performance)
library(see)

# default regression plots
model <- lm(mpg ~ wt * cyl + gear, data = mtcars)
plot(model)

# plot as a 2x2 matrix
par(mfrow = c(2, 2)) 
plot(model)
par(mfrow = c(1, 1))


# using the nice performance plots (zoom out if you get an error about plot size)
check_model(model)

# performance package also has functions to help check assumptions
check_normality(model)
?check_normality

check_heteroskedasticity(model)
?check_heteroskedasticity
