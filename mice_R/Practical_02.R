library(mice)

nhanes

#complet case-analysis
model <- lm(chl ~ bmi + age, data = nhanes)
summary(model)

md.pattern(nhanes)

imp <- mice(nhanes, print = FALSE)
imp$predictorMatrix

imp <- mice(nhanes, print = FALSE, maxit = 10, seed = 24415) #10 iterations
plot(imp) #inspect the trace lines for convergence

fit <- with(imp, lm(chl ~ bmi + age))
coef(fit$analyses[[1]])

coef(fit$analyses[[2]])

est <- pool(fit)
summary(est)

summary(model)
