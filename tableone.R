pacman::p_load(tableone)
## tableone package itself
library(tableone)
## survival package for Mayo Clinic's PBC data
library(survival)
data(pbc)
# drop ID from variable list
vars <- names(pbc)[-1]
## Create Table 1 stratified by trt (can add more stratifying variables)
tableOne <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc,
                           factorVars = c("status","edema","stage"))
## Specifying nonnormal variables will show the variables appropriately,
## and show nonparametric test p-values. Specify variables in the exact
## argument to obtain the exact test p-values.
print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
      exact = c("status","stage"), smd = TRUE,
      formatOptions = list(big.mark = ","))


