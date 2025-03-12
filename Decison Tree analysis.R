model <- function(p) {
  
  eff_not_resolved <- p$p_2nd_line_success * p$eff_2nd_line_success +
    (1 - p$p_2nd_line_success) * p$eff_2nd_line_failure
  
  res <- list(
    
    cost.short_course = p$c_short_course +
      (1 - p$p_short_course_success) * p$c_2nd_line,
    
    cost.full_course = p$c_full_course +
      (1 - p$p_full_course_success) * p$c_2nd_line,
    
    qaly.short_course = p$p_short_course_success * p$eff_resolved +
      (1 - p$p_short_course_success) * eff_not_resolved,
    
    qaly.full_course = p$p_full_course_success * p$eff_resolved +
      (1 - p$p_full_course_success) * eff_not_resolved
    
  )
  
  if (is.data.frame(p)) as.data.frame(res) else res
  
}

# Single set of parameters as list
single_params <- list(
  
  # Probabilities
  p_short_course_success = 0.6,
  p_full_course_success  = 0.8,
  p_2nd_line_success     = 0.9,
  
  # Costs
  c_short_course         = 250,
  c_full_course          = 500,
  c_2nd_line             = 1000,
  
  # Effects
  eff_resolved           = 12.0,
  eff_2nd_line_success   = 11.8,
  eff_2nd_line_failure   = 10.2
  
)

single_res <- model(single_params)

print(single_res)

single_icer <- (single_res$cost.full_course - single_res$cost.short_course) /
  (single_res$qaly.full_course - single_res$qaly.short_course)

print(single_icer)

# Multiple parameter sets in data.frame

N_psa <- 100

multiple_params <- data.frame(
  
  # Probabilities
  p_short_course_success = rbeta(N_psa, 60, 40),
  p_full_course_success  = rbeta(N_psa, 80, 20),
  p_2nd_line_success     = rbeta(N_psa, 90, 10),
  
  # Costs
  c_short_course         = rgamma(N_psa, shape = 100, scale = 2.5),
  c_full_course          = rgamma(N_psa, shape = 100, scale = 5.0),
  c_2nd_line             = rgamma(N_psa, shape = 100, scale = 10.0),
  
  # Effects
  eff_resolved           = rgamma(N_psa, shape = 25, scale = 0.480),
  eff_2nd_line_success   = rgamma(N_psa, shape = 25, scale = 0.472),
  eff_2nd_line_failure   = rgamma(N_psa, shape = 25, scale = 0.408)
  
)

multiple_res <- model(multiple_params)

summary(multiple_res)

plot(
  x = multiple_res$qaly.full_course - multiple_res$qaly.short_course,
  y = multiple_res$cost.full_course - multiple_res$cost.short_course,
  xlab = "Incremental QALYs",
  ylab = "Incremental costs",
  type = 'p'
)
