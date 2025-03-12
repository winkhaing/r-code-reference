N_psa <- 1000

params <- data.frame(
  p_Healthy_Diseased = rbeta(N_psa, 3, 97),
  p_Healthy_Dead     = rbeta(N_psa, 1, 99),
  p_Diseased_Dead    = rbeta(N_psa, 5, 95),
  
  c_Healthy  = rgamma(N_psa, shape = 25, scale = 50 / 25),
  c_Diseased = rgamma(N_psa, shape = 25, scale = 1000 / 25),
  q_Healthy  = rbeta(N_psa, 90, 10),
  q_Diseased = rbeta(N_psa, 60, 40)
)

model <- function(.params) {
  with(.params, {
    n_t <- 40
    n_s <- 3
    n_c <- 1000
    
    v_state_names <- c("Healthy", "Diseased", "Dead")
    
    m_P <- matrix(
      0,
      nrow = 3,
      ncol = 3,
      dimnames = list(from = v_state_names,
                      to = v_state_names)
    )
    
    m_P["Healthy",   "Healthy"] <-
      1 - p_Healthy_Diseased - p_Healthy_Dead
    m_P["Healthy",  "Diseased"] <- p_Healthy_Diseased
    m_P["Healthy",      "Dead"] <- p_Healthy_Dead
    m_P["Diseased", "Diseased"] <- 1 - p_Diseased_Dead
    m_P["Diseased",     "Dead"] <- p_Diseased_Dead
    m_P["Dead",         "Dead"] <- 1
    
    state_membership <- array(
      NA_real_,
      dim = c(n_t, n_s),
      dimnames = list(cycle = 1:n_t,
                      state = v_state_names)
    )
    
    state_membership[1,] <- c(n_c, 0, 0)
    
    for (i in 2:n_t) {
      state_membership[i,] <- state_membership[i - 1,] %*% m_P
    }
    
    m_payoffs <- matrix(
      0,
      nrow = 3,
      ncol = 2,
      dimnames = list(state = v_state_names,
                      payoff = c("Cost", "QALY"))
    )
    
    m_payoffs["Healthy",  "Cost"] <- c_Healthy
    m_payoffs["Healthy",  "QALY"] <- q_Healthy
    m_payoffs["Diseased", "Cost"] <- c_Diseased
    m_payoffs["Diseased", "QALY"] <- q_Diseased
    
    payoff_trace <- state_membership %*% m_payoffs
    
    colSums(payoff_trace) / n_c
    
  })
}

psa_results <- t(sapply(
  X = split(params, 1:N_psa),
  FUN = model,
  simplify = TRUE
))

plot(psa_results[, 2], psa_results[, 1], type = 'p', xlab = "QALY", ylab = "Cost")

