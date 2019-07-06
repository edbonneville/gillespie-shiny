#########################
## Gillespie functions ##
#########################


# 3) Computes interevent times in days.
# - time_var is of the form returned from time_since function
inter_event <- function(time_var){
  times <- (c(time_var, 0) - c(time_var[1], time_var))
  return(times[-length(times)])
}


rate_fun <- function(curr_state, pars, type) {
  
  # Set up
  N <- sum(curr_state[-length(curr_state)])
  curr_state <- as.list(curr_state)
  pars <- as.list(pars)
  
  # Compute rates, depending on epidemic type
  if (type == "SI") {
    
    with(c(pars, curr_state), c(infection = beta * S * I / N))
    
  } else if (type == "SIS" | type == "SIR") {
    
    with(c(pars, curr_state), c(infection = beta * S * I / N,
                                recovery = gamma * I))
    
  } else if (type == "SIR_dem") {
    
    with(c(pars, curr_state), c(birth = mu * N,
                                infection = beta * S * I / N,
                                recovery = gamma * I,
                                death_S = mu * S,
                                death_I = mu * I,
                                death_R = mu * R))
  }
}
                      

gillespie <- function(pars, # named vector, c("beta", "gamma", "mu")
                      state_0, # named vector, c("S", "I", "R", "t")
                      trans_mat, # transition matrix, see example
                      t_end, # until when? in days
                      type, # "SI", "SIS", "SIR" or "SIR_dem"
                      alpha) { # shape parameter of gamma interevent dist
  
  # Max steps to take
  n_steps <- 10000 
  
  # Matrix storing states 
  samps <- matrix(NA, ncol = length(state_0), nrow = n_steps + 1,
                  dimnames = list(NULL, names(state_0)))
  
  # Make matrix to store rates 
  rates <- matrix(NA, ncol = nrow(trans_mat), nrow = n_steps + 1,
                  dimnames = list(NULL, rownames(trans_mat)))
  
  # Initialize time, iteration and state
  i <- 1
  t <- state_0["t"]
  samps[1, ] <- state_0
  
  # Loop until t_end or max steps
  while(i < n_steps - 1 && t < t_end) { 
    
    # Current state 
    state <- samps[i, ]
    
    # Compute and store rates
    rate_iter <- rates[i, ] <- rate_fun(state, pars, type = type)
    tot_rate <- sum(rate_iter)
    
    # Check rates are all non-negative
    if (all(rate_iter == 0)) break
    
    # Draw time to next event
    t <- state["t"]
    tau <- rgamma(1, shape = alpha, rate = alpha * tot_rate)
    
    # Determine which event occured
    which_event <- sample(1:nrow(trans_mat), 
                          size = 1, 
                          prob = rate_iter / tot_rate)
    
    # Update the state, count and transition vector
    samps[i + 1, ] <- state + c(trans_mat[which_event, ], tau)
    i <- i + 1
  }
  
  # Format simulations
  samps <- cbind(samps, rates)
  samps <- as.data.frame(samps[complete.cases(samps), ])
  return(samps)
}

# Set up transition matrices here

# SI
trans_mat_SI <- matrix(c(-1, 1), 
                       byrow = T, ncol = 2, 
                       dimnames = list(c("infection"), 
                                       c("S", "I")))

# SIS
trans_mat_SIS <- matrix(c(-1, 1,
                          1, -1), 
                        byrow = T, ncol = 2, 
                        dimnames = list(c("infection", "recovery"), 
                                        c("S", "I")))

# SIR
trans_mat_SIR <- matrix(c(-1, 1, 0,
                          0, -1, 1), 
                        byrow = T, ncol = 3, 
                        dimnames = list(c("infection", "recovery"), 
                                        c("S", "I", "R")))

# SIR_dem
trans_mat_SIRdem <- matrix(c(1, 0, 0,
                             -1, 1, 0,
                             0, -1, 1,
                             -1, 0, 0,
                             0, -1, 0,
                             0, 0, -1), byrow = T, ncol = 3, 
                           dimnames = list(c("birth", "infection", "recovery",
                                             "death_S", "death_I", "death_R"), 
                                           c("S", "I", "R")))
