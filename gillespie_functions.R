#*****************************  Simulation Functions  *********************************#


# These are functions used to simulate either SI, SIS or SIR epidemic
# call using source("DataSimulation.R")
# Please clean all of these!!


#**************************************************************************************#


### SIS Simulation (Gillespie) ~ 


# Function for all rates
rates <- function(B, Sig, N, I){
  inf_rate <- B * I - (B * I^2) / N
  rec_rate <- Sig * I ## check this
  tot_rate <- rec_rate + inf_rate
  ratio_rate <- rec_rate / tot_rate
  
  # Return in easy to access vector
  rates <- c("Infect" = inf_rate, "Recov" = rec_rate,
             "Total" = tot_rate, "Ratio" = ratio_rate)
  return(rates)
}

# Gillespie code
gillespie <- function(c, # current event counts
                      t_origin, # start time
                      t_end, # end time (days)
                      n_steps, # max steps
                      B, # Beta
                      Sig, # Sigma
                      N, # Pop Size
                      I_start,
                      alpha){ # Current state
  
  samps <- matrix(NA, ncol = 4, nrow = n_steps)
  colnames(samps) <- c("Counter", "time", "I", "Transition")
  samps[1, ] <- c(c, 0, I_start, 0)
  
  # Initialize time and iteration
  i <- 1
  t <- 0
  
  while(i < n_steps - 1 && t < t_end){ 
    # Current state and counter
    I <- samps[i, 3]
    c <- samps[i, 1]
    
    # In case I falls below 1
    if(I < 1){I <- 1} 
    rate_iter <- rates(B, Sig, N, I)
    
    # Execute transitions
    if(runif(1) > rate_iter[4]){
      samps[i + 1, 1] <- c + 1
      samps[i + 1, 3] <- I + 1
      samps[i + 1, 4] <- 1
    } else{
      samps[i + 1, 1] <- c
      samps[i + 1, 3] <- I - 1
      samps[i + 1, 4] <- 0
      
    }
    
    # Add interevent time 
    t <- samps[i, 2]
    tau <- rgamma(1, shape = alpha, rate = alpha * rate_iter[3])
    samps[i + 1, 2] <- t + tau
    
    # Set counters
    i <- i + 1
  }
  
  # Format simulations
  samps <- as.data.frame(samps[complete.cases(samps), ])
  samps$Date <- as.Date(samps$time, origin = as.Date(t_origin))
  samps$S <- N - samps$I
  
  # To use in Metropolis
  samps$y <- inter_event(samps$time)
  samps$x1 <- samps$I
  samps$x2 <- (samps$I)^2
  return(samps[-1, ]) # Remove first row (interevent non existent)
}


#**************************************************************************************#


### Gillespie for SI Epidemic ~ 


# Gillespie code
gillespie.SI <- function(t_origin, # start date
                         B, # Beta
                         N, # Pop Size
                         I_start,
                         alpha) { # Current state   
  
  n_steps <- N - I_start # Until everyone gets infected
  samps <- matrix(NA, ncol = 2, nrow = n_steps)
  colnames(samps) <- c("time", "I")
  samps[1, ] <- c(0, I_start)
  
  # Initialize time and iteration
  for(i in 1:(n_steps - 1)) {
    I <- samps[i, 2]
    inf_rate <- B * I - (B * I^2) / N
    samps[i + 1, 2] <- I + 1
    
    # Compute time to next event 
    t <- samps[i, 1]
    tau <- rgamma(1, shape = alpha, rate = alpha * inf_rate)
    
    # Update time and no. infected
    samps[i + 1, 1] <- t + tau
    samps[i + 1, 2] <- I + 1
  }
  
  # Format simulations
  samps <- as.data.frame(samps)
  samps$Date <- as.Date(samps$time, origin = as.Date(t_origin))
  samps$S <- N - samps$I
  
  # To use in Metropolis
  samps$y <- inter_event(samps$time)
  samps$x1 <- samps$I
  samps$x2 <- (samps$I)^2
  return(samps[-1, ]) # Remove first row (interevent non existent)
}


#**************************************************************************************#


### SIR Gillespie ~ 


rates.SIR <- function(B, Sig, N, S, I) {
  inf_rate <- B * S * I / N
  rec_rate <- Sig * I ## check this
  tot_rate <- rec_rate + inf_rate
  ratio_rate <- rec_rate / tot_rate
  
  # Return in easy to access vector
  rates <- c("Infect" = inf_rate, "Recov" = rec_rate,
             "Total" = tot_rate, "Ratio" = ratio_rate)
  return(rates)
}


gillespie.SIR <- function(t_origin, # start date
                          t_end, # end time (days after t0)
                          B, # Beta
                          Sig, # Sigma
                          N,
                          S_start,
                          I_start,
                          alpha) { # Current state
  
  n_steps <- 10000 # max steps to take
  samps <- matrix(NA, ncol = 4, nrow = n_steps + 1)
  colnames(samps) <- c("Transition", "time", "S",  "I")
  samps[1, ] <- c(0, 0, S_start, I_start)
  
  # Initialize time and iteration
  i <- 1
  t <- 0
  
  while(i < n_steps - 1 && t < t_end) { 
    # Current state 
    S <- samps[i, 3]
    I <- samps[i, 4]

    # In case I falls below 1
    if(I == 0){break} 
    rate_iter <- rates.SIR(B, Sig, N, S, I)
    
    # Execute transitions
    if(runif(1) > rate_iter[4]) {
      samps[i + 1, 3] <- S - 1
      samps[i + 1, 4] <- I + 1
      samps[i + 1, 1] <- 1
    } else {
      samps[i + 1, 3] <- S 
      samps[i + 1, 4] <- I - 1
      samps[i + 1, 1] <- 0
    }
    
    # Add interevent time 
    t <- samps[i, 2]
    tau <- rgamma(1, shape = alpha, rate = alpha * rate_iter[3])
    samps[i + 1, 2] <- t + tau
    
    # Set counters
    i <- i + 1
  }
  
  # Format simulations
  samps <- as.data.frame(samps[complete.cases(samps), ])
  samps$Date <- as.Date(samps$time, origin = as.Date(t_origin))

  # To use in Metropolis
  samps$R <- N - samps$S - samps$I
  samps$y <- inter_event(samps$time)
  samps$x1 <- samps$I
  samps$x2 <- (samps$I)^2
  return(samps[-1, ]) # Remove first row (interevent non existent)
}


#**************************************  End  *****************************************#


# 3) Computes interevent times in days.
# - time_var is of the form returned from time_since function
inter_event <- function(time_var){
  times <- (c(time_var, 0) - c(time_var[1], time_var))
  times <- ifelse(times == 0, 0.5, times)
  return(times[-length(times)])
}

