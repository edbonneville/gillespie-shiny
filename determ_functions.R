#########################################
## Calculating deterministic evolution ##
## of epidemic i.e. solving ODE's      ##
#########################################


# For SI-type epidemics

determ_func_SI <- function(initial_values,
                           timepoints, 
                           parameter_list) {
  
  
  si_model <- function(current_timepoint, 
                       state_values, 
                       parameters) {
    
    S <- state_values[1]        
    I <- state_values[2]        
    
    with(as.list(parameters), {
      
      dS <- -beta * S * I / N
      dI <- beta * S * I / N
      
      results <- c(dS, dI)
      list(results)
    }
    )
  }
  
  output <- lsoda(initial_values, 
                  timepoints, 
                  si_model, 
                  parameter_list)
  
  output <- as.data.frame(output) %>%
    select(t = time, S_det = S, I_det = I)
  return(output)
}


# For SIS-type
# Source: http://rpubs.com/docblount/111138

determ_func_SIS <- function(initial_values, 
                            timepoints, 
                            parameter_list) {
  
  sis_model <- function(current_timepoint, 
                        state_values, 
                        parameters) {
    
    # Define states
    S <- state_values[1] # susceptible 
    I <- state_values[2] # infectious
    
    with(as.list(parameters), {
      
      # Get derivatives
      dS <- -beta * S * I / N + gamma * I
      dI <- beta * S * I / N - gamma * I
      
      results <- c(dS, dI)
      list(results)
    })
  }
  
  # Format output
  output <- lsoda(initial_values, 
                  timepoints, 
                  sis_model, 
                  parameter_list)
  
  output <- as.data.frame(output) %>%
    select(t = time, S_det = S, I_det = I)
}


# For SIR-type 

determ_func_SIR <- function(initial_values, 
                            timepoints, 
                            parameter_list) {
  
  sir_model <- function(current_timepoint, 
                        state_values, 
                        parameters) {
    
    S <- state_values[1]        
    I <- state_values[2]        
    
    with(as.list(parameters), {
      
      # compute derivatives
      dS <- -beta * S * I / N
      dI <- beta * S * I / N - gamma * I
      dR <- gamma * I
      
      # combine results
      results <- c(dS, dI, dR)
      list(results)
    }
    )
  }
  
  output <- lsoda(initial_values, 
                  timepoints, 
                  sir_model, 
                  parameter_list)
  
  output <- as.data.frame(output) %>%
    select(t = time, S_det = S, I_det = I, R_det = R)
  return(output)
}
