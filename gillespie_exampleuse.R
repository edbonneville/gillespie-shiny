###############################
## Showing Gillespie use for ##
## different epidemic models ##
###############################


## For SI

pars <- c("beta" = 0.1) 
state_0 <- c("S" = 199, "I" = 1, "t" = 0)

gillespie(pars,
          state_0,
          trans_mat_SI,
          t_end = 500, 
          type = "SI", 
          alpha = 1) %>% 
  gather(class, value, S, I) %>%
  mutate(class = factor(class, 
                        levels = c("S", "I"))) %>% 
  ggplot(aes(t, value, col = class)) +
  geom_point(size = 1.5)


## For SIS

pars <- c("beta" = 0.1, "gamma" = 1 / 200) 
state_0 <- c("S" = 199, "I" = 1, "t" = 0)

gillespie(pars,
          state_0,
          trans_mat_SIS,
          t_end = 300, 
          type = "SIS", 
          alpha = 1) %>% 
  gather(class, value, S, I) %>%
  ggplot(aes(t, value, col = class)) +
  geom_point(size = 1.5)


# For SIR

pars <- c("beta" = 0.1, "gamma" = 1/50) 

state_0 <- c("S" = 200, "I" = 1, "R" = 0, "t" = 0)

gillespie(pars,
          state_0,
          trans_mat_SIR,
          t_end = 350, 
          type = "SIR", 
          alpha = 1) %>% 
  gather(class, value, S, I, R) %>%
  ggplot(aes(t, value, col = class)) +
  geom_point(size = 1.5)


# For SIR_dem

pars <- c("beta" = 0.1, "gamma" = 0.002, "mu" = 0.001) 
state_0 <- c("S" = 199, "I" = 1, "R" = 0, "t" = 0)

gillespie(pars,
          state_0,
          trans_mat_SIRdem,
          t_end = 500, 
          type = "SIR_dem", 
          alpha = 1) %>% 
  gather(class, value, S, I, R) %>%
  gather(event, rate, birth:death_R) %>%
  mutate(class = factor(class, 
                        levels = c("S", "I", "R"))) %>%
  ggplot(aes(t, value, col = class)) +
  geom_point(size = 1.5)
