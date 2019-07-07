#######################
## server Shiny file ##
#######################


# Load necessary packages 
library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)
library(deSolve)
library(shinyWidgets)


# Load support functions
source("gillespie_functions.R")
source("determ_functions.R")


# Make ggplot2 theme
theme_set(theme_minimal(base_size = 14))


# Start server function
shinyServer(function(input, output, session) {
  
  # set.seed(1984)
  
  # Run everything upon pressing "Run"
  sims <- eventReactive(input$run, {
    
    # Check N = S + I condition
    N_check <- reactive({
      validate(
        need(input$N > input$S && input$N == input$S + input$I, 
             "N must be equal to S + I. Neither S or I can be greater than N.
             Please adjust your values!")
      )
    })
    
    N_check()
    
    # Generate from selected model 
    if (input$type == "SI") {
      
      dat_sim <- gillespie(c("beta" = input$beta / 365), # convert to days
                           c("S" = input$S, "I" = input$I, "t" = 0),
                           trans_mat_SI,
                           t_end = input$t_end, 
                           type = "SI", 
                           alpha = input$alpha) %>% 
        gather(class, value, S, I) %>%
        gather(event, rate, infection) %>%
        mutate(class = factor(class, 
                              levels = c("S", "I")))
      
    } else if (input$type == "SIS") {
      
      dat_sim <- gillespie(c("beta" = input$beta / 365, # convert to days
                             "gamma" = (input$gamma)^-1), # invert infectious period
                           c("S" = input$S, "I" = input$I, "t" = 0),
                           trans_mat_SIS,
                           t_end = input$t_end, 
                           type = "SIS", 
                           alpha = input$alpha) %>% 
        gather(class, value, S, I) %>%
        gather(event, rate, infection:recovery) %>%
        mutate(class = factor(class, 
                              levels = c("S", "I")))
      
    } else if (input$type == "SIR") {

      dat_sim <- gillespie(c("beta" = input$beta / 365,
                             "gamma" = (input$gamma)^-1),
                           c("S" = input$S, "I" = input$I, "R" = 0, "t" = 0),
                           trans_mat_SIR,
                           t_end = input$t_end, 
                           type = "SIR", 
                           alpha = input$alpha) %>% 
        gather(class, value, S, I, R) %>%
        gather(event, rate, infection:recovery) %>%
        mutate(class = factor(class, 
                              levels = c("S", "I", "R")))

      }
  })
  

    # Make plot
    output$plot_descrip <- renderPlotly({

      # If user wants to overlay
      if (input$overlay_determ) {
        
        p <- sims() %>% 
          ggplot(aes(t, value, color = class)) + 
          geom_point(size = 1.5) +
          geom_line(aes(t, I_det), linetype = "dashed", colour = "black") +
          geom_line(aes(t, S_det), linetype = "dashed", colour = "black") +
          xlab("Time since first infection (days)") + 
          ylab("Number in class") 
        
        # Add R if SIR
        if (input$type == "SIR") {
          p <- p + geom_line(aes(t, R_det), linetype = "dashed", colour = "black")
        } 
          # Generate plot
          p <- ggplotly(p)
        
      } else {
        p <- sims() %>% 
          ggplot(aes(t, value, color = class)) + 
          geom_point(size = 1.5) +
          xlab("Time since first infection (days)") + 
          ylab("Number in class") 
        
        p <- ggplotly(p)
      }
      
    }) 
    
    # Render only deterministic plots
    output$plot_determ <- renderPlotly({
      
      p <- sims() %>% 
        gather(event_det, rate_det, contains("det")) %>% 
        mutate(event_det = factor(event_det, 
                                  levels = c("S_det", "I_det", "R_det"), 
                                  labels = c("S", "I", "R"))) %>% 
        ggplot(aes(t, rate_det, col = event_det)) +
        geom_line(size = 1.5) +
        xlab("Time since first infection (days)") + 
        ylab("Number in class") 
        
        p <- ggplotly(p)
    })
    
    # Make plot of rates
    
    output$plot_rates <- renderPlotly({
      
      p <- sims() %>%
        ggplot(aes(t, rate, col = event)) + 
        geom_line(size = 1.5) +
        xlab("Time since first infection (days)") + 
        ylab("Event rate") +
        theme_minimal(base_size = 14) + 
        theme(legend.title = element_blank())
      
      p <- ggplotly(p)
    }) 
    
    output$interev_vs_I <- renderPlotly({
      
      p <- sims() %>% 
        filter(event =="infection" & class == "I") %>% 
        mutate(interev = inter_event(t)) %>%
        slice(-1) %>% # remove first zero
        ggplot(aes(value, interev)) + geom_point() +
        xlab("Number infectious") +
        ylab("Infectious interevent time")
              
        #geom_histogram(aes(y=..density..), 
        #               bins = 50,
        #               col = "black",
        #               fill = "grey", 
        #               alpha = 0.75) 
      p <- ggplotly(p)
    }) 
    
    output$interev_dist <- renderPlotly({
      
      p <- sims() %>% 
        filter(event =="infection" & class == "I") %>% 
        mutate(interev = inter_event(t)) %>%
        slice(-1) %>% # remove first zero
        ggplot(aes(interev)) + 
        geom_histogram(aes(y=..density..), 
                       bins = 50,
                       col = "black",
                       fill = "grey", 
                       alpha = 0.75) +
        xlab("Infection interevent time") +
        ylab("Density")

      p <- ggplotly(p)
    }) 
})

