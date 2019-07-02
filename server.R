# Load necessary packages 
library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)

# Load support functions
source("gillespie_functions.R")


# for testing
input <- list("beta" = 0.1, "gamma" = 0.01, "N" = 150)

shinyServer(function(input, output) {
  
  # set.seed(1984)
  
  # Run everything upon pressing "Run"
  test <- eventReactive(input$run, {
    # Generate from correct model 
    if (input$type == "SI") {
      dat_sim <- gillespie.SI(t_origin = Sys.Date(), 
                              B = input$beta, 
                              N = input$N, 
                              I_start = 1, 
                              alpha = 1) %>%
        gather(class, value, c(2, 4)) %>%
        mutate(class = factor(class, levels = c("S", "I")))
      
      
    } else if (input$type == "SIS") {
      dat_sim <- gillespie(c = 1,
                           t_origin = Sys.Date(), 
                           t_end = 500, 
                           n_steps = 500,
                           B = input$beta, 
                           Sig = input$gamma, 
                           N = input$N, 
                           I_start = 1, 
                           alpha = 1)  %>%
        gather(class, value, c(3, 6)) %>%
        mutate(class = factor(class, levels = c("S", "I")))
      
    } else {
      dat_sim <- gillespie.SIR(t_origin = Sys.Date(), 
                               t_end = 1000, 
                               B = input$beta, 
                               Sig = input$gamma, 
                               N = input$N, 
                               S_start = input$N - 1, 
                               I_start = 1, 
                               alpha = 1) %>% 
        gather(class, value, c(3, 4, 6)) %>%
        mutate(class = factor(class, levels = c("S", "I", "R")))
    }
  })
  
    # Make plot
    output$plot_descrip <- renderPlotly({
      
      p <- ggplot(data = test(), aes(time, value, color = class)) + 
        geom_point(size = 1.5) +
        xlab("Time since first infection (days)") + 
        ylab("Number in class") +
        theme_minimal(base_size = 14) + 
        theme(legend.title = element_blank())
      
      p <- ggplotly(p) #%>% 
      
      # Make bg transparent
      #layout(plot_bgcolor="rgba(0, 0, 0, 0)",
      #         paper_bgcolor="rgba(0, 0, 0, 0)") 
      
      p
    }) 
    
    # Make plot
    output$plot_tau <- renderPlotly({
      
      p <- test() %>%
        filter(class == "I") %>%
        mutate(y = inter_event(time)) %>%
        ggplot(aes(x1, y)) + 
        geom_point(size = 1.5) +
        xlab("Number infectious") + 
        ylab("Infection interevent time") +
        theme_minimal(base_size = 14) + 
        theme(legend.title = element_blank())
      
      p <- ggplotly(p) #%>% 
      
      # Make bg transparent
      #layout(plot_bgcolor="rgba(0, 0, 0, 0)",
      #         paper_bgcolor="rgba(0, 0, 0, 0)") 
      
      p
    }) 
    
})


