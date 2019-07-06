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
      
      dat_sim <- gillespie(c("beta" = input$beta),
                           c("S" = input$N - 1, "I" = 1, "t" = 0),
                           trans_mat_SI,
                           t_end = input$t_end, 
                           type = "SI", 
                           alpha = input$alpha) %>% 
        gather(class, value, S, I) %>%
        gather(event, rate, infection) %>%
        mutate(class = factor(class, 
                              levels = c("S", "I")))
      
    } else if (input$type == "SIS") {
      
      dat_sim <- gillespie(c("beta" = input$beta,
                             "gamma" = input$gamma),
                           c("S" = input$N - 1, "I" = 1, "t" = 0),
                           trans_mat_SIS,
                           t_end = input$t_end, 
                           type = "SIS", 
                           alpha = input$alpha) %>% 
        gather(class, value, S, I) %>%
        gather(event, rate, infection:recovery) %>%
        mutate(class = factor(class, 
                              levels = c("S", "I")))
      
    } else {

      dat_sim <- gillespie(c("beta" = input$beta,
                             "gamma" = input$gamma),
                           c("S" = input$N - 1, "I" = 1, "R" = 0, "t" = 0),
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
      
      p <- ggplot(data = test(), aes(t, value, color = class)) + 
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
    output$plot_rates <- renderPlotly({
      
      p <- test() %>%
        ggplot(aes(t, rate, col = event)) + 
        geom_line(size = 1.5) +
        xlab("Time since first infection (days)") + 
        ylab("Event rate") +
        theme_minimal(base_size = 14) + 
        theme(legend.title = element_blank())
      
      p <- ggplotly(p) #%>% 
      
      # Make bg transparent
      #layout(plot_bgcolor="rgba(0, 0, 0, 0)",
      #         paper_bgcolor="rgba(0, 0, 0, 0)") 
      
      p
    }) 
    
})


