# Load necessary packages 
library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)

source("gillespie_functions.R")




shinyServer(function(input, output) {
  
  output$plot_SIR <- renderPlotly({
    
    set.seed(1984)
    dat_sim_SIR <- gillespie.SIR(t_origin = "2019-01-01", 
                                 t_end = 1000, 
                                 B = input$B1, 
                                 Sig = input$gamma, 
                                 N = input$N, 
                                 S_start = input$N - 1, 
                                 I_start = 1, 
                                 alpha = 1)
    
    plot1 <- gather(dat_sim_SIR, key, value, c(3, 4, 6)) %>%
      ggplot(., aes(time, value, color = key)) + geom_point(size = 2) +
      scale_colour_manual("Legend",
                          breaks = c("S", "I", "R"),
                          values = c("blue", "red", "green")) +
      theme_minimal(base_size = 16) 
    
    p <- ggplotly(plot1)
    #p <- layout(p, fig_bgcolor="rgb(255, 255, 255)", 
    #            plot_bgcolor="rgba(0, 0, 0, 0)", 
    #            paper_bgcolor="rgba(0, 0, 0, 0)")
    p
  })
  
  
  #output$plot_inftau <- renderPlot({ })
})


