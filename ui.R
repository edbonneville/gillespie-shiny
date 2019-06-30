# Load necessary packages 
library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)


# Show dashboard
dashboardPage(
  dashboardHeader(title = "Gillespie App"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput('B1', "Beta 1", value = 5, 
                  min = 0, max = 15, step = 0.01),
      sliderInput('gamma', "Recovery Rate", value = 2, 
                  min = 0, max = 15, step = 0.01),
      numericInput('N', "N", value = 200, min = 0, max = 10000)
    )
  ),
  dashboardBody(
    box(title = "Plot", plotlyOutput("plot_SIR"), 
        height = 8, width = 16)
  )
)