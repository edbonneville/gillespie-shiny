# Load necessary packages 
library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)

# Customise sidebar
sidebar <- dashboardSidebar(
  width = 340,
  
  # Read description of app or run directly
  sidebarMenu(id = "nav",
              menuItem("Information", tabName = "app_descrip"),
              menuItem("Simulation", tabName = "sims")
  ),
  
  # Pick compartmental model
  conditionalPanel(
    condition = 'input.nav == "sims"',
    selectInput("type", "Model:", list(SI = "SI",
                                       SIS = "SIS",
                                       SIR = "SIR")),
    
    # For SI
    conditionalPanel(
      condition = 'input.type == "SI"',
      sliderInput('beta', "Beta", value = 5, 
                  min = 0, max = 25, step = 0.1),
      numericInput('N', "N", value = 200, min = 0, max = 10000)
    ),
    
    # For SIS & SIR, add recovery
    conditionalPanel(
      condition = 'input.type == "SIS" || input.type == "SIR"',
      sliderInput('beta', "Beta", value = 5, 
                  min = 0, max = 15, step = 0.1),
      sliderInput('gamma', "Recovery Rate", value = 2, 
                  min = 0, max = 15, step = 0.1),
      numericInput('N', "N", value = 150, min = 0, max = 10000)
    ),
    
    # Button to run based on inputs
    actionButton("run", "Run")
  )
  # hr() Draws a horizontal line 
)

# Explore conditional panels




# Customise body
body <- dashboardBody(
  tabItems(
   tabItem(tabName = "sims",
           fluidPage(
             fluidRow(
               tabBox(
                 width = 10,
                 height = 8,
                 tabPanel(title = "test",
                          plotlyOutput("plot_descrip"), 
                          height = 7, width = 9),
                 tabPanel(title = "test2", 
                          plotlyOutput("plot_tau"),
                          height = 7, width = 9)
               )
             )
           )
   ),
   tabItem(tabName = "app_descrip",
           includeMarkdown("README.md")
   )
  )
)

# Bring it all together
dashboardPage(
  dashboardHeader(title = "Stochastic simulation of epidemics",
                  titleWidth = 340),
  sidebar,
  body,
  skin = "red"
)