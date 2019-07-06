# Load necessary packages 
library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)


# Customise sidebar
sidebar <- dashboardSidebar(
  width = 340,
  
  # Read description of app or run directly
  # icons from https://fontawesome.com
  sidebarMenu(id = "nav",
              menuItem("Information", tabName = "app_descrip",
                       icon = icon("info")),
              menuItem("Simulation", tabName = "sims",
                       icon = icon("arrow-circle-right"))
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
      sliderInput('beta', "Beta", value = 0.25, 
                  min = 0, max = 0.5, step = 0.001),
      sliderInput('alpha', "Shape of Gamma distribution \n
                  of interevent times", value = 1, 
                  min = 0.01, max = 5, step = 0.01)
    ),
    
    # For SIS & SIR, add recovery
    conditionalPanel(
      condition = 'input.type == "SIS" || input.type == "SIR"',
      sliderInput('beta', "Beta", value = 0.25, 
                  min = 0, max = 0.5, step = 0.001),
      sliderInput('gamma', "Recovery Rate", value = 0.005, 
                  min = 0, max = 0.5, step = 0.001),
      sliderInput('alpha', "Shape parameter Gamma distribution \n
                  of interevent times", value = 1, 
                  min = 0.01, max = 5, step = 0.01)
    ),
    numericInput('N', "N", value = 200, min = 0, max = 10000),
    numericInput('t_end', "Time frame (days) ", 
                 value = 250, min = 0, max = 500),
    
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
                 tabPanel(title = "Model over time",
                          plotlyOutput("plot_descrip"), 
                          height = 7, width = 9),
                 tabPanel(title = "Event rates", 
                          plotlyOutput("plot_rates"),
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