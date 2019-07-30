###################
## ui Shiny file ##
###################


# Load necessary packages
load_packages <- Vectorize(function(package) {
  
  # Check if package is installed, suppress warning message (only logical T/F)
  condition <- suppressWarnings(!require(package, character.only = T))
  
  # Install if not installed yet
  if (condition)
    install.packages(package, dep = T)
  
  # Load
  require(package, character.only = T)
})

packages <- c("shiny", "shinydashboard", "plotly",
              "shinyWidgets", "tidyverse", "deSolve")

load_packages(packages)


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
    
    # Add transmission param and alpha for interevent times
    sliderInput('beta', "Beta (per year)", value = 36.5, 
                min = 0, max = 50, step = 0.1),
    sliderInput('alpha', "Shape of Gamma distribution \n
                  of interevent times", value = 1, # exponential by default
                min = 0, max = 5, step = 0.01),
    
    
    # For SIS & SIR, add recovery
    conditionalPanel(
      condition = 'input.type == "SIS" || input.type == "SIR"',
      sliderInput('gamma', "Average infectious period (days)", value = 50, 
                  min = 0, max = 365, step = 1)
    ),

    hr(),
    
    # Button to run based on inputs
    actionButton("run", "Run"),
    
    h5("Initial conditions:", style="padding:15px;"),
    splitLayout(
      cellWidths = c("29%", "29%", "29%"),
      numericInput('N', "N", value = 200, min = 0, max = 1000),
      numericInput('S', "S", value = 199, min = 0, max = 1000),            
      numericInput('I', "I", value = 1, min = 0, max = 1000)
    ),
    
    numericInput('t_end', "Time frame (days) ", 
                 value = 360, min = 0, max = 365) # Two months 
    
  )
)

# Customise body
body <- dashboardBody(
  tabItems(
   tabItem(tabName = "sims",
             fluidRow(
               tabBox(
                 width = 12,
                 tabPanel(title = "Trajectories",
                          plotlyOutput("plot_descrip"), 
                          height = 7, width = 9),
                 tabPanel(title = "Event rates", 
                          plotlyOutput("plot_rates"),
                          height = 7, width = 9),
                 tabPanel(title = "Deterministic",
                          plotlyOutput("plot_determ"),
                          height = 7, width = 9),
                 tabPanel(title = "Interevent vs. # Infectious",
                          plotlyOutput("interev_vs_I"),
                          height = 7, width = 9),
                 tabPanel(title = "Distribution interevent",
                          plotlyOutput("interev_dist"),
                          height = 7, width = 9)
               )
             ),
             fluidRow(
               box(
                 materialSwitch(inputId = "overlay_determ", 
                                label = "Overlay deterministic curves", 
                                status = "success", 
                                value = FALSE)
               ),
               box(
                 textInput("filename", "Save a .csv of the dataset:", 
                           "enter_filename_here"),
                 downloadButton("download", "Download")
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
