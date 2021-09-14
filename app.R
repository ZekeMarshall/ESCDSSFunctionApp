# This app aims to provide users with the ability to determine suitability of 
# tree species to different environmental conditions based on fitting functions
# to user defined suitability 'scores'.

library(shiny)
library(ggplot2)
library(bs4Dash)
library(gt)
library(janitor)
library(broom)
library(glue)
library(equatiomatic)

# Load modules
source("Modules/functions.R", local = TRUE)
source("Modules/constants.R", local = TRUE)
source("Modules/md.R", local = TRUE)

# User interface
ui <- dashboardPage(
    
    dashboardHeader(
        
        title = "ESC-DSS Suitability Tool"
        
        ), # Close dashboardHeader
    
    
    dashboardSidebar(
        
        sidebarMenu(
            # id = "sidebarMenu",
            menuItem(
                text = "Moisture Deficit",
                tabName = "md"
            ),
            menuItem(
                text = "Accumulated Temperature",
                tabName = "at"
            ),
            menuItem(
                text = "Continentality",
                tabName = "ct"
            ),
            menuItem(
                text = "Exposure",
                tabName = "dams"
            ),
            menuItem(
                text = "Soil Moisture Regime",
                tabName = "smr"
            ),
            menuItem(
                text = "Soil Nutrient Regime",
                tabName = "snr"
            ),
            menuItem(
                text = "Summary",
                tabName = "summary"
            )
            
        ) # Close sidebarMenu
        
    ), # Close dashboardSidebar
    
    dashboardBody(
        
        tabItems(

            tabItem(tabName = "md",
                    mdUI(id = "md.id"))

        ) # Close tabItems
    
    ) # Close Body
    
) # Close UI


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    callModule(module = md,
               id = "md.id")
    
} # Close server

# Run the application 
shinyApp(ui = ui, server = server)
