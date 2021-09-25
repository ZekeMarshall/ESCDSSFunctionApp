# This app aims to provide users with the ability to determine suitability gradients
# of tree species to different environmental conditions based on fitting functions
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
source("Modules/suitability.R", local = TRUE)
source("Modules/data.R", local = TRUE)
source("Modules/filepaths.R", local = TRUE)

# User interface
ui <- dashboardPage(
    
    dashboardHeader(
        
        title = "ESC-DSS Suitability Tool"
        
        ), # Close dashboardHeader
    
    
    dashboardSidebar(
        
        sidebarMenu(
            # id = "sidebarMenu",
            menuItem(
                text = "Suitability",
                tabName = "suit"
            ),
            menuItem(
                text = "New Species",
                tabName = "at"
            ),
            menuItem(
                text = "Summary",
                tabName = "summary"
            )
            
        ) # Close sidebarMenu
        
    ), # Close dashboardSidebar
    
    dashboardBody(
        
        # tags$head(
        #     tags$style(HTML(".box {
        #               height: 90vh; overflow-y: auto;
        #             }"
        #     ) # close HTML       
        #     )            # close tags$style
        # ),             # close tags#Head
        # 
        
        tabItems(

            tabItem(tabName = "suit",
                    suitUI(id = "suit.id"))
            
            

        ) # Close tabItems
    
    ) # Close Body
    
) # Close UI


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    callModule(module = suit,
               id = "suit.id")
    
    
    
} # Close server

# Run the application 
shinyApp(ui = ui, server = server)
