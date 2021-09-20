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
        
        # tags$head(
        #     tags$style(HTML(".box {
        #               height: 90vh; overflow-y: auto;
        #             }"
        #     ) # close HTML       
        #     )            # close tags$style
        # ),             # close tags#Head
        # 
        
        tabItems(

            tabItem(tabName = "md",
                    suitUI(id = "md.id"))
            
            # tabItem(tabName = "at",
            #         suitUI(id = "at.id")),
            # 
            # tabItem(tabName = "ct",
            #         suitUI(id = "ct.id")),
            # 
            # tabItem(tabName = "dams",
            #         suitUI(id = "dams.id")),
            # 
            # tabItem(tabName = "smr",
            #         suitUI(id = "smr.id")),
            # 
            # tabItem(tabName = "snr",
            #         suitUI(id = "snr.id"))
            
            

        ) # Close tabItems
    
    ) # Close Body
    
) # Close UI


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    callModule(module = suit,
               id = "md.id",
               suit_factor = "md",
               max_x = 320)
    
    # callModule(module = suit,
    #            id = "at.id",
    #            suit_factor = "Accumulated temperature",
    #            max_x = 320)
    # 
    # callModule(module = suit,
    #            id = "ct.id",
    #            suit_factor = "Continentality",
    #            max_x = 320)
    # 
    # callModule(module = suit,
    #            id = "dams.id",
    #            suit_factor = "Exposure",
    #            max_x = 320)
    # 
    # callModule(module = suit,
    #            id = "smr.id",
    #            suit_factor = "Soil Moisture Regime",
    #            max_x = 320)
    # 
    # callModule(module = suit,
    #            id = "snr.id",
    #            suit_factor = "Soil Nutrient Regime",
    #            max_x = 320)
    
    
    
} # Close server

# Run the application 
shinyApp(ui = ui, server = server)
