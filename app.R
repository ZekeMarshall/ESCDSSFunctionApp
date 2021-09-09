#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ESC-DSS Function Generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "suit_factor",
                label = "Suitability Factor:",
                choices = c("Accumulated Temperature" = "at",
                            "Continentality" = "ct",
                            "Exposure" = "dams",
                            "Moisture Deficit" = "md",
                            "Soil Moisture Regime" = "smr",
                            "Soil Nutrient Regime" = "snr")
            ),
            selectInput(
                inputId = "function_type",
                label = "Function:",
                choices = c("Normal Distribution" = "norm")
            ),
            sliderInput("mean",
                        "Mean:",
                        min = 0,
                        max = 320,
                        value = 160),
            sliderInput("sd",
                        "Spread:",
                        min = 0,
                        max = 100,
                        value = 15)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("suitPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$suitPlot <- renderPlot({
        
        if(input$function_type == "norm"){
            
            mean <- input$mean
            sd <- input$sd
            
            q1 <- qnorm(.25, mean = mean, sd = sd)
            q2 <- qnorm(.50, mean = mean, sd = sd)
            q3 <- qnorm(.75, mean = mean, sd = sd)
            
    
            p1 <- ggplot2::ggplot(data = data.frame(x = c(0, 320)), aes(x)) +
                ggplot2::stat_function(fun = dnorm, n = 155, args = list(mean = mean, sd = sd)) +
                ggplot2::geom_vline(xintercept = q1,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = q2,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = q3,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::theme_classic() +
                ggplot2::coord_cartesian(xlim = c(0, 320),
                                         expand = FALSE) +
                NULL
                
            
            
            p1
        
        }
        
        # output$suitTable
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
