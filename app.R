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
            
            at_x_vec <- c(0:320)

            n <- 155

            res <- dnorm(x =  at_x_vec, mean = as.numeric(mean), sd = as.numeric(sd))
            ymax <- max(res)

            # ***Change these to actual quartile values from res***
            ymax_1q <- ymax*0.25
            # ymax_1qy <- qnorm(.25, mean = 150, sd = 15)
            ymax_2q <- ymax*0.5
            ymax_3q <- ymax*0.75
            
            # Find points where ymax is above res
            above_1q <- ymax_1q > res
            above_2q <- ymax_2q > res
            above_3q <- ymax_3q > res
            
            # Points always intersect when above=TRUE, then FALSE or reverse
            intersect_points_1q <- which(diff(above_1q) != 0)
            intersect_points_2q <- which(diff(above_2q) != 0)
            intersect_points_3q <- which(diff(above_3q) != 0)
            
    
            p1 <- ggplot2::ggplot(data = data.frame(x = c(0, 320)), aes(x)) +
                ggplot2::stat_function(fun = dnorm, n = 155, args = list(mean = mean, sd = sd)) +
                ggplot2::geom_hline(yintercept = ymax_1q,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = intersect_points_1q[1],
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = intersect_points_1q[2],
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_hline(yintercept = ymax_2q,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = intersect_points_2q[1],
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = intersect_points_2q[2],
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_hline(yintercept = ymax_3q,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = intersect_points_3q[1],
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = intersect_points_3q[2],
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::theme_classic() +
                ggplot2::coord_cartesian(xlim = c(0, 320),
                                         expand = FALSE) +
                NULL
                
            
            
            p1
        
        }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
