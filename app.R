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
library(bs4Dash)

# Define suitability colors
vs_col <- "#9ec7a9"
s_col <- "#EFFFAC"
m_col <- "#FFE9AC"
u_col <- "#FFACAC"


ui <- dashboardPage(
    dashboardHeader(title = "ESC-DSS Function Generator"),
    dashboardSidebar(textInput(inputId = "species", 
                               label = "Species", 
                               value = ""),
                     selectInput(
                         inputId = "suit_factor",
                         label = "Suitability Factor:",
                         choices = c("Moisture Deficit" = "md",
                                     "Accumulated Temperature" = "at",
                                     "Continentality" = "ct",
                                     "Exposure" = "dams",
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
                                 max = 150,
                                 value = 50),
                     sliderInput("vs_range",
                                 "Very Suitable Range:",
                                 min = 0,
                                 max = 1,
                                 value = 0.25),
                     sliderInput("s_range",
                                 "Suitable Range:",
                                 min = 0,
                                 max = 0.5,
                                 value = 0.125),
                     sliderInput("m_range",
                                 "Mildy Suitable Range:",
                                 min = 0,
                                 max = 0.5,
                                 value = 0.125)#,
                     # sliderInput("u_bound",
                     #             "Unsuitable Bound:",
                     #             min = 0,
                     #             max = 0.5,
                     #             value = 0.25)
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(title = "Suitability Function Generator",
                width =9,
                plotOutput("suitPlot"),
                collapsible = FALSE
            ),
            box(title = "Parameter Values",
                width = 3,
                numericInput(inputId = "x",
                             label = "X value",
                             value = 160),
                numericInput(inputId = "y",
                             label = "Y value",
                             value = 1),
                collapsible = FALSE
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    

    output$suitPlot <- renderPlot({
        
        # Define shading function
        # Adapted from: https://sebastiansauer.github.io/shade_Normal_curve/
        
        shade_curve <- function(df, x, y, fill, zstart, zend, alpha){
            ggplot2::geom_area(data = dplyr::filter(df, 
                                                    x >= zstart,
                                                    x < zend),
                               ggplot2::aes(y = y), 
                               fill = fill,
                               color = NA, 
                               alpha = alpha)
        }
        
        
        if(input$function_type == "norm"){
            
            # Create parameters
            params <- data.frame(x = input$x,
                                 y = input$y)
            
            
            # Retrieve mean and standard deviation inputs
            mean <- input$mean
            sd <- input$sd
            
            # Create normal distribution data
            norm_dist <- dnorm(x = c(0:320), mean = mean, sd = sd)
            max <- max(norm_dist)
            norm_dist_rel <- data.frame(x = c(0:320), y = norm_dist/max)

            # Retrieve user-stipulated suitability ranges
            vs_range <- input$vs_range
            s_range <- input$s_range
            m_range <- input$m_range

            # Create x intercepts for suitability ranges
            vline_1 <- qnorm(0.5 + (vs_range/2), mean = mean, sd = sd)
            vline_2 <- qnorm(0.5 - (vs_range/2), mean = mean, sd = sd)
            vline_3 <- qnorm(0.5 + (vs_range/2) + (s_range), mean = mean, sd = sd)
            vline_4 <- qnorm(0.5 - (vs_range/2) - (s_range), mean = mean, sd = sd)
            vline_5 <- qnorm(0.5 + (vs_range/2) + (s_range) + (m_range), mean = mean, sd = sd)
            vline_6 <- qnorm(0.5 - (vs_range/2) - (s_range) - (m_range), mean = mean, sd = sd)
            
            p1 <- ggplot2::ggplot(data = norm_dist_rel, mapping = ggplot2::aes(x = x,
                                                                               y = y)) +
                ggplot2::geom_line() +
                shade_curve(df = norm_dist_rel,
                            x = x,
                            y = y,
                            alpha = 0.3,
                            zstart = vline_1, 
                            zend = vline_2, 
                            fill = vs_col) +
                
                
                # ggplot2::geom_area(mapping = ggplot2::aes(x = x, y = y),
                #                    fill = vs_col, 
                #                    xlim = c(vline_1, vline_2)) +

                # # Very suitable area fill
                # ggplot2::stat_function(fun = dnorm,
                #                        geom = "area",
                #                        fill = vs_col,
                #                        alpha = 0.3,
                #                        args = list(mean = mean, sd = sd),
                #                        xlim = c(vline_1, vline_2)) +
                # # Suitable area fill
                # ggplot2::stat_function(fun = dnorm,
                #                        geom = "area",
                #                        fill = s_col,
                #                        alpha = 0.3,
                #                        args = list(mean = mean, sd = sd),
                #                        xlim = c(vline_1, vline_3)) +
                # ggplot2::stat_function(fun = dnorm,
                #                        geom = "area",
                #                        fill = s_col,
                #                        alpha = 0.3,
                #                        args = list(mean = mean, sd = sd),
                #                        xlim = c(vline_2, vline_4)) +
                # # Mildy suitable area fill
                # ggplot2::stat_function(fun = dnorm,
                #                        geom = "area",
                #                        fill = m_col,
                #                        alpha = 0.3,
                #                        args = list(mean = mean, sd = sd),
                #                        xlim = c(vline_3, vline_5)) +
                # ggplot2::stat_function(fun = dnorm,
                #                        geom = "area",
                #                        fill = m_col,
                #                        alpha = 0.3,
                #                        args = list(mean = mean, sd = sd),
                #                        xlim = c(vline_4, vline_6)) +
                # # Unsuitable area fill
                # ggplot2::stat_function(fun = dnorm,
                #                        geom = "area",
                #                        fill = u_col,
                #                        alpha = 0.3,
                #                        args = list(mean = mean, sd = sd),
                #                        xlim = c(vline_5, 320)) +
                # ggplot2::stat_function(fun = dnorm,
                #                        geom = "area",
                #                        fill = u_col,
                #                        alpha = 0.3,
                #                        args = list(mean = mean, sd = sd),
                #                        xlim = c(0, vline_6)) +
                
            #     # Add parametisation values
            #     ggplot2::geom_point(data = params,
            #                         mapping = ggplot2::aes(x = x,
            #                                                y = y,
            #                                                color = "red",
            #                                                size = 12)) +
            #     
            #     # Vertical suitability lines
            #     ggplot2::geom_vline(xintercept = vline_1,
            #                         size = 0.5,
            #                         color = "grey") +
            #     ggplot2::geom_vline(xintercept = vline_2,
            #                         size = 0.5,
            #                         color = "grey") +
            #     ggplot2::geom_vline(xintercept = vline_3,
            #                         size = 0.5,
            #                         color = "grey") +
            #     ggplot2::geom_vline(xintercept = vline_4,
            #                         size = 0.5,
            #                         color = "grey") +
            #     ggplot2::geom_vline(xintercept = vline_5,
            #                         size = 0.5,
            #                         color = "grey") +
            #     ggplot2::geom_vline(xintercept = vline_6,
            #                         size = 0.5,
            #                         color = "grey") +
            #     ggplot2::coord_cartesian(xlim = c(0, 320),
            #                              expand = FALSE) +
            #     ggplot2::ggtitle(label = input$species,
            #                      subtitle = input$suit_factor) +
            #     ggplot2::scale_x_continuous(breaks = seq(0,320,20)) +
            #     ggplot2::scale_x_continuous(breaks = c(0:1)) +
            #     ggplot2::xlab(label = "Moisture Deficit (MD)") +
            #     ggplot2::ylab(NULL) +
            #     ggplot2::theme_classic(base_size = 16) +
            #     ggplot2::theme(legend.position = "none") +
                NULL



            p1
        
        }
        
    })
    
    # output$suitTable <- renderTable({
    #     
    #     mean <- input$mean
    #     sd <- input$sd
    #     
    #     vs_range <- input$vs_range
    #     s_range <- input$s_range
    #     m_range <- input$m_range
    #     # u_range <- input$u_range
    #     
    #     vline_1 <- qnorm(0.5 + (vs_range/2), mean = mean, sd = sd)
    #     vline_2 <- qnorm(0.5 - (vs_range/2), mean = mean, sd = sd)
    #     vline_3 <- qnorm(0.5 + (vs_range/2) + (s_range/2), mean = mean, sd = sd)
    #     vline_4 <- qnorm(0.5 - (vs_range/2) - (s_range/2), mean = mean, sd = sd)
    #     vline_5 <- qnorm(0.5 + (vs_range/2) + (s_range/2) + (m_range/2), mean = mean, sd = sd)
    #     vline_6 <- qnorm(0.5 - (vs_range/2) - (s_range/2) - (m_range/2), mean = mean, sd = sd)
    #     
    #     df <- 
    #     
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
