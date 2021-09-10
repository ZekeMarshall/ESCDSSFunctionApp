# This app aims to provide users with the ability to determine suitability of 
# tree species to different environmental conditions based on fitting functions
# to user defined suitability 'scores' and manual judgement.

library(shiny)
library(ggplot2)
library(bs4Dash)
library(gt)
library(janitor)

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
                     # sliderInput("mean",
                     #             "Mean:",
                     #             min = 0,
                     #             max = 320,
                     #             value = 160),
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
                fluidRow(
                    column(6,
                           numericInput(inputId = "mean",
                                        label = "Peak X Value",
                                        value = 160),
                           numericInput(inputId = "x1",
                                        label = "X Parameters",
                                        value = ""),
                           numericInput(inputId = "x2",
                                        label = NULL,
                                        value = ""),
                           numericInput(inputId = "x3",
                                        label = NULL,
                                        value = ""),
                           numericInput(inputId = "x4",
                                        label = NULL,
                                        value = ""),
                           numericInput(inputId = "x5",
                                        label = NULL,
                                        value = ""),
                           numericInput(inputId = "x6",
                                        label = NULL,
                                        value = "")
                           ),
                    column(6,
                           numericInput(inputId = "ymean",
                                        label = "Peak Y Value",
                                        value = 1),
                           numericInput(inputId = "y1",
                                        label = "Y Parameters",
                                        value = ""),
                           numericInput(inputId = "y2",
                                        label = NULL,
                                        value = ""),
                           numericInput(inputId = "y3",
                                        label = NULL,
                                        value = ""),
                           numericInput(inputId = "y4",
                                        label = NULL,
                                        value = ""),
                           numericInput(inputId = "y5",
                                        label = NULL,
                                        value = ""),
                           numericInput(inputId = "y6",
                                        label = NULL,
                                        value = ""),
                           )
                ),
                collapsible = FALSE
            ),
            box(title = "Suitability Data",
                width = 12,
                gt_output("suitTable"),
                collapsible = FALSE),
            box(title = "Notes",
                width = 12,
                collapsible = FALSE,
                tags$h3("Process"),
                tags$ol(
                    tags$li("Select a suitability factor"),
                    tags$li("Select a function (norm, norm-rskew, norm-lskew,...."),
                    tags$li("Select a mean/peak suitability value"), 
                    tags$li("Enter parameter data points")
                ),
                tags$h3("Questions"),
                tags$ul(
                    tags$li("What do the 0-1 scores mean? We can either fix
                            them at intervals which correspond to the suitability
                            categories; or ignore these values and set the ranges 
                            manually using the sliders, but then we might as well 
                            just be selecting the ranges without a curve!"), 
                    tags$li("Second list item"), 
                    tags$li("Third list item")
                )
                
                
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    

    output$suitPlot <- renderPlot({
        
        # Determine subtitle
        if(input$suit_factor == "md"){
            subtitle = "Moisture Deficit"
        } else if(input$suit_factor == "at"){
            subtitle = "Accumulated Temperature"
        } else if(input$suit_factor == "ct"){
            subtitle = "Continentality"
        } else if(input$suit_factor == "dams"){
            subtitle = "Exposure"
        } else if(input$suit_factor == "smr"){
            subtitle = "Soil Moisture Regime"
        } else if(input$suit_factor == "snr"){
            subtitle = "Soil Nutrient Regime"
        }
        
        # Define shading function
        # Adapted from: https://sebastiansauer.github.io/shade_Normal_curve/
        shade_curve <- function(df, x, y, fill, zstart, zend, alpha){
            ggplot2::geom_area(data = dplyr::filter(df, 
                                                    x >= zstart,
                                                    x <= zend),
                               ggplot2::aes(y = y), 
                               fill = fill,
                               color = "black", 
                               alpha = alpha)
        }
        
        
        if(input$function_type == "norm"){
            
            # Create parameters
            params <- data.frame(x = c(input$mean,
                                       input$x1,
                                       input$x2,
                                       input$x3,
                                       input$x4,
                                       input$x5,
                                       input$x6),
                                 y = c(input$ymean,
                                       input$y1,
                                       input$y2,
                                       input$y3,
                                       input$y4,
                                       input$y5,
                                       input$y6)
                                 )
            
            
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
            
            
            # Plot data
            p1 <- ggplot2::ggplot(data = norm_dist_rel, mapping = ggplot2::aes(x = x,
                                                                               y = y)) +
                ggplot2::geom_line() +
                shade_curve(df = norm_dist_rel,
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = vline_2, 
                            zend = vline_1, 
                            fill = vs_col) +
                shade_curve(df = norm_dist_rel,
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = vline_4, 
                            zend = vline_2, 
                            fill = s_col) +
                shade_curve(df = norm_dist_rel,
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = vline_1, 
                            zend = vline_3, 
                            fill = s_col) +
                shade_curve(df = norm_dist_rel,
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = vline_3, 
                            zend = vline_5, 
                            fill = m_col) +
                shade_curve(df = norm_dist_rel,
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = vline_6, 
                            zend = vline_4, 
                            fill = m_col) +
                shade_curve(df = norm_dist_rel,
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = 0, 
                            zend = vline_6, #ifelse statements
                            fill = u_col) +
                shade_curve(df = norm_dist_rel,
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = vline_5, 
                            zend = 320, 
                            fill = u_col) +
                
                # Add parametisation values
                ggplot2::geom_point(data = params,
                                    mapping = ggplot2::aes(x = x,
                                                           y = y,
                                                           color = "red",
                                                           size = 12)) +

                # Vertical suitability lines
                ggplot2::geom_vline(xintercept = vline_1,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = vline_2,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = vline_3,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = vline_4,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = vline_5,
                                    size = 0.5,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = vline_6,
                                    size = 0.5,
                                    color = "grey") +
                
                # Horizontal suitability lines
                ggplot2::geom_hline(yintercept =
                                        dplyr::filter(norm_dist_rel,
                                                      abs(x-vline_1) == min(abs(x-vline_1))) |>
                                        dplyr::select(y) |>
                                        as.numeric(),
                                    size = 0.5,
                                    color = "grey") +

                ggplot2::geom_hline(yintercept =
                                        dplyr::filter(norm_dist_rel,
                                                      abs(x-vline_3) == min(abs(x-vline_3))) |>
                                        dplyr::select(y) |>
                                        as.numeric(),
                                    size = 0.5,
                                    color = "grey") +

                ggplot2::geom_hline(yintercept =
                                        dplyr::filter(norm_dist_rel,
                                                      abs(x-vline_5) == min(abs(x-vline_5))) |>
                                        dplyr::select(y) |>
                                        as.numeric(),
                                    size = 0.5,
                                    color = "grey") +
            
                # ggplot2::geom_hline(yintercept = 0.25) + 
                # 
                # ggplot2::geom_hline(yintercept = 0.50) +
                # 
                # ggplot2::geom_hline(yintercept = 0.75) +
                
                # Labels
                
                ggplot2::geom_text(ggplot2::aes(x = vline_1, 
                                                y = 1, 
                                                label = format(round(vline_1, 1), nsmall = 1), 
                                                hjust = -0.1)) +
                
                
                ggplot2::geom_text(ggplot2::aes(x = vline_3, 
                                                y = 1, 
                                                label = format(round(vline_3, 1), nsmall = 1), 
                                                hjust = -0.1)) +
                
                
                # Graph attributes
                ggplot2::coord_cartesian(xlim = c(0, 330),
                                         ylim = c(0,1.1),
                                         expand = FALSE) +
                ggplot2::ggtitle(label = input$species) +
                ggplot2::scale_x_continuous(breaks = seq(0,320,20)) +
                ggplot2::scale_y_continuous(breaks = seq(0,1,0.1)) +
                ggplot2::xlab(label = subtitle) +
                ggplot2::ylab(NULL) +
                ggplot2::theme_classic(base_size = 16) +
                ggplot2::theme(legend.position = "none") +
                NULL



            p1
        
        }
        
    })
    
    output$suitTable <- render_gt({
        
        # Retrieve mean and standard deviation inputs
        mean <-  input$mean
        sd <-  input$sd
        
        # Retrieve user-stipulated suitability ranges
        vs_range <- input$vs_range
        s_range <-  input$s_range
        m_range <-  input$m_range
        
        # Create x intercepts for suitability ranges
        vline_1 <- qnorm(0.5 + (vs_range/2), mean = mean, sd = sd)
        vline_2 <- qnorm(0.5 - (vs_range/2), mean = mean, sd = sd)
        vline_3 <- qnorm(0.5 + (vs_range/2) + (s_range), mean = mean, sd = sd)
        vline_4 <- qnorm(0.5 - (vs_range/2) - (s_range), mean = mean, sd = sd)
        vline_5 <- qnorm(0.5 + (vs_range/2) + (s_range) + (m_range), mean = mean, sd = sd)
        vline_6 <- qnorm(0.5 - (vs_range/2) - (s_range) - (m_range), mean = mean, sd = sd)
        
        df <- data.frame(x = c(0:320)) |> 
            dplyr::mutate(
                suitability = 
                    dplyr::case_when(
                        # Very suitable
                        x <= vline_1 & x >= vline_2 ~ "Very suitable",
                        x > vline_1 & x <= vline_3 ~ "Suitable",
                        x < vline_2 & x >= vline_4 ~ "Suitable",
                        x < vline_4 & x >= vline_6 ~ "Mildly suitable",
                        x > vline_3 & x <= vline_5 ~ "Mildly suitable",
                        x < vline_5 ~ "Unsuitable",
                        x > vline_6 ~ "Unsuitable",
                        TRUE ~ as.character(x)
                )
            )


        # Create data frame
        df_table <- df |>
            dplyr::filter(x %in% seq(0, 320, 20))
        
        df_table_t <- df_table |>
            data.table::transpose() |>
            janitor::row_to_names(1)
        
        # Create table
        table <- gt::gt(df_table_t) |> 
            gt::data_color(
                columns = gt::everything(),
                color = scales::col_factor(
                    palette = c(vs_col, s_col, m_col, u_col),
                    levels = c("Very suitable", "Suitable", "Mildly suitable", "Unsuitable")
                    )
            ) |> 
            gt::cols_align(
                align = c("center"),
                columns = gt::everything()
            ) |> 
            gt::tab_options(table.width = "100%")
            
        
        # Print table
        table 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
