# This app aims to provide users with the ability to determine suitability of 
# tree species to different environmental conditions based on fitting functions
# to user defined suitability 'scores' and manual judgement.

library(shiny)
library(ggplot2)
library(bs4Dash)
library(gt)
library(janitor)
library(plotly)
library(equatiomatic)

# Load modules
source("Modules/functions.R", local = TRUE)
# source("Modules/loess.R", local = TRUE)

# Define suitability colors
vs_col <- "#9ec7a9"
s_col <- "#EFFFAC"
m_col <- "#FFE9AC"
u_col <- "#FFACAC"


ui <- dashboardPage(
    dashboardHeader(title = "ESC-DSS Function Generator"),
    dashboardSidebar(textInput(inputId = "species", 
                               label = "Species", 
                               value = "Eucalyptus glaucescens"),
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
                         choices = c("Polynomial" = "poly",
                                     "Normal Distribution" = "norm")
                     ),
                     numericInput(
                         inputId = "poly_num",
                         label = "Poly Num:",
                         value = 3
                     )
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            withMathJax(), # Initialize mathJax so the equation renders properly
            box(title = "Suitability Function Generator",
                width = 9,
                height = 600,
                plotOutput("suitPlot"),
                collapsible = FALSE
            ),
            box(title = "Parameter Values",
                width = 3,
                fluidRow(
                    column(6,
                           numericInput(inputId = "x1",
                                        label = NULL,
                                        value = 20),
                           numericInput(inputId = "x2",
                                        label = NULL,
                                        value = 60),
                           numericInput(inputId = "x3",
                                        label = NULL,
                                        value = 90),
                           numericInput(inputId = "x4",
                                        label = NULL,
                                        value = 120),
                           numericInput(inputId = "x5",
                                        label = NULL,
                                        value = 140),
                           numericInput(inputId = "x6",
                                        label = NULL,
                                        value = 160),
                           numericInput(inputId = "x7",
                                        label = NULL,
                                        value = 180),
                           numericInput(inputId = "x8",
                                        label = NULL,
                                        value = 230),
                           numericInput(inputId = "x9",
                                        label = NULL,
                                        value = 260),
                           numericInput(inputId = "x10",
                                        label = NULL,
                                        value = 290)
                           ),
                    column(6,
                           numericInput(inputId = "y1",
                                        label = NULL,
                                        value = 1,
                                        step = 0.1,
                                        max = 1,
                                        min = 0),
                           numericInput(inputId = "y2",
                                        label = NULL,
                                        value = 1,
                                        step = 0.1,
                                        max = 1,
                                        min = 0),
                           numericInput(inputId = "y3",
                                        label = NULL,
                                        value = 1,
                                        step = 0.1,
                                        max = 1,
                                        min = 0),
                           numericInput(inputId = "y4",
                                        label = NULL,
                                        value = 1,
                                        step = 0.1,
                                        max = 1,
                                        min = 0),
                           numericInput(inputId = "y5",
                                        label = NULL,
                                        value = 1,
                                        step = 0.1,
                                        max = 1,
                                        min = 0),
                           numericInput(inputId = "y6",
                                        label = NULL,
                                        value = 1,
                                        step = 0.1,
                                        max = 1,
                                        min = 0),
                           numericInput(inputId = "y7",
                                        label = NULL,
                                        value = 1,
                                        step = 0.1,
                                        max = 1,
                                        min = 0),
                           numericInput(inputId = "y8",
                                        label = NULL,
                                        value = 0.85,
                                        step = 0.1,
                                        max = 1,
                                        min = 0),
                           numericInput(inputId = "y9",
                                        label = NULL,
                                        value = 0.6,
                                        step = 0.1,
                                        max = 1,
                                        min = 0),
                           numericInput(inputId = "y10",
                                        label = NULL,
                                        value = 0.3,
                                        step = 0.1,
                                        max = 1,
                                        min = 0)
                           )
                ),
                collapsible = FALSE
            ),
            withMathJax(), # Initialize mathJax so the equation renders properly
            box(title = NULL,
                width = 12,
                eqOutput("ploy_eq"),
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
    
    

    output$suitPlot <- renderPlot(height = 550,{
        
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
        
        
        if(input$function_type == "poly"){
            
            # Create parameters
            params <- data.frame(x = c(input$x1,
                                       input$x2,
                                       input$x3,
                                       input$x4,
                                       input$x5,
                                       input$x6,
                                       input$x7,
                                       input$x8,
                                       input$x9,
                                       input$x10
                                 ),
                                 y = c(input$y1,
                                       input$y2,
                                       input$y3,
                                       input$y4,
                                       input$y5,
                                       input$y6,
                                       input$y7,
                                       input$y8,
                                       input$y9,
                                       input$y10
                                 )
                                 )
            
            # Set axis min and max values
            max_x <- 320
            
            # params <- data.frame(x = c(0,
            #                            80,
            #                            120,
            #                            200,
            #                            240,
            #                            320
            #                            ),
            #                      y = c(0.2,
            #                            0.4,
            #                            0.6,
            #                            1,
            #                            0.6,
            #                            0.4
            #                            )
            #                     )
            
            # # Create parameters
            # params <- data.frame(x = c(20,
            #                            60,
            #                            90,
            #                            120,
            #                            140,
            #                            160,
            #                            180,
            #                            230,
            #                            260,
            #                            290
            #                       ),
            #                       y = c(1,
            #                             1,
            #                             1,
            #                             1,
            #                             1,
            #                             1,
            #                             1,
            #                             0.85,
            #                             0.6,
            #                             0.3
            #                       )
            #                       )
            
            
            
            
            model <- lm(data = params,
                        y ~ poly(x, input$poly_num)) #input$poly_num
            
            params_fit <- data.frame(x = seq(0:max_x), 
                                     y = predict(object = model, data.frame(x = seq(0:max_x))))
            
            # Find the y intercept values
            y0=0
            f <- splinefun(params_fit$x, params_fit$y)
            lines.0 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)
            
            if(length(lines.0) > 1){
                lines.0_low <- min(lines.0)
                lines.0_high <- max(lines.0)
            } else if(length(lines.0) == 1 & lines.0 < median(params_fit$x)){
                lines.0_low <- lines.0
                lines.0_high <- NA
            } else if(length(lines.0) == 1 & lines.0 > median(params_fit$x)){
                lines.0_low <- NA
                lines.0_high <- lines.0
                
            }
            
            # Find the very suitable range first as these values can be used to determine
            # whether the other values are low or high if only one exists.
            y0=0.75
            f <- splinefun(params_fit$x, params_fit$y)
            lines.75 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)
            
            if(length(lines.75) > 1){
                vs_low <- min(lines.75)
                vs_high <- max(lines.75)
            } else if(lines.75 < lines.0_high){
                vs_low <- NA
                vs_high <- lines.75
            } else if(lines.75 > lines.0_high){
                vs_low <- lines.75
                vs_high <- NA
            }
            
            # Find the mildly suitable range values
            y0=0.25
            f <- splinefun(params_fit$x, params_fit$y)
            lines.25 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)
            
            if(length(lines.25) > 1){
                m_low <- min(lines.25)
                m_high <- max(lines.25) 
            } else if(lines.25 >= vs_high){
                m_low <- NA
                m_high <- lines.25 
            } else if(lines.25 <= vs_low){
                m_low <- lines.25
                m_high <- NA 
            }
            
            # Find the suitable range values
            y0=0.5
            f <- splinefun(params_fit$x, params_fit$y)
            lines.5 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)
            
            if(length(lines.5) > 1){
                s_low <- min(lines.5)
                s_high <- max(lines.5) 
            } else if(lines.5 >= vs_high){
                s_low <- NA
                s_high <- lines.5 
            } else if(lines.5 <= vs_low){
                s_low <- lines.5
                s_high <- NA 
            }
            
            # By default the unsuitable range contains values greater than m_high, but less
            # than the yintercept if... and less than m_low if...
            
            
            
            fit_plot <- ggplot2::ggplot() +
                
                # Add fitted data
                ggplot2::geom_line(data = params_fit,
                                   mapping = ggplot2::aes(x = x,
                                                          y = y),
                                   size = 0.5) +
                
                # Horizontal suitability lines
                ggplot2::geom_hline(yintercept = 0.25,
                                    size = 0.25,
                                    color = "grey") +
                ggplot2::geom_hline(yintercept = 0.50,
                                    size = 0.25,
                                    color = "grey") +
                ggplot2::geom_hline(yintercept = 0.75,
                                    size = 0.25,
                                    color = "grey") +
                
                
                # Vertical range lines
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(m_low), m_low, 0),
                                    size = 0.25,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(s_low), s_low, 0),
                                    size = 0.25,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(vs_low), vs_low, 0),
                                    size = 0.25,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(vs_high), vs_high, max_x),
                                    size = 0.25,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(s_high), s_high, max_x),
                                    size = 0.25,
                                    color = "grey") +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(m_high), m_high, max_x),
                                    size = 0.25,
                                    color = "grey") +
                
                # Unsuitable area fill
                shade_curve(df = params_fit,
                            x = x,
                            y = y,
                            alpha = 0.5,
                            zstart = ifelse(lines.0_low < vs_low & lines.0_low > 0, lines.0_low, 0),
                            zend = ifelse(is.na(m_low), 0, m_low),  
                            fill = "red") +
                shade_curve(df = params_fit,
                            x = x,
                            y = y,
                            alpha = 0.5,
                            zstart = ifelse(is.na(m_high), max_x, m_high),
                            zend = ifelse(is.na(lines.0_high), max_x, lines.0_high),
                            fill = "red") +
                
                # Mildly suitable area fill
                shade_curve(df = params_fit,
                            x = x,
                            y = y,
                            alpha = 0.5,
                            zstart = ifelse(is.na(m_low), 0, m_low),
                            zend = ifelse(is.na(s_low), 0, s_low),
                            fill = "orange") +
                shade_curve(df = params_fit,
                            x = x,
                            y = y,
                            alpha = 0.5,
                            zstart = ifelse(is.na(s_high), max_x, s_high),
                            zend = ifelse(is.na(m_high), max_x, m_high),
                            fill = "orange") +
                
                # Suitable area fill
                shade_curve(df = params_fit,
                            x = x,
                            y = y,
                            alpha = 0.5,
                            zstart = ifelse(is.na(s_low), 0, s_low),
                            zend = ifelse(is.na(vs_low), 0, vs_low),
                            fill = "lightgreen") +
                shade_curve(df = params_fit,
                            x = x,
                            y = y,
                            alpha = 0.5,
                            zstart = ifelse(is.na(vs_high), max_x, vs_high),
                            zend = ifelse(is.na(s_high), max_x, s_high),
                            fill = "lightgreen") +
                
                # Very suitable area fill
                shade_curve(df = params_fit,
                            x = x,
                            y = y,
                            alpha = 0.5,
                            zstart = ifelse(is.na(vs_low), 0, vs_low),  
                            zend = ifelse(is.na(vs_high), max_x, vs_high),  
                            fill = "green") +
                
                # Add parametisation points
                ggplot2::geom_point(data = params,
                                    mapping = ggplot2::aes(x = x,
                                                           y = y,
                                                           size = 0.5)) +
                
                
                
                # Plot options
                ggplot2::coord_cartesian(xlim = c(0, 330),
                                         ylim = c(0,1.1),
                                         expand = FALSE) +
                ggplot2::ggtitle(label = input$species) +
                ggplot2::scale_x_continuous(breaks = seq(0,max_x,20)) +
                ggplot2::scale_y_continuous(breaks = seq(0,1,0.1)) +
                ggplot2::xlab(label = subtitle) +
                ggplot2::ylab(NULL) +
                ggplot2::theme_classic(base_size = 16) +
                ggplot2::theme(legend.position = "none") +
                NULL
            
            fit_plot
            
            # plotly::ggplotly(fit_plot, height = 600)
            
            
            
        }
        
    })
    
    output$ploy_eq <- renderEq({
        
        # Create parameters
        params <- data.frame(x = c(input$x1,
                                   input$x2,
                                   input$x3,
                                   input$x4,
                                   input$x5,
                                   input$x6,
                                   input$x7,
                                   input$x8,
                                   input$x9,
                                   input$x10
                             ),
                             y = c(input$y1,
                                   input$y2,
                                   input$y3,
                                   input$y4,
                                   input$y5,
                                   input$y6,
                                   input$y7,
                                   input$y8,
                                   input$y9,
                                   input$y10
                            )
        )
        
        model <- lm(data = params,
                    y ~ poly(x, input$poly_num))
        
        equatiomatic::extract_eq(model, use_coefs = TRUE, coef_digits = 4) 
        
    })
    
    output$suitTable <- render_gt({
        
        # Create parameters
        params <- data.frame(x = c(input$x1,
                                   input$x2,
                                   input$x3,
                                   input$x4,
                                   input$x5,
                                   input$x6,
                                   input$x7,
                                   input$x8,
                                   input$x9,
                                   input$x10
        ),
        y = c(input$y1,
              input$y2,
              input$y3,
              input$y4,
              input$y5,
              input$y6,
              input$y7,
              input$y8,
              input$y9,
              input$y10
        )
        )
        
        # Set axis min and max values
        max_x <- 320
        
        model <- lm(data = params,
                    y ~ poly(x, input$poly_num)) #input$poly_num
        
        params_fit <- data.frame(x = seq(0:max_x), 
                                 y = predict(object = model, data.frame(x = seq(0:max_x))))
        
        # Find the y intercept values
        y0=0
        f <- splinefun(params_fit$x, params_fit$y)
        lines.0 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)
        
        if(length(lines.0) > 1){
            lines.0_low <- min(lines.0)
            lines.0_high <- max(lines.0)
        } else if(length(lines.0) == 1 & lines.0 < median(params_fit$x)){
            lines.0_low <- lines.0
            lines.0_high <- NA
        } else if(length(lines.0) == 1 & lines.0 > median(params_fit$x)){
            lines.0_low <- NA
            lines.0_high <- lines.0
            
        }
        
        # Find the very suitable range first as these values can be used to determine
        # whether the other values are low or high if only one exists.
        y0=0.75
        f <- splinefun(params_fit$x, params_fit$y)
        lines.75 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)
        
        if(length(lines.75) > 1){
            vs_low <- min(lines.75)
            vs_high <- max(lines.75)
        } else if(lines.75 < lines.0_high){
            vs_low <- NA
            vs_high <- lines.75
        } else if(lines.75 > lines.0_high){
            vs_low <- lines.75
            vs_high <- NA
        }
        
        # Find the mildly suitable range values
        y0=0.25
        f <- splinefun(params_fit$x, params_fit$y)
        lines.25 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)
        
        if(length(lines.25) > 1){
            m_low <- min(lines.25)
            m_high <- max(lines.25) 
        } else if(lines.25 >= vs_high){
            m_low <- NA
            m_high <- lines.25 
        } else if(lines.25 <= vs_low){
            m_low <- lines.25
            m_high <- NA 
        }
        
        # Find the suitable range values
        y0=0.5
        f <- splinefun(params_fit$x, params_fit$y)
        lines.5 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)
        
        if(length(lines.5) > 1){
            s_low <- min(lines.5)
            s_high <- max(lines.5) 
        } else if(lines.5 >= vs_high){
            s_low <- NA
            s_high <- lines.5 
        } else if(lines.5 <= vs_low){
            s_low <- lines.5
            s_high <- NA 
        }
        
        # By default the unsuitable range contains values greater than m_high, but less
        # than the yintercept if... and less than m_low if...
        
        df <- data.frame(x = c(0:320)) |>
            dplyr::mutate(
                suitability =
                    # Maybe it's not best to do this with case_when?
                    dplyr::case_when(
                        # Very suitable
                        x <= ifelse(is.na(vs_high), max_x, vs_high) & x >= ifelse(is.na(vs_low), 0, vs_low) ~ "Very suitable", ##
                        
                        # High suitable
                        x > vs_high & x <= ifelse(is.na(s_high), max_x, s_high) ~ "Suitable", ##
                        # Low suitable
                        x < vs_low & x >= ifelse(is.na(s_low), 0, s_low) ~ "Suitable", ##
                        
                        # High mildly suitable
                        x > s_high & x <= ifelse(is.na(m_high), max_x, m_high) ~ "Mildly suitable",
                        # Low mildly suitable
                        x < s_low & x >= ifelse(is.na(m_low), 0, m_low) ~ "Mildly suitable",
                        
                        # High unsuitable
                        x > m_high ~ "Unsuitable",
                        # Low unsuitable
                        x < m_low ~ "Unsuitable",
                        
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
