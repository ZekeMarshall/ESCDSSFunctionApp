# This app aims to provide users with the ability to determine suitability of 
# tree species to different environmental conditions based on fitting functions
# to user defined suitability 'scores'.

library(shiny)
library(ggplot2)
library(bs4Dash)
library(gt)
library(janitor)
# library(plotly)
library(equatiomatic)

# Load modules
source("Modules/functions.R", local = TRUE)

# Read default scores data

# Define suitability colors
vs_col <- "#9ec7a9"
s_col <- "#EFFFAC"
m_col <- "#FFE9AC"
u_col <- "#FFACAC"

# Define horizontal and vertical line color
line_col <- "black"

# Define horizontal and vertical line size
line_size <- 0.15

# Establish default list of species


# User interface
ui <- dashboardPage(
    dashboardHeader(
        title = "ESC-DSS Suitability Tool"
        ),
    
    dashboardSidebar(
        
        # sidebarMenu(
        #     id = "sidebarMenu",
        #     menuItem(
        #         text = "Moisture Deficit",
        #         tabName = "md"
        #     ),
        #     menuItem(
        #         text = "Accumulated Temperature",
        #         tabName = "at"
        #     ),
        #     menuItem(
        #         text = "Continetaliity",
        #         tabName = "ct"
        #     ),
        #     menuItem(
        #         text = "Exposure",
        #         tabName = "dams"
        #     ),
        #     menuItem(
        #         text = "Soil Moisture Regime",
        #         tabName = "smr"
        #     ),
        #     menuItem(
        #         text = "Soil Nutrient Regime",
        #         tabName = "snr"
        #     ),
        # )
        
        
        
        
        textInput(inputId = "species", 
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
                         choices = c("Polynomial" = "poly")
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
                fluidRow(
                    column(8,
                           eqOutput("ploy_eq")
                    ),
                    column(4,
                           eqOutput("model_r.squared"))
                ),
                # header = NULL,
                collapsible = FALSE
            ),
            box(title = "Suitability Data",
                width = 12,
                gt_output("suitTable"),
                # header = NULL,
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
server <- function(input, output, x1) {
    
    # # These observe events update the variables for selection
    # observeEvent(input$suit_factor,  {
    #     
    #     req(input$suit_factor)
    # 
    #     # Determine maximum x axis value
    #     if(input$suit_factor == "md"){
    #         max_x = 320
    #     } else if(input$suit_factor == "at"){
    #         max_x = 3000
    #     } else if(input$suit_factor == "ct"){
    #         max_x = 12
    #     } else if(input$suit_factor == "dams"){
    #         max_x = 24
    #     } else if(input$suit_factor == "smr"){
    #         max_x = 8
    #     } else if(input$suit_factor == "snr"){
    #         max_x = 6
    #     }
    #     
    # })
    # 
    # # These observe events update the variables for selection
    # observeEvent(input$species,  {
    #     req(input$species)
    #     req(input$suit_factor)
    #     
    #     updateNumericInput(session,
    #                        inputId = "x1",
    #                        choices = dplyr::filter(default_scores, 
    #                                                species == input$species,
    #                                                suit_factor == input$suit_factor,
    #                                                x = ""))
    # })
    
    
    # Create reactive objects
    
    max_x <- 320
    
    params <- reactive({
        
        data.frame(x = c(input$x1,
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
        })
            
    

    # Create a polynomial model to fit suitability score parameters
    model <- reactive({
        
        lm(data = params(),
           y ~ poly(x, input$poly_num))
        
    })
    
    # Fitted model data
    params_fit <- reactive({
        
        data.frame(x = seq(0:max_x),
                   y = predict(object = model(), data.frame(x = seq(0:max_x))))
        
    })
    
    # x intercept values
    
    x_ints <- reactive({
        
       get_x_intercepts(fitted_data = params_fit(), x = x, y = y)
        
    })
    
    


    

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
        
            
            fit_plot <- ggplot2::ggplot() +
                
                # Unsuitable area fill
                shade_curve(df = params_fit(),
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = ifelse(x_ints()$lines.0_low < x_ints()$vs_low & x_ints()$lines.0_low > 0, x_ints()$lines.0_low, 0),
                            zend = ifelse(is.na(x_ints()$m_low), 0, x_ints()$m_low),  
                            fill = u_col) +
                shade_curve(df = params_fit(),
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = ifelse(is.na(x_ints()$m_high), max_x, x_ints()$m_high),
                            zend = ifelse(is.na(x_ints()$lines.0_high), max_x, x_ints()$lines.0_high),
                            fill = u_col) +
                
                # Mildly suitable area fill
                shade_curve(df = params_fit(),
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = ifelse(is.na(x_ints()$m_low), 0, x_ints()$m_low),
                            zend = ifelse(is.na(x_ints()$s_low), 0, x_ints()$s_low),
                            fill = m_col) +
                shade_curve(df = params_fit(),
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = ifelse(is.na(x_ints()$s_high), max_x, x_ints()$s_high),
                            zend = ifelse(is.na(x_ints()$m_high), max_x, x_ints()$m_high),
                            fill = m_col) +
                
                # Suitable area fill
                shade_curve(df = params_fit(),
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = ifelse(is.na(x_ints()$s_low), 0, x_ints()$s_low),
                            zend = ifelse(is.na(x_ints()$vs_low), 0, x_ints()$vs_low),
                            fill = s_col) +
                shade_curve(df = params_fit(),
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = ifelse(is.na(x_ints()$vs_high), max_x, x_ints()$vs_high),
                            zend = ifelse(is.na(x_ints()$s_high), max_x, x_ints()$s_high),
                            fill = s_col) +
                
                # Very suitable area fill
                shade_curve(df = params_fit(),
                            x = x,
                            y = y,
                            alpha = 1,
                            zstart = ifelse(is.na(x_ints()$vs_low), 0, x_ints()$vs_low),  
                            zend = ifelse(is.na(x_ints()$vs_high), max_x, x_ints()$vs_high),  
                            fill = vs_col) +
                
                # Horizontal suitability lines
                ggplot2::geom_hline(yintercept = 0.25,
                                    size = line_size,
                                    color = line_col) +
                ggplot2::geom_hline(yintercept = 0.50,
                                    size = line_size,
                                    color = line_col) +
                ggplot2::geom_hline(yintercept = 0.75,
                                    size = line_size,
                                    color = line_col) +
                ggplot2::geom_hline(yintercept = 1,
                                    size = line_size,
                                    color = line_col) +
                
                
                # Vertical range lines
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(x_ints()$m_low), x_ints()$m_low, 0),
                                    size = line_size,
                                    color = line_col) +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(x_ints()$s_low), x_ints()$s_low, 0),
                                    size = line_size,
                                    color = line_col) +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(x_ints()$vs_low), x_ints()$vs_low, 0),
                                    size = line_size,
                                    color = line_col) +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(x_ints()$vs_high), x_ints()$vs_high, max_x),
                                    size = line_size,
                                    color = line_col) +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(x_ints()$s_high), x_ints()$s_high, max_x),
                                    size = line_size,
                                    color = line_col) +
                ggplot2::geom_vline(xintercept = ifelse(is.numeric(x_ints()$m_high), x_ints()$m_high, max_x),
                                    size = line_size,
                                    color = line_col) +
                
                # Add fitted data
                ggplot2::geom_line(data = params_fit(),
                                   mapping = ggplot2::aes(x = x,
                                                          y = y),
                                   size = 0.5) +
                
                # Add parametisation points
                ggplot2::geom_point(data = params(),
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
            
    })
    
    output$ploy_eq <- renderEq({
        
        equatiomatic::extract_eq(model(), use_coefs = TRUE, coef_digits = 4) 
        
    })
    
    output$model_r.squared <- renderEq({
    
        glue::glue("R.squared: {broom::glance(model())$r.squared}")
        
    })
    
    output$suitTable <- render_gt({
        
        df <- data.frame(x = c(0:320)) |>
            dplyr::mutate(
                suitability =
                    # Maybe it's not best to do this with case_when?
                    dplyr::case_when(
                        # Very suitable
                        x <= ifelse(is.na(x_ints()$vs_high), max_x, x_ints()$vs_high) & x >= ifelse(is.na(x_ints()$vs_low), 0, x_ints()$vs_low) ~ "Very suitable", ##
                        
                        # High suitable
                        x > x_ints()$vs_high & x <= ifelse(is.na(x_ints()$s_high), max_x, x_ints()$s_high) ~ "Suitable", ##
                        # Low suitable
                        x < x_ints()$vs_low & x >= ifelse(is.na(x_ints()$s_low), 0, x_ints()$s_low) ~ "Suitable", ##
                        
                        # High mildly suitable
                        x > x_ints()$s_high & x <= ifelse(is.na(x_ints()$m_high), max_x, x_ints()$m_high) ~ "Mildly suitable",
                        # Low mildly suitable
                        x < x_ints()$s_low & x >= ifelse(is.na(x_ints()$m_low), 0, x_ints()$m_low) ~ "Mildly suitable",
                        
                        # High unsuitable
                        x > x_ints()$m_high ~ "Unsuitable",
                        # Low unsuitable
                        x < x_ints()$m_low ~ "Unsuitable",
                        
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
