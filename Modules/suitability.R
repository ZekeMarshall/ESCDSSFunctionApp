# Establishes UI module function
suitUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    withMathJax(), # Initialize mathJax so the equation renders properly
    
    box(title = "Suitability Function Generator",
        width = 9,
        height = 580,
        plotOutput(outputId = ns("suitPlot")),
        collapsible = FALSE
    ),
    
    box(title = "Options",
        id = ns("options"),
        # style = '.card-body {overflow-y: scroll;}',
        width = 3,
        height = 580,
        
        tags$div(
          tags$style(HTML(".card-body {overflow-y: scroll;}") # Close HTML
                     ) # Close tags$style
          ),# Close tags$div
        
        # tags$div(
        #   tags$style("#md.id-options.card.shiny-bound-input {overflow-y: scroll;}") # Close tags$style
        # ),# Close tags$div
        
        # tags$style("#md.id-options.card.shiny-bound-input {overflow-y: scroll;}"),
        
        
        textInput(inputId = ns("species"),
                  label = "Species",
                  value = "Eucalyptus glaucescens"),
        
        numericInput(inputId = ns("poly_num"),
                     label = "Polynomial Order:",
                     value = 4),
        
        fluidRow(
          column(6,
                 # tags$h5(tags$b("X Value")),
                 tags$b("X Value"),
                 numericInput(inputId = ns("x1"),
                              label = NULL,
                              value = 0, width = "99%"),
                 numericInput(inputId = ns("x2"),
                              label = NULL,
                              value = 20, width = "99%"),
                 numericInput(inputId = ns("x3"),
                              label = NULL,
                              value = 60),
                 numericInput(inputId = ns("x4"),
                              label = NULL,
                              value = 90),
                 numericInput(inputId = ns("x5"),
                              label = NULL,
                              value = 120),
                 numericInput(inputId = ns("x6"),
                              label = NULL,
                              value = 140),
                 numericInput(inputId = ns("x7"),
                              label = NULL,
                              value = 160),
                 numericInput(inputId = ns("x8"),
                              label = NULL,
                              value = 180),
                 numericInput(inputId = ns("x9"),
                              label = NULL,
                              value = 200),
                 numericInput(inputId = ns("x10"),
                              label = NULL,
                              value = 230),
                 numericInput(inputId = ns("x11"),
                              label = NULL,
                              value = 260),
                 numericInput(inputId = ns("x12"),
                              label = NULL,
                              value = 290),
                 numericInput(inputId = ns("x13"),
                              label = NULL,
                              value = 320)
          ),
          column(6,
                 # tags$h5(tags$b("Y Value")),
                 tags$b("Y Value"),
                 numericInput(inputId = ns("y1"),
                              label = NULL,
                              value = 1,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y2"),
                              label = NULL,
                              value = 1,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y3"),
                              label = NULL,
                              value = 1,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y4"),
                              label = NULL,
                              value = 1,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y5"),
                              label = NULL,
                              value = 1,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y6"),
                              label = NULL,
                              value = 1,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y7"),
                              label = NULL,
                              value = 1,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y8"),
                              label = NULL,
                              value = 1,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y9"),
                              label = NULL,
                              value = 1,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y10"),
                              label = NULL,
                              value = 0.85,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y11"),
                              label = NULL,
                              value = 0.6,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y12"),
                              label = NULL,
                              value = 0.3,
                              step = 0.1,
                              max = 1,
                              min = 0),
                 numericInput(inputId = ns("y13"),
                              label = NULL,
                              value = 0,
                              step = 0.1,
                              max = 1,
                              min = 0)
                 
          ) # Close score column
            
        ), # Close fluidRow
        collapsible = FALSE
        # ) # Closes style div
      ),
    
    
      withMathJax(), # Initialize mathJax so the equation renders properly
    
      box(title = NULL,
          width = 12,
          fluidRow(
            column(8,
                   eqOutput(outputId = ns("ploy_eq"))
            ),
            column(4,
                   eqOutput(outputId = ns("model_r.squared")))
          ),
          # header = NULL,
          collapsible = FALSE
      ),
    
      box(title = "Suitability Data",
          width = 12,
          gt_output(outputId = ns("suitTable")),
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

}

# Establishes the server module function
suit <- function(input, output, session, max_x, suit_factor) {
  
  # default_scores <- default_scores |> 
  #   dplyr::filter("Suitability.factor" == suit_factor)
  
   
  # # These observe events update the variables for selection
  # observeEvent(input$species,  {
  #     req(input$species)
  # 
  #     updateNumericInput(session,
  #                        inputId = "x1",
  #                        choices = 
  #                          dplyr::filter(default_scores,
  #                                        species == input$species) |>
  #                          )
  #     })
  

  params <- reactive({
    
    params <- data.frame(x = c(input$x1,
                               input$x2,
                               input$x3,
                               input$x4,
                               input$x5,
                               input$x6,
                               input$x7,
                               input$x8,
                               input$x9,
                               input$x10,
                               input$x11,
                               input$x12,
                               input$x13
                            
                               ), # Close x values
                         y = c(input$y1,
                               input$y2,
                               input$y3,
                               input$y4,
                               input$y5,
                               input$y6,
                               input$y7,
                               input$y8,
                               input$y9,
                               input$y10,
                               input$y11,
                               input$y12,
                               input$y13
                               ) # Close y values
                
                ) # Close data frame
    
    params <- params |>
      dplyr::filter(!is.na(x)) |> 
      dplyr::filter(!is.na(y))
    
    return(params)
    
    
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
  
  
  
  
  
  
  output$suitPlot <- renderPlot(height = 520, {
    
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
      ggplot2::geom_hline(yintercept = 0.3,
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
      ggplot2::coord_cartesian(xlim = c(0, (max_x*1.05)),
                               ylim = c(0,1.1),
                               expand = FALSE) +
      ggplot2::ggtitle(label = input$species) +
      ggplot2::scale_x_continuous(breaks = seq(0,max_x,20)) +
      ggplot2::scale_y_continuous(breaks = seq(0,1,0.1)) +
      ggplot2::xlab(label = suit_factor) +
      ggplot2::ylab(NULL) +
      ggplot2::theme_classic(base_size = 16) +
      ggplot2::theme(legend.position = "none") +
      NULL
    
    fit_plot
    
  })
  
  output$ploy_eq <- renderEq({
    
    equatiomatic::extract_eq(model(), use_coefs = TRUE, coef_digits = 4) 
    
  })
  
  output$model_r.squared <- renderEq({
    
    glue::glue("R.squared: {broom::glance(model())$r.squared}")
    
  })
  
  output$suitTable <- render_gt({
    
    df <- data.frame(x = c(0:max_x)) |>
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
      dplyr::filter(x %in% seq(0, max_x, 20))
    
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


