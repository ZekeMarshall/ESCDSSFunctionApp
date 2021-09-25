# Establishes UI module function
suitUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    withMathJax(), # Initialize mathJax so the equation renders properly
    
    box(title = "Suitability Function Generator",
        width = 9,
        height = 540,
        plotOutput(outputId = ns("suitPlot")),
        collapsible = FALSE
    ),
    
    box(title = "Options",
        id = ns("options"),
        # style = '.card-body {overflow-y: scroll;}',
        width = 3,
        height = 540,
        
        tags$div(
          tags$style(HTML(".card-body {overflow-y: scroll;}") # Close HTML
                     ) # Close tags$style
          ),# Close tags$div
        
        # tags$div(
        #   tags$style("#md.id-options.card.shiny-bound-input {overflow-y: scroll;}") # Close tags$style
        # ),# Close tags$div
        
        # tags$style("#md.id-options.card.shiny-bound-input {overflow-y: scroll;}"),
        
        # tags$style("#md.id-options {overflow-y: scroll;}"),
        
        
        selectInput(inputId = ns("species"),
                    label = "Species",
                    choices = species),
        
        selectInput(inputId = ns("suit_factor"),
                    label = "Suitability Factor:",
                    choices = c(`Moisture Deficit` = "md",
                                `Accumulated Temperature` = "at",
                                `Continentality` = "ct",
                                `Exposure` = "dams",
                                `Soil Moisture Regime` = "smr",
                                `Soil Nutrient Regime` = "snr")),
        
        selectInput(inputId = ns("model_type"),
                    label = "Model Type:",
                    choices = c(`Polynomial` = "poly",
                                `Logarithmic` = "log")),
        
        numericInput(inputId = ns("poly_num"),
                     label = "Polynomial Order:",
                     value = 2),
        
        checkboxInput(inputId = ns("fit_plot"),
                      label = "Display Model",
                      value = FALSE),
        
        checkboxInput(inputId = ns("range01"),
                      label = "Range 0-1",
                      value = FALSE),
        
        fluidRow(
          
          column(6,
                 downloadButton(outputId = ns("downmodeldata"),
                                label = "Model Data",
                                class = "dlButton")),
          column(6,
                 downloadButton(outputId = ns("downmodel"),
                                label = "Model",
                                class = "dlButton"))
          
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
          collapsible = FALSE)
    )

}

# Establishes the server module function
suit <- function(input, output, session, suit_factor, species) {
  
  max_x <- reactive({
    if(input$suit_factor == "md"){
      max_x_val <- 320
    } else if(input$suit_factor == "at"){
      max_x_val <- 3000
    } else if(input$suit_factor == "ct"){
      max_x_val <- 12
    } else if(input$suit_factor == "dams"){
      max_x_val <- 24
    } else if(input$suit_factor == "smr"){
      max_x_val <- 8
    } else if(input$suit_factor == "snr"){
      max_x_val <- 6
    }
    
    return(max_x_val)
    
  })
  
  step <- reactive({
    if(input$suit_factor == "md"){
      step <- 0.5
    } else if(input$suit_factor == "at"){
      step <- 5
    } else if(input$suit_factor == "ct"){
      step <- 0.01
    } else if(input$suit_factor == "dams"){
      step <- 0.01
    } else if(input$suit_factor == "smr"){
      step <- 0.01
    } else if(input$suit_factor == "snr"){
      step <- 0.01
    }
    
    return(step)
    
  })
  
  params <- reactive({
    
    params <- tidy_scores_v1 |>
      
      dplyr::filter(factor == input$suit_factor,
                    species == input$species) |> 
      
      dplyr::rename("x" = "value",
                    "y" = "score")
    
    return(params)
    
    })

  
  
  
  # Create a polynomial model to fit suitability score parameters
  model <- reactive({
    
    if(input$model_type == "poly"){
      
      order <- input$poly_num
      
      lm(data = params(),
         y ~ poly(x, as.numeric(order)))
      
    } else if(input$model_type == "log"){
      
      
      
    }
    
  })
  
  # Fitted model data
  modelled_data <- reactive({
    
    max_x <- as.numeric(max_x())
    step <- as.numeric(step())
    
    modelled_data <- data.frame(x = seq(0, max_x, step),
                                y = predict(object = model(), data.frame(x = seq(0, max_x, step))))
    
    if(input$range01 == TRUE){
      
      modelled_data <- modelled_data |> 
        dplyr::mutate(
          y = dplyr::case_when(
            y > 1 ~ 1,
            y < 0 ~ 0,
            TRUE ~ as.numeric(y)
          )
        )
      
      
    } else if(input$range01 == FALSE){
      
      modelled_data <- modelled_data
      
    }
    
    return(modelled_data)
    
  })
  
  # Fitted model data
  modelled_data_xints <- reactive({
    
    max_x <- as.numeric(max_x())
    step <- as.numeric(step())
    
    modelled_data_xints <- data.frame(x = seq(0, max_x, step),
                                      y = predict(object = model(), data.frame(x = seq(0, max_x, step))))
    
    return(modelled_data_xints)
    
  })
  
  # x intercept values
  
  x_ints <- reactive({
    
    get_x_intercepts(fitted_data = modelled_data_xints(), x = x, y = y)
    
  })
  
  
  
  
  
  
  output$suitPlot <- renderPlot(height = 520, {
    
    max_x <- as.numeric(max_x())
    step <- as.numeric(step())
    
    if(input$fit_plot == TRUE){
      
      fit_plot <- ggplot2::ggplot() +
        
        # Unsuitable area fill
        shade_curve(df = modelled_data(),
                    x = x,
                    y = y,
                    alpha = 1,
                    zstart = ifelse(x_ints()$lines.0_low < x_ints()$vs_low & x_ints()$lines.0_low > 0, x_ints()$lines.0_low, 0),
                    zend = ifelse(is.na(x_ints()$m_low), 0, x_ints()$m_low),  
                    fill = u_col) +
        shade_curve(df = modelled_data(),
                    x = x,
                    y = y,
                    alpha = 1,
                    zstart = ifelse(is.na(x_ints()$m_high), max_x, x_ints()$m_high),
                    zend = ifelse(is.na(x_ints()$lines.0_high), max_x, x_ints()$lines.0_high),
                    fill = u_col) +
        
        # Mildly suitable area fill
        shade_curve(df = modelled_data(),
                    x = x,
                    y = y,
                    alpha = 1,
                    zstart = ifelse(is.na(x_ints()$m_low), 0, x_ints()$m_low),
                    zend = ifelse(is.na(x_ints()$s_low), 0, x_ints()$s_low),
                    fill = m_col) +
        shade_curve(df = modelled_data(),
                    x = x,
                    y = y,
                    alpha = 1,
                    zstart = ifelse(is.na(x_ints()$s_high), max_x, x_ints()$s_high),
                    zend = ifelse(is.na(x_ints()$m_high), max_x, x_ints()$m_high),
                    fill = m_col) +
        
        # Suitable area fill
        shade_curve(df = modelled_data(),
                    x = x,
                    y = y,
                    alpha = 1,
                    zstart = ifelse(is.na(x_ints()$s_low), 0, x_ints()$s_low),
                    zend = ifelse(is.na(x_ints()$vs_low), 0, x_ints()$vs_low),
                    fill = s_col) +
        shade_curve(df = modelled_data(),
                    x = x,
                    y = y,
                    alpha = 1,
                    zstart = ifelse(is.na(x_ints()$vs_high), max_x, x_ints()$vs_high),
                    zend = ifelse(is.na(x_ints()$s_high), max_x, x_ints()$s_high),
                    fill = s_col) +
        
        # Very suitable area fill
        shade_curve(df = modelled_data(),
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
        ggplot2::geom_line(data = modelled_data(),
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
                                 ylim = c(0,1.15),
                                 expand = FALSE) +
        ggplot2::ggtitle(label = input$species) +
        ggplot2::scale_x_continuous(breaks = seq(0,max_x,max_x/16)) +
        ggplot2::scale_y_continuous(breaks = seq(0,1,0.1)) +
        ggplot2::xlab(label = input$suit_factor) +
        ggplot2::ylab(NULL) +
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(legend.position = "none") +
        NULL
      
      fit_plot
      
    } else if(input$fit_plot == FALSE){
      
      fit_plot <- ggplot2::ggplot() +
      
        # Add parametisation points
        ggplot2::geom_point(data = params(),
                            mapping = ggplot2::aes(x = x,
                                                   y = y,
                                                   size = 0.5)) +
        
        
        
        # Plot options
        ggplot2::coord_cartesian(xlim = c(0, (max_x*1.05)),
                                 ylim = c(0,1.15),
                                 expand = FALSE) +
        ggplot2::ggtitle(label = input$species) +
        ggplot2::scale_x_continuous(breaks = seq(0,max_x,max_x/16)) +
        ggplot2::scale_y_continuous(breaks = seq(0,1,0.1)) +
        ggplot2::xlab(label = input$suit_factor) +
        ggplot2::ylab(NULL) +
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(legend.position = "none") +
        NULL
      
      fit_plot
      
      
      
    }
    
  })
  
  output$ploy_eq <- renderEq({
    
    equatiomatic::extract_eq(model(), use_coefs = TRUE, coef_digits = 4) 
    
  })
  
  output$model_r.squared <- renderEq({
    
    glue::glue("R.squared: {broom::glance(model())$r.squared}")
    
  })
  
  output$suitTable <- render_gt({
    
    max_x <- as.numeric(max_x())
    step <- as.numeric(step())
    
    df <- data.frame(x = seq(0, max_x, step)) |>
      dplyr::mutate(
        suitability =
          # Maybe it's not best to do this with case_when?
          dplyr::case_when(
            # Very suitable
            x <= ifelse(is.na(x_ints()$vs_high), max_x, x_ints()$vs_high) & x >= ifelse(is.na(x_ints()$vs_low), 0, x_ints()$vs_low) ~ "VS", ##
            
            # High suitable
            x > x_ints()$vs_high & x <= ifelse(is.na(x_ints()$s_high), max_x, x_ints()$s_high) ~ "S", ##
            # Low suitable
            x < x_ints()$vs_low & x >= ifelse(is.na(x_ints()$s_low), 0, x_ints()$s_low) ~ "S", ##
            
            # High mildly suitable
            x > x_ints()$s_high & x <= ifelse(is.na(x_ints()$m_high), max_x, x_ints()$m_high) ~ "MS",
            # Low mildly suitable
            x < x_ints()$s_low & x >= ifelse(is.na(x_ints()$m_low), 0, x_ints()$m_low) ~ "MS",
            
            # High unsuitable
            x > x_ints()$m_high ~ "U",
            # Low unsuitable
            x < x_ints()$m_low ~ "U",
            
            TRUE ~ as.character(x)
          )
      )
    
    
    
    # Create data frame
    df_table <- df |>
      dplyr::filter(x %in% seq(0, max_x, max_x/16))
    
    df_table_t <- df_table |>
      data.table::transpose() |>
      janitor::row_to_names(1)
    
    # Create table
    table <- gt::gt(df_table_t) |>
      gt::data_color(
        columns = gt::everything(),
        color = scales::col_factor(
          palette = c(vs_col, s_col, m_col, u_col),
          levels = c("VS", "S", "MS", "U")
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
  
  
  output$downmodel <- downloadHandler(

    filename = function() {
      
      paste0("ESCDSSModel", " - ", input$suit_factor, " - ", input$species, ".rds", sep="")
      
    },

    content = function(file) {
      
      model <- model()
      
      saveRDS(object = model, file = file)
      
    }

  )
  
  output$downmodeldata <- downloadHandler(
    
    filename = function() {
      
      paste0("ESCDSSModelData", " - ", input$suit_factor, " - ", input$species, ".csv", sep="")
      
    },
    
    content = function(file) {
      
      params_fit <- params_fit()
      
      write.csv(x = params_fit, file = file)
      
    }
    
  )
  
  
  
  
}


