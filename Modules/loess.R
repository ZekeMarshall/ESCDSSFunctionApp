# Loess

# Create parameters
params <- data.frame(x = c(input$x1,
                           input$x2,
                           input$x3,
                           input$x4,
                           input$x5,
                           input$x6#,
                           # input$x7,
                           # input$x8,
                           # input$x9
                          ),
                          y = c(input$y1,
                                input$y2,
                                input$y3,
                                input$y4,
                                input$y5,
                                input$y6#,
                                # input$y7,
                                # input$y8,
                                # input$y9
                          )
                     )

params_fit <- loess(y~x, params)

xl <- seq(0,320, (320/1000))

smoothed_fit <- data.frame(x = xl,
                           y = predict(params_fit, xl)) |> 
  dplyr::mutate(
    y = dplyr::case_when(
      y > 1 ~ 1,
      TRUE ~ as.numeric(y)
    )
  )


fit_plot <- ggplot2::ggplot() +
  
  ggplot2::geom_point(data = params,
                      mapping = ggplot2::aes(x = x, 
                                             y = y,
                                             color = "red",
                                             size = 10)) +
  
  ggplot2::geom_line(data = smoothed_fit,
                     mapping = ggplot2::aes(x = x,
                                            y = y),
                     size = 0.5) +
  
  ggplot2::geom_hline(yintercept = 0.25,
                      size = 0.5,
                      color = "grey") +
  
  ggplot2::geom_hline(yintercept = 0.50,
                      size = 0.5,
                      color = "grey") +
  
  ggplot2::geom_hline(yintercept = 0.75,
                      size = 0.5,
                      color = "grey") +
  
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

fit_plot