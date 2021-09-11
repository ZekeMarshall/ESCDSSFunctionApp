# Create parameters
params <- data.frame(x = c(0,
                           80,
                           120,
                           200,
                           240,
                           320
                      ),
                      y = c(0.2,
                            0.4,
                            0.6,
                            1,
                            0.6,
                            0.4
                      )
                      )

model <- lm(data = params,
            y ~ poly(x, 3)) #input$poly_num

params_fit <- data.frame(x = seq(0:320), 
                         y = predict(object = model, data.frame(x = seq(0:320))))



y0=0.5
f <- splinefun(params_fit$x, params_fit$y)
lines.5 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)

s_low <- lines.5[1]
s_high <- lines.5[2]

y0=0.25
f <- splinefun(params_fit$x, params_fit$y)
lines.25 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)

m_low <- lines.25[1]
m_high <- lines.25[2]

y0=0.75
f <- splinefun(params_fit$x, params_fit$y)
lines.75 <- RootNonlinearInterpolant(params_fit$x, params_fit$y, f, y0)

vs_low <- lines.75[1]
vs_high <- lines.75[2]



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
  ggplot2::geom_vline(xintercept = m_low,
                      size = 0.25,
                      color = "grey") +
  ggplot2::geom_vline(xintercept = s_low,
                      size = 0.25,
                      color = "grey") +
  ggplot2::geom_vline(xintercept = vs_low,
                      size = 0.25,
                      color = "grey") +
  ggplot2::geom_vline(xintercept = vs_high,
                      size = 0.25,
                      color = "grey") +
  ggplot2::geom_vline(xintercept = s_high,
                      size = 0.25,
                      color = "grey") +
  ggplot2::geom_vline(xintercept = m_high,
                      size = 0.25,
                      color = "grey") +
  
  # Unsuitable area fill
  shade_curve(df = params_fit,
              x = x,
              y = y,
              alpha = 0.5,
              zstart = 0, 
              zend = m_low, 
              fill = "red") +
  shade_curve(df = params_fit,
              x = x,
              y = y,
              alpha = 0.5,
              zstart = 320, 
              zend = 320, 
              fill = "red") +
  
  # Mildly suitable area fill
  shade_curve(df = params_fit,
              x = x,
              y = y,
              alpha = 0.5,
              zstart = m_low, 
              zend = s_low, 
              fill = "orange") +
  shade_curve(df = params_fit,
              x = x,
              y = y,
              alpha = 0.5,
              zstart = s_high, 
              zend = 320, 
              fill = "orange") +
  
  # Suitable area fill
  shade_curve(df = params_fit,
              x = x,
              y = y,
              alpha = 0.5,
              zstart = s_low, 
              zend = vs_low, 
              fill = "lightgreen") +
  shade_curve(df = params_fit,
              x = x,
              y = y,
              alpha = 0.5,
              zstart = vs_high, 
              zend = s_high, 
              fill = "lightgreen") +
  
  # Very suitable area fill
  shade_curve(df = params_fit,
              x = x,
              y = y,
              alpha = 0.5,
              zstart = vs_low, 
              zend = vs_high, 
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
  # ggplot2::ggtitle(label = input$species) +
  ggplot2::scale_x_continuous(breaks = seq(0,320,20)) +
  ggplot2::scale_y_continuous(breaks = seq(0,1,0.1)) +
  # ggplot2::xlab(label = subtitle) +
  ggplot2::ylab(NULL) +
  ggplot2::theme_classic(base_size = 16) +
  ggplot2::theme(legend.position = "none") +
  NULL

fit_plot

plotly::ggplotly(fit_plot, height = 600)
