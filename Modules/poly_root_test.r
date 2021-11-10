# Set axis min and max values
max_x <- 320

# Create parameters
params <- data.frame(x = c(20,
                           60,
                           90,
                           120,
                           140,
                           160,
                           180,
                           230,
                           260,
                           290
                      ),
                      y = c(1,
                            1,
                            1,
                            1,
                            0.9,
                            0.8,
                            0.7,
                            0.4,
                            0.2,
                            0.1
                      )
                      )




model <- lm(data = params,
            y ~ poly(x, 3, raw = T))

# model <- lm(data = params,
#             y ~ poly(x, 3))

params_fit <- data.frame(x = seq(0:max_x), 
                         y = predict(object = model, 
                                     data.frame(x = seq(0:max_x))))

################################################################################
# Find intercept values
################################################################################

# Find the y intercept values
y0 <- 0
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
y0 <- 0.75
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
y0 <- 0.25
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
y0 <- 0.5
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

################################################################################
# Plot fitted model
################################################################################



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
                           ylim = c(0, 1.1),
                           expand = FALSE) +
  # ggplot2::ggtitle(label = input$species) +
  ggplot2::scale_x_continuous(breaks = seq(0, max_x, 20)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  # ggplot2::xlab(label = subtitle) +
  ggplot2::ylab(NULL) +
  ggplot2::theme_classic(base_size = 16) +
  ggplot2::theme(legend.position = "none") +
  NULL

fit_plot

################################################################################
# Create table
################################################################################


df <- data.frame(x = c(0:320)) |>
  dplyr::mutate(
    suitability =
      dplyr::case_when(
        # Very suitable
        x <= ifelse(is.na(vs_high), max_x, vs_high) & x >= ifelse(is.na(vs_low), 0, vs_low) ~ "Very suitable",
        x > ifelse(is.na(vs_high), max_x, vs_high) & x <= ifelse(is.na(s_high), max_x, s_high) ~ "Suitable",
        x < ifelse(is.na(vs_low), 0, vs_low) & x >= ifelse(is.na(s_low), 0, s_low) ~ "Suitable",
        x < ifelse(is.na(s_low), 0, s_low) & x >= ifelse(is.na(m_low), 0, m_low) ~ "Mildly suitable",
        x > ifelse(is.na(s_high), max_x, s_high) & x <= ifelse(is.na(m_high), max_x, m_high) ~ "Mildly suitable",
        x < ifelse(is.na(m_low), 0, m_low) ~ "Unsuitable",
        x > ifelse(is.na(m_high), max_x, m_high) ~ "Unsuitable",
        TRUE ~ as.character(x)
      )
  )


max_x <- 320
step <- 16
df <- params_fit

# Create data frame
df_table <- df |>
  dplyr::filter(x %in% seq(0, max_x, max_x/16)) |> 
  dplyr::mutate(y = round(y, digits = 2))

df_table_t <- df_table |>
  data.table::transpose() |>
  janitor::row_to_names(1)



# Create table

colnames <- colnames(df_table_t)

table <- gt::gt(df_table_t) |>
  
  gt::tab_style(
    style = cell_fill(color = "red"),
    locations = cells_body(
      columns = `20`,
      rows = `20` < 1
    )
  )


# Print table
table

################################################################################
# Get x intercepts
################################################################################

foo <- get_x_intercepts(fitted_data = params_fit)

################################################################################
# Extract equation
################################################################################


broom::tidy(model)

foo <- broom::glance(model)

eq <- equatiomatic::extract_eq(model = model, use_coefs = TRUE)

eq2 <- eq |> 
  stringr::str_remove_all(pattern = c("[\\(\\)\\{\\}]")) |> 
  stringr::str_remove_all(pattern = stringr::fixed("\\operatorname")) |> 
  stringr::str_remove_all(pattern = stringr::fixed("\\widehat"))

eq3 <- parse(text = eq2)


################################################################################
# Log Model
###############################################################################

model <- lm(data = params,
            y ~ 0.01^x)

model <- nls(y ~ SSasymp(x, y), data = params)
# 
# nls(y ~ stats::SSasymp(), data = params)

params_fit <- data.frame(x = seq(0:max_x), 
                         y = predict(object = model, data.frame(x = seq(0:max_x))))



fit_plot <- ggplot2::ggplot() +
  
  # Add fitted data
  ggplot2::geom_line(data = params_fit,
                     mapping = ggplot2::aes(x = x,
                                            y = y),
                     size = 0.5) +

  ggplot2::geom_point(data = params,
                      mapping = ggplot2::aes(x = x,
                                             y = y,
                                             size = 0.5)) +
  NULL

fit_plot

