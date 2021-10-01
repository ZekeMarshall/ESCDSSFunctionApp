# Define shading function
# Adapted from: https://sebastiansauer.github.io/shade_Normal_curve/
shade_curve <- function(df, x, y, fill, zstart, zend, alpha){
  ggplot2::geom_area(data = dplyr::filter(df, 
                                          x >= zstart,
                                          x <= zend),
                     ggplot2::aes(y = y,
                                  x = x), 
                     fill = fill,
                     color = "transparent", 
                     alpha = alpha)
}

# https://stackoverflow.com/questions/52650467/how-to-estimate-x-value-from-y-value-input-after-approxfun-in-r?noredirect=1&lq=1
RootNonlinearInterpolant <- function (x, y, f, y0 = 0) {
  if (is.unsorted(x)) {
    ind <- order(x)
    x <- x[ind]; y <- y[ind]
  }
  z <- y - y0
  k <- which(z[-1] * z[-length(z)] < 0)
  nk <- length(k)
  xk <- numeric(nk)
  F <- function (x) f(x) - y0
  for (i in 1:nk) xk[i] <- uniroot(F, c(x[k[i]], x[k[i] + 1]))$root
  xk
}

# Get suitability ranges from x intercept values
get_x_intercepts <- function(fitted_data, x = x, y = y) {

    # Find the y intercept values
    y0=0
    f <- splinefun(fitted_data$x, fitted_data$y)
    lines.0 <- RootNonlinearInterpolant(fitted_data$x, fitted_data$y, f, y0)
    
    if(length(lines.0) > 1){
      lines.0_low <- min(lines.0)
      lines.0_high <- max(lines.0)
    } else if(length(lines.0) == 1 & lines.0 < median(fitted_data$x)){
      lines.0_low <- lines.0
      lines.0_high <- NA
    } else if(length(lines.0) == 1 & lines.0 > median(fitted_data$x)){
      lines.0_low <- NA
      lines.0_high <- lines.0
    
    }
    
    # Find the very suitable range first as these values can be used to determine
    # whether the other values are low or high if only one exists.
    y.75 = 0.75
    f.75 <- splinefun(fitted_data$x, fitted_data$y)
    lines.75 <- RootNonlinearInterpolant(fitted_data$x, fitted_data$y, f.75, y.75)
    
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
    y.3 = 0.3
    f.3 <- splinefun(fitted_data$x, fitted_data$y)
    lines.3 <- RootNonlinearInterpolant(fitted_data$x, fitted_data$y, f.3, y.3)
    
    if(length(lines.3) > 1){
      m_low <- min(lines.3)
      m_high <- max(lines.3)
    } else if(lines.3 >= vs_high){
      m_low <- NA
      m_high <- lines.3
    } else if(lines.3 <= vs_low){
      m_low <- lines.3
      m_high <- NA
    }
    
    # Find the suitable range values
    y.5 = 0.5
    f.5 <- splinefun(fitted_data$x, fitted_data$y)
    lines.5 <- RootNonlinearInterpolant(fitted_data$x, fitted_data$y, f.5, y.5)
    
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
    
    x_intercepts <- data.frame(lines.0_low, lines.0_high, m_low, m_high, s_low, s_high, vs_low, vs_high)
    
    return(x_intercepts)
    
}

# Extract an lm model formula as a character string
extract_model_equation <- function(model) {
  
  intercept <- paste(model$coefficients[[1]][[1]])
  coefficients <- paste(model$coefficients[2:length(model[[1]])])
  
  equation <- as.character()
  
  for (i in 1:length(coefficients)) {
    
    equation <- paste(equation, coefficients[i], "*x^", i, " + ", sep = "")
    
  }
  
  equation <- gsub('.{3}$', '', equation)
  
  equation <- paste(intercept, '+', equation)
  
  return(equation)
  
}