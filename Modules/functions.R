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