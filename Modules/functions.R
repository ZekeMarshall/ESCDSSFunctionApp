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