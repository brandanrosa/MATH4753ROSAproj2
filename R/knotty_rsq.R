#' knotty_rsq
#'
#' Calculates the knot value `xk` which maximizes `Rsquared`
#'
#' @param df data frame
#' @param nval used for the length of the `seq()` and `vector()` functions
#'
#' @return a knot value `xk` which maximizes `Rsquared`
#' @export
#'
#' @examples \dontrun{knotty_rsq(df,nval=1000)}
knotty_rsq <- function(df, nval = 1000) {

  diam <- spruce.df$BHDiameter
  height <- spruce.df$Height

  x <- seq(min(diam), max(diam), length = nval)
  y <- vector(mode = "numeric", length = nval)

  for(i in 1:nval){
    y[i] <- myknot(x[i], x = spruce.df$BHDiameter, y = spruce.df$Height)
  }

  nmax <- which.max(y)

  plot(x, y, type = "l",lwd = 2, col = "blue", ylab = "Rsquared", xlab = "xk", main = "Rsquared ~ xk")
  abline(v = x[nmax], lwd = 3, col = "maroon")
  text(x = x[nmax] - 0.5,
       y = 0.68,
       srt=90,
       paste0(round(x[nmax], 4)))
  x[nmax]
}
