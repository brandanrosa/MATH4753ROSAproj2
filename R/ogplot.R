#' ogplot
#'
#' The scatterplot of the data with fitted piecewise regression model
#'
#' @param xk knot value
#'
#' @return a plot of the data with fitted piecewise regression model
#' @export
#'
#' @examples \dontrun{ogplot(xk = 18)}
ogplot <- function(xk) {

  coeff2 <- function(xk){
    df=within(spruce.df,
              {
                X <-(BHDiameter-xk)*(BHDiameter>xk)
              }
    )
    lmp <- lm(Height ~ BHDiameter + X , data=df)
    tmp <- summary(lmp)
    coef(lmp)
  }
  myf3 = function(x, xk, coef){
    coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
  }

  with(spruce.df,plot(BHDiameter,
                      Height,
                      pch = 21,
                      cex=1.5,
                      bg="hotpink",
                      col = "darkgreen",
                      main="Height ~ BHDiameter: Piecewise Regression"
  ))
  cf <- coeff2(xk)
  ff <- myf3(x=spruce.df$BHDiameter, xk, coef = cf)
  curve(myf3(x, xk, coef = cf), add = TRUE, lwd=3, col="darkgreen")

  l <- list(coeff = cf, myf33 = ff)
  invisible(l)
}
