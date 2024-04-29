#' myknot
#'
#' Calculates the `Rsquared` value for a given `xk`
#'
#' @param xk chosen knot value
#' @param x explanatory variable
#' @param y response variable
#'
#' @return `Rsquared` value
#' @export
#'
#' @examples \dontrun{myknot(18,x,y)}
myknot = function(xk, x, y){
  df = within(spruce.df, X<-(BHDiameter-xk)*(BHDiameter>xk))
  lmp = lm(Height ~ BHDiameter + X, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}
