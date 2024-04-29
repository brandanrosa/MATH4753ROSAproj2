#' myknot
#'
#' Calculates `r.squared` from the give knot value `x_k`
#'
#' @param x_k knot value
#' @param x explanatory variable
#' @param y response variable
#' @param data data frame
#'
#' @return `r.squared` from the give knot value `x_k`
#'
#' @importFrom stats lm
#'
#' @export
#'
#' @examples \dontrun{myknot(17, x=x, y=y, data)}
myknot <- function(x_k, x, y, data){
  df=within(data, {
    X<-(x-x_k)*(x>x_k)
  }
  )
  lmp=lm(y ~ x + X, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}
