#' myciplot
#'
#' This function performs a SLR analysis on the `lm()` of data set
#'
#' @param x the linear model `lm()` of a data set
#' @param alpha alpha value
#' @param ... passes arguments to additional functions
#'
#' @return A histogram, a residual diagnostic plot, and a named list with everything you could ever wish for
#'
#' @importFrom stats qchisq confint rnorm sd dnorm lowess
#' @importFrom graphics hist plot curve lines
#' @importFrom grDevices rgb
#'
#' @export
#'
#' @examples \dontrun{myciplot(x=ylm, alpha=0.05)}
myciplot <- function(x, alpha=0.05, ...) {
  smx <- summary(x)

  bh0 <- x$coefficients[1]
  bh0

  bh1 <- x$coefficients[2]
  bh1

  a <- smx$sigma
  sig2 <- a^2

  mycisig2 <- function(x, alpha=0.05) { # x = ylm
    n = length(x$residuals)

    L <- (n-2) * sig2 / qchisq(alpha/2, df = n-2, lower.tail = FALSE)
    U <- (n-2) * sig2 / qchisq(1-alpha/2, df = n-2, lower.tail = FALSE)
    ci <- c(L,U)
    ci
  }

  cisig2 <- mycisig2(x)
  cisig2

  cib <- confint(x)
  cib

  fitted <- x$fitted.values
  fitted

  residuals <- x$residuals
  residuals

  # Histogram with curve
  h1 <- hist(x = residuals, plot = FALSE)
  r1 <- h1$density/max(h1$density)
  hist(residuals, col = rgb(r1, r1^2, 0), freq = FALSE,
       main = "Distribution of Residuals")

  x <- rnorm(1000, mean = mean(residuals), sd = sd(residuals))
  curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), -60, 60, add=T, col="hotpink", lwd = 3)

  plot(x = fitted, y = residuals, pch = 21, col = "black", cex = 1.5, bg = "green", main = "Diagnostic Plot")
  lines(lowess(x = fitted, y = residuals, ...), col = "red", lwd = 2)

  ll <- list(bh0=bh0, bh1=bh1, sig2=sig2, cisig2=cisig2, cib=cib, fitted=fitted, residuals=residuals)
  ll
}
