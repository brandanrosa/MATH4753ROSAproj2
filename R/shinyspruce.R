#' shinyspruce
#'
#' This Shiny App investigates a piecewise regression analyses on plots based on the Spruce data set.
#'
#' @return two plots
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples \dontrun{shinyspruce()}
shinyspruce <- function() {
  runApp(system.file("shinyspruce",
                     package = "MATH4753ROSAproj2"),
         launch.browser = TRUE)
}
