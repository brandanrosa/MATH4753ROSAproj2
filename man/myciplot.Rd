% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myciplot.R
\name{myciplot}
\alias{myciplot}
\title{myciplot}
\usage{
myciplot(x, alpha = 0.05, ...)
}
\arguments{
\item{x}{the linear model `lm()` of a data set}

\item{alpha}{alpha value}

\item{...}{passes arguments to additional functions}
}
\value{
A histogram, a residual diagnostic plot, and a named list with everything you could ever wish for
}
\description{
This function performs a SLR analysis on the `lm()` of data set
}
\examples{
\dontrun{myciplot(x=ylm, alpha=0.05)}
}
