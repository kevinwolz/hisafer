% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define.R
\name{check_accepted}
\alias{check_accepted}
\title{Check validity of Hi-sAFe accepted values}
\usage{
check_accepted(variable, exp.plan)
}
\arguments{
\item{variable}{A character string of the name of the variable to check.}

\item{exp.plan}{The exp.plan of a "hip" object.}
}
\value{
An error message or empty character stirng.
}
\description{
Checks validity of Hi-sAFe inputs accepted values found in the package param_defs.txt file
Used within \code{\link{check_input_values}}.
}
\keyword{internal}
