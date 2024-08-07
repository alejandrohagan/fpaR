% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/divide.R
\name{divide}
\alias{divide}
\title{Divide function with error handling for divide by zero or NA}
\usage{
divide(
  .data,
  col_name,
  numerator,
  denominator,
  alternative_result = NA_integer_
)
}
\arguments{
\item{.data}{a tibble or DBI object}

\item{col_name}{new column name for result}

\item{numerator}{column for numerator}

\item{denominator}{column for denominator}

\item{alternative_result}{alternative results if divide results are inf or NA, must be numeric}
}
\value{
tibble or dbi object
}
\description{
A safe divide function that will catch info or NA values and return an alternative result
This is tibble or DBI friendly and will return same class as input
}
\examples{
divide(mtcars,div_col,mpg,0,10)
}
