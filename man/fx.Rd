% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{fx}
\alias{fx}
\title{Foreign Exchange Rates Dataset}
\format{
A data frame with multiple rows and 4 columns:
\describe{
\item{date}{\code{Date}. The date for which the exchange rate is recorded.}
\item{from_currency}{\code{character}. The source currency code (e.g., "USD").}
\item{to_currency}{\code{character}. The target currency code (e.g., "EUR").}
\item{exchange}{\code{numeric}. The exchange rate from \code{from_currency} to \code{to_currency}.}
}
}
\source{
Generated from \code{fpaR::fx}
}
\usage{
fx
}
\description{
This dataset contains foreign exchange rates between different currencies for specific dates.
}
\examples{
\dontrun{
data(fx)
head(fx)
summary(fx)
}
}
\keyword{datasets}
