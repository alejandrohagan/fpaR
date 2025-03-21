% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_intelligence_fn.R
\name{pytd_fn}
\alias{pytd_fn}
\title{Previous year-to-date execution function}
\usage{
pytd_fn(x)
}
\arguments{
\item{x}{ti object}
}
\value{
dbi object
}
\description{
\code{\link[=pytd_fn]{pytd_fn()}} is the function that is called by \code{\link[=pytd]{pytd()}} when passed through to \link{calculate}
}
\details{
This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
by \link{calculate}
This will return a dbi object that can converted to a tibble object with \code{\link[dplyr:compute]{dplyr::collect()}}
}
\seealso{
\code{\link[=pytd]{pytd()}} for the function's intent
}
