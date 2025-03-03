% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_intelligence_fn.R
\name{qoq_fn}
\alias{qoq_fn}
\title{Quarter-over-quarter execution function}
\usage{
qoq_fn(x)
}
\arguments{
\item{x}{ti object}
}
\value{
dbi object
}
\description{
\code{qoq_fn()} is the function that is called by \code{qoq()} when passed through to \code{calculate()}
}
\details{
This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
by \code{calculated()}
This will return a dbi object that can converted to a tibble object with \code{dplyr::collect()}
}
\seealso{
\code{\link[=qoq]{qoq()}} for the function's intent
}
