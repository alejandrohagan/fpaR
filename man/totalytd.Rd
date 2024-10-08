% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_intelligence.R
\name{totalytd}
\alias{totalytd}
\title{Total year to date values}
\usage{
totalytd(.data, ..., date_var, value_var)
}
\arguments{
\item{.data}{tibble of values}

\item{...}{optional columns to group by}

\item{date_var}{column with date var to aggregate by}

\item{value_var}{column with value to aggregate}
}
\value{
tibble
}
\description{
Total year to date values
}
\examples{
totalytd(fpaR:sales,date_var = order_date,value_var = quantity)
}
