% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_intelligence.R
\name{yoy_dbi}
\alias{yoy_dbi}
\title{Year over year values for DBI objects}
\usage{
yoy_dbi(.data, ..., date_var, value_var, lag_n = 1, time_unit = "day")
}
\arguments{
\item{.data}{tibble of values}

\item{...}{optional columns to group by}

\item{date_var}{column with date var to aggregate by}

\item{value_var}{column with value to aggregate}

\item{lag_n}{number of time units to lagg}

\item{time_unit}{to return aggregate data by 'day','week','month','quarter' or 'year'}
}
\description{
Year over year values for DBI objects
}
