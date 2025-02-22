% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abc.R
\name{abc}
\alias{abc}
\title{Classify a group by proportion of a variable (A,B,C)}
\usage{
abc(.data, ..., dim, a = 0.7, b = 0.26, c = 0.04, func = c("sum"))
}
\arguments{
\item{.data}{tibble or DBI object}

\item{...}{columns to group by}

\item{dim}{dimension to classify by (should be additive and positive)}

\item{a}{initial segment threshold expressed as a percentage eg .7}

\item{b}{incremental segment threshold expressed as a percentage eg .26}

\item{c}{final segment threshold expressed as a percentage eg .04}

\item{func}{either "sum" or "n"; sum to sum your dim or n for row count}
}
\value{
a tibble or DBI object
}
\description{
This returns a table that will segment your data into A,B or C segments based on custom
thresholds
}
\examples{

customer_abc <- abc(fpaR::sales,customer_key,dim=quantity,a=.7,b=.2,c=.1)
}
