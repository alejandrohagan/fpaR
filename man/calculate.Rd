% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{calculate}
\alias{calculate}
\title{Calculate}
\arguments{
\item{x}{segment object}
}
\value{
dbi object

dbi object
}
\description{
Calculate

Calculate
}
\examples{
x <- ytd(sales,.date=order_date,.value=quantity,calendar_type="standard")
calculate(x)
sales |>
    group_by(store_key) |>
    abc(category_values = c(.3,.5,.75,.85)) |>
    calculate() |>
}
