% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_intelligence_classes.R
\name{qtdopq}
\alias{qtdopq}
\title{Current period quarter-to-date over previous period quarter}
\usage{
qtdopq(.data, .date, .value, calendar_type, lag_n = 1)
}
\arguments{
\item{.data}{tibble or dbi object (either grouped or ungrouped)}

\item{.date}{the date column to group by}

\item{.value}{the value column to summarize}

\item{calendar_type}{select either 'standard' or '5-5-4' calendar, see 'Details' for additional information}

\item{lag_n}{the number of periods to lag}
}
\value{
ti object
}
\description{
A short description...
}
\details{
\itemize{
\item This function creates a complete calendar object that fills in any missing days, weeks, months, quarters, or years
\item If you provide a grouped object with \code{\link[dplyr:group_by]{dplyr::group_by()}}, it will generate a complete calendar for each group
\item The function creates a \code{ti} object, which pre-processes the data and arguments for further downstream functions
}

\strong{standard calendar}
\itemize{
\item The standard calendar splits the year into 12 months (with 28–31 days each) and uses a 7-day week
\item It automatically accounts for leap years every four years to match the Gregorian calendar
}

\strong{5-5-4 calendar}
\itemize{
\item The 5-5-4 calendar divides the fiscal year into 52 weeks (occasionally 53), organizing each quarter into two 5-week periods and one 4-week period.
\item This system is commonly used in retail and financial reporting
}
}
\examples{
qtdopq(sales,.date=order_date,.value=quantity,calendar_type='standard',lag_n=1)
}
\seealso{
Other time_intelligence: 
\code{\link{atd}()},
\code{\link{dod}()},
\code{\link{mom}()},
\code{\link{momtd}()},
\code{\link{mtd}()},
\code{\link{mtdopm}()},
\code{\link{pmtd}()},
\code{\link{pqtd}()},
\code{\link{pwtd}()},
\code{\link{pytd}()},
\code{\link{qoq}()},
\code{\link{qoqtd}()},
\code{\link{qtd}()},
\code{\link{wow}()},
\code{\link{wowtd}()},
\code{\link{wtd}()},
\code{\link{wtdopw}()},
\code{\link{yoy}()},
\code{\link{yoytd}()},
\code{\link{ytd}()},
\code{\link{ytdopy}()}
}
\concept{time_intelligence}
