% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize multiple years of accident data from the fars database into a data frame tbl}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A list of character strings giving the desired years to read from the fars database}
}
\value{
This function returns a data frame tbl (tbl_df) which summarizes the number of accidents for each month and for each year requested.
}
\description{
This is a simple function that takes a list of years as character string input,
reads the appropriate accident data files from the US National Highway Traffic Safety Administration's
Fatality Analysis Reporting System (fars) for the requested years into a data.frame and then
summarizes the accident data by month and by year.  The function has no error conditions,
but an invalid year request will cause a warning to be issued by the fars_read_years function.
}
\examples{
\dontrun{accidentSummary <- fars_summarize_years(c("2014","2015"))}

}
