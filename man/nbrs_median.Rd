% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbrvalues.R
\name{nbrs_median}
\alias{nbrs_median}
\title{Add median of neighbors values to SF object}
\usage{
nbrs_median(sf_object, unique_id_col, date_col, variable)
}
\arguments{
\item{sf_object}{An object of class 'SF'}

\item{unique_id_col}{A string identifying a unique column to serve as an identifier}

\item{date_col}{(Optional) A string identifying a variable for date, use if data is over multiple time periods.}

\item{variable}{A string identifying a numeric variable}
}
\value{
Column string of neighbors
}
\description{
Add median of neighbors values to SF object
}
\examples{
panel_example_sf <- nbrs_median(sf_object = panel_example_sf, unique_id_col = "country", variable = "gdp", date_col="year")
panel_example_sf <- nbrs_median(panel_example_sf, "country", "gdp", "year")
cross_section_example_sf <- nbrs_median(sf_object = cross_section_example_sf, unique_id_col = "country", variable = "gdp")
cross_section_example_sf <- nbrs_median(cross_section_example_sf, "country", "gdp")

}
