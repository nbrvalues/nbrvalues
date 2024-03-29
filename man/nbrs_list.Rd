% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbrvalues.R
\name{nbrs_list}
\alias{nbrs_list}
\title{Add list of neighbors to SF object}
\usage{
nbrs_list(sf_object, unique_id_col)
}
\arguments{
\item{sf_object}{An object of class 'SF'}

\item{unique_id_col}{A unique column to serve as an identifier}
}
\value{
String column with all neighbors
}
\description{
Add list of neighbors to SF object
}
\examples{
example_sf <- nbrs_list(sf_object = example_sf, unique_id_col = "country")
example_sf <- nbrs_list(example_sf, "country")

}
