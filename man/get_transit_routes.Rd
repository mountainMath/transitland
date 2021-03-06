% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transitland.R
\name{get_transit_routes}
\alias{get_transit_routes}
\title{Get transit routes}
\usage{
get_transit_routes(params, get_all = FALSE)
}
\arguments{
\item{parms}{list of query parameters. Valid parameters are
bbox
location}

\item{paginate}{TRUE or FALSE, will try to retrieve all results if TRUE. If per_page is set it will iterate
through pages}
}
\description{
Get transit routes
}
\examples{
get_transit_routes(list(bbox=c(-122.4183,37.7758,-122.4120,37.7858)))
}
