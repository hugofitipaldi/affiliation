% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocode_functions.R
\name{geocode_nominatim}
\alias{geocode_nominatim}
\title{Aggregation of queries to find country name based on address}
\usage{
geocode_nominatim(
  search_query,
  country = NULL,
  language = "en",
  fields = "coordinates",
  email
)
}
\arguments{
\item{search_query}{free-text (affiliation field)}

\item{email}{your email to access the openstreet API}
}
\value{
This function returns a data frame with searched query, latitude, longitude and assigned country
}
\description{
Queries openstreet API to find latitude, longitude and country based on address
}
\details{
This function was designed to internal use in the package. It uses free-text affiliation field as query for open street API
}
\author{
Hugo Fitipaldi
}
