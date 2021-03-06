% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_map.R
\name{eq_create_label}
\alias{eq_create_label}
\title{Popup annotation text}
\usage{
eq_create_label(data, annot_col = "DATE")
}
\arguments{
\item{data}{Dataset of earthquakes. For cleaning use \code{\link{eq_clean_data}}}

\item{annot_col}{Use this parameter for popup annotation with specific varaible}
}
\value{
Leaflet map
}
\description{
Creates HTML for annotation popup text
}
\details{
\code{\link{eq_map}} uses this function
}
\examples{
\dontrun{
d<-dataset_noaa_raw
d<-dplyr::filter(eq_clean_data(d), COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
eq_map(d)
For Date annotation popup
eq_map(d, annot_col="DATE")
}
}
