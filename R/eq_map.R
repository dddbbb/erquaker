#' Map of Significant Earthquakes
#'
#' Creates map with circle points of earthqakes with popup annotation
#'
#' @param data Dataset of earthquakes. For cleaning use \code{\link{eq_clean_data}}
#' @param annot_col Use this parameter for popup annotation with specific varaible
#'
#' @return Leaflet map
#' @export
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom magrittr %>%
#' @examples
#' dataset_noaa_raw %>%
#' eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map()
#'   # For Date annotation popup
#'   eq_map(annot_col="DATE")
#'
eq_map <- function(data, annot_col="popup text"){
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = data, radius = data$EQ_PRIMARY+2,
                     lng = ~ LONGITUDE, lat = ~ LATITUDE,
                     popup = ~ eq_create_label(data, annot_col), weight = 2)

}


#' Popup annotation text
#'
#' Creates HTML for annotation popup text
#'
#' \code{\link{eq_map}} uses this function
#'
#' @param data Dataset of earthquakes. For cleaning use \code{\link{eq_clean_data}}
#' @param annot_col Use this parameter for popup annotation with specific varaible
#'
#' @return Leaflet map
#' @export
#' @examples
#' d<-dataset_noaa_raw
#' d<-dplyr::filter(eq_clean_data(d), COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
#' eq_map(d)
#' # For Date annotation popup
#' eq_map(d, annot_col="DATE")
#'
eq_create_label<-function(data, annot_col = "DATE"){
  data<-eq_location_clean(data)
  if(annot_col == "popup text"){
    paste("<b>Location:</b> ",
          data$LOCATION_NAME,
          ifelse(is.na(data$EQ_PRIMARY), "", paste("<br><b>Magnitude: </b>", data$EQ_PRIMARY)),
          ifelse(is.na(data$TOTAL_DEATHS), "", paste("<br><b>Total deaths: </b>",
                                                     data$TOTAL_DEATHS)))
  }
  else {
    data[[annot_col]]
  }
}
