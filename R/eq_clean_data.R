#' Clean dataset obtained from the U.S. National Oceanographic and Atmospheric Administration (NOAA)
#'
#'  Function takes raw dataset as data.frame which contains DATE, YEAR, MONTH, DAY, LATITUDE, LONGITUDE,
#'  TOTAL_DEATHS,EQ_PRIMARY,LOCATION_NAME varaibles.
#'  Dataset can be obtained from NOAA's
#' \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{Significant
#' EarthQuake Database}
#'
#' @param raw dataset fo earthquakes at data.frame format
#'
#' @return Returns cleaned and formatted data.frame
#' @export
#' @importFrom dplyr mutate select one_of
#' @importFrom tidyr unite
#' @importFrom lubridate days
#' @importFrom magrittr %>%
#' @examples
#' #dataset is a name of file with NOAA data
#' d<-readr::read_delim(dataset, delim = "\t")
#' eq_clean_data(d)
#'
eq_clean_data<-function(raw) {
  #Raplace NA by 01 for events with missing month and days data
  na.replace <- function (x) {
    x[is.na(x)] <- "01"
    return(x)
  }
  bc = ISOdate(0000,01,01)
  raw$MONTH<-na.replace(raw$MONTH)
  raw$DAY<-na.replace(raw$DAY)
  cleaned <- raw %>%
    dplyr::mutate(date2=ifelse(YEAR > 0, 0, (ISOdate(abs(YEAR), MONTH, DAY)-bc)))%>%
    tidyr::unite(DATE, YEAR, MONTH, DAY, sep="_")%>%
    dplyr::mutate(DATE = as.Date(DATE, "%Y_%m_%d"))%>%
    dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE))%>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE))%>%
    dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS))%>%
    dplyr::mutate(DATE = ifelse(is.na(DATE), as.Date(bc-lubridate::days(date2)+lubridate::days(1)), DATE))%>%
    dplyr::mutate(DATE = as.Date(DATE, origin="1970-01-01"))%>%
    dplyr::select(-dplyr::one_of("date2"))
}


#' Location clean for dataset obtained from the U.S. National Oceanographic and Atmospheric Administration (NOAA)
#'
#' Dataset contains varaible LOCATION_NAME that should be trimmed. This function deletes names of countries and regions which
#' in LOCATION_NAME placed before ":"
#'
#'
#' @param raw dataset fo earthquakes at data.frame format
#'
#' @return dataframe with trimmed LOCATION_NAME field
#' @export
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_title str_replace_all
#' @importFrom magrittr %>%
#' @examples
#' #dataset is a name of file with NOAA data
#' readr::read_delim(dataset, delim = "\t") %>%
#' eq_location_clean
eq_location_clean <- function(raw){
  cleaned <- raw%>%
    dplyr::mutate(LOCATION_NAME = stringr::str_to_title(stringr::str_replace_all(raw$LOCATION_NAME, ".*:", "")))
}

