[![Travis-CI Build Status](https://travis-ci.org/dddbbb/erquaker.svg?branch=master)](https://travis-ci.org/dddbbb/erquaker)
## Erquaker package

This erquaker package provides functions for cleaning and visualizing NOAA Significant earthquakes. Dataset can be obtained from NOAA's https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
It contains:
 - eq_clean_data() - Function takes raw dataset as data.frame which contains DATE, YEAR, MONTH, DAY, LATITUDE, LONGITUDE,         TOTAL_DEATHS,EQ_PRIMARY,LOCATION_NAME varaibles.
 - eq_location_clean() - function deletes names of countries and regions which in dataset$LOCATION_NAME placed before ":"
 - geom_timeline() - Function plots timeline with circle point of earthquakes
 - geom_timeline_label() - Function plots Labels for earthquakes
 - eq_map() - Creates map with circle points of earthqakes with popup annotation
 - eq_create_label()  - Creates HTML for annotation popup text
