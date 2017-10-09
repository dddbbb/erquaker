context("Mapping")
d<-eq_clean_data(dataset_noaa_raw)
d<-dplyr::filter(d, COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
d<-eq_map(d)

testthat::test_that('eq_map produce leaflet object',{
  testthat::expect_that(d, is_a('leaflet'))
})
d<-eq_create_label(eq_clean_data(dataset_noaa_raw))
testthat::test_that('There is a Date type by default', {
  testthat::expect_that(d, is_a('Date'))
})
