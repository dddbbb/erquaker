context("Cleaning dataset")
d<-eq_clean_data(dataset_noaa_raw)
testthat::test_that('Dataset format correctness', {
  testthat:: expect_is(d$DATE, 'Date')
  testthat::expect_is(d$LATITUDE, 'numeric')
  testthat::expect_is(d$LONGITUDE, 'numeric')
  testthat::expect_is(d$TOTAL_DEATHS, 'numeric')
  testthat::expect_is(d$EQ_PRIMARY, 'numeric')
})

d<-eq_location_clean(d)
testthat::test_that('Location name has changed', {
  f<-d$LOCATION_NAME[1]!=dataset_noaa_raw$LOCATION_NAME[1]
  testthat::expect_that(f, is_true())
})
