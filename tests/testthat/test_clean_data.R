library(testthat)
library(erquaker)
context("Cleaning dataset")
d<-eq_clean_data(dataset_noaa_raw)
test_that('Dataset format correctness', {
  expect_is(d$DATE, 'Date')
  expect_is(d$LATITUDE, 'numeric')
  expect_is(d$LONGITUDE, 'numeric')
  expect_is(d$TOTAL_DEATHS, 'numeric')
  expect_is(d$EQ_PRIMARY, 'numeric')
})

d<-eq_location_clean(d)
test_that('Location name had changed', {
  f<-d$LOCATION_NAME[1]!=dataset_noaa_raw$LOCATION_NAME[1]
  expect_that(f, is_true())
})
