context("Utility functions:Air pressure from elevation")
test_that("airpressure_from_elev works as expected", {
  #101.3kPa at 0m elevation
  expect_equal(airpressure_from_elev(0),101.3)
  #89.8kPa at 1000m
  expect_lt(airpressure_from_elev(1000),90)
  #26.4kPa at 10000m
  expect_lt(airpressure_from_elev(10000),30)
})