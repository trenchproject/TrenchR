context("Day of the year")
test_that("day_of_year function works as expected", {
  # First day of the year
  dayofyear = day_of_year("2017-01-01", format= "%Y-%m-%d")
  expect_equal(dayofyear, 1)
  # Summer Equinox
  dayofyear =  day_of_year("2017-06-20", format= "%Y-%m-%d")
  expect_equal(dayofyear, 171)
  # Last day of the year
  dayofyear = day_of_year("2017-12-31", format= "%Y-%m-%d")
  expect_equal(dayofyear, 365)
})