context("Solar noon")
test_that("solar_noon function works as expected", {
  # Vancouver - Solar noon in the month of June
  june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")
  solar_noon_van = solar_noon(-123.117, june10, -8)
  
  # we would expect 12:11:57 ~= 12.2
  expect_gt(solar_noon_van, 12.1)
  expect_lt(solar_noon_van, 12.3)
  
  # Sydney - Solar noon in the month of January (daylight saving time applied)
  jan15 = day_of_year("2018-01-15", format= "%Y-%m-%d")
  solar_noon_syd = solar_noon(151.217, jan15, 11)
  
  # we would expect 13:04:13 ~= 13.07
  expect_gt(solar_noon_syd, 13.06)
  expect_lt(solar_noon_syd, 13.08)
  
  # Madrid - on June 10th daylight saving
  solar_noon_mad = solar_noon(-3.703, june10, 1)
  
  # we would expect 13:14 ~= 13.23
  expect_gt(solar_noon_mad, 13.2)
  expect_lt(solar_noon_mad, 13.3)
})
