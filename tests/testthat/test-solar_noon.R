context("Utility Functions::Solar noon")
test_that("solar_noon function works as expected", {
  # Vancouver - Solar noon in the month of June
  june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")
  solar_noon_van = solar_noon(-123.117, june10)
  
  # we would expect 12:11:57 ~= 12.2
  expect_gt(solar_noon_van, 12.1)
  expect_lt(solar_noon_van, 12.3)
  
  # Sydney - Solar noon in the month of January
  jan15 = day_of_year("2018-01-15", format= "%Y-%m-%d")
  solar_noon_syd = solar_noon(151.217, jan15)
  
  # we would exppect 12:04:13 ~= 12.07
  expect_gt(solar_noon_syd, 12.06)
  expect_lt(solar_noon_syd, 12.08)
})
