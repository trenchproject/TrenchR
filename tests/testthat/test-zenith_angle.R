context("Utility Functions::Zenith Angle")
test_that("zenith_angle function works as expected", {
  # Vancouver - Approx Solar Noon
  june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")
  za = zenith_angle(june10, 49.267, -123.117, 12)
  
  # Need proper counter test
  

})
