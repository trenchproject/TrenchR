context("Utility Functions::Azimuth Angle")
test_that("azimuth_angle function works as expected", {
  # Vancouver - Approx Solar Noon
  june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")
  aa = azimuth_angle(june10, 49.267, -123.117, 12)
  
  #Expect 173.84, but getting (4.839)
  #Vancouver is GMT-8
  

})
