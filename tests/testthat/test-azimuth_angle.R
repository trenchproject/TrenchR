context("Utility Functions::Azimuth Angle")
test_that("azimuth_angle function works as expected", {
  # Vancouver - Approx Solar Noon
  june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")
  aa_van = azimuth_angle(june10, 49.25, -123.119, 12)
  
  expect_gt(aa_van, 6)
  expect_lt(aa_van, 7)
  #Expect 173.73, but getting (6.08)  - good
  #Vancouver is GMT-8
  
  
  # Seattle - Approx Solar Noon
  aa_sea = azimuth_angle(june10, 47.606, -122.331, 12)
  
  #Expect 175.11 getting 4.73  - good
  
  # Sydney - Approx Solar Noon
  aa_syd = azimuth_angle(june10, -33.87, 151.217, 12)
  
  #Expect 358.5 getting 178.5
  
  # Paris
  aa_par = azimuth_angle(june10, 48.87, 2.667, 12)
  
  # Expect 155.14 getting 5.99
  
  # Beijing
  aa_bei = azimuth_angle(june10, 39.9075, 116.397, 12)
  
  # 169.19 getting 10.7 - good
  
  # saudi
  aa_sau = azimuth_angle(june10, 24.633, 46.7167, 12)
  
  # 226 getting 46
})


june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")
aa = azimuth_angle(june10, 49.267, -123.117, 12)
aa_sea
aa_van
