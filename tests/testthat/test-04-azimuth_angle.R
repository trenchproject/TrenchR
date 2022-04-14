context("Azimuth Angle")

expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}

test_that("azimuth_angle function works as expected", {
  # Vancouver - Approx Solar Noon expect 6.08
  june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")
  aa_van = azimuth_angle(june10, 49.25, -123.119, 12)
  
  expect_gt(aa_van, 6)
  expect_lt(aa_van, 7)
  
  # Sydney - Approx Solar Noon Expect 181.5
  aa_syd = azimuth_angle(june10, -33.87, 151.217, 12, 10)
  
  expect_gt(aa_syd, 181)
  expect_lt(aa_syd, 182)

  # Beijing expect 10.8
  aa_bei = azimuth_angle(june10, 39.9075, 116.397, 12)
  
  expect_gt(aa_bei, 10)
  expect_lt(aa_bei, 11)
  
  # saudi expect 314
  aa_sau = azimuth_angle(june10, 24.633, 46.7167, 12)
  
  expect_gt(aa_sau, 313)
  expect_lt(aa_sau, 315)
  
  # Madrid offset necessary since timezone is completely off from what it is supposed to be. (expect 47.6)
  aa_mad = azimuth_angle(june10, 40.4167, -3.703, 12, 1)
  
  expect_gt(aa_mad, 47)
  expect_lt(aa_mad, 48)

   expect_similar(azimuth_angle(doy    = 112, 
                 lat    = 47.61, 
                 lon    = -122.33, 
                 hour   = 0, 
                 offset = -8), 107.8561)

})
