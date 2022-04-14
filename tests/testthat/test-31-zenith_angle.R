context("Zenith Angle")
test_that("zenith_angle function works as expected", {
  # Vancouver - Approx Solar Noon
  june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")
  za_van = zenith_angle(june10, 49.267, -123.117, 12)
  
  # Expect cos(za) to be 0.8966
  expect_gt(cos(za_van*pi/180), 0.895)
  expect_lt(cos(za_van*pi/180), 0.897)

  # Seattle - Approx Solar Noon
  za_sea = zenith_angle(june10, 47.606, -122.331, 12)
  
  # Expect cos(za) to be 0.9093
  expect_gt(cos(za_sea*pi/180), 0.90)
  expect_lt(cos(za_sea*pi/180), 0.91)
  
  # Madrid - Approx Solar Noon
  za_mad = zenith_angle(june10, 40.4167, -3.703, 12, 1)
  
  # Expect cos(za) to be 0.9179
  expect_gt(cos(za_mad*pi/180), 0.917)
  expect_lt(cos(za_mad*pi/180), 0.919)
})
