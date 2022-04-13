context("Declination Angle")
test_that("dec_angle function works as expected", {
  # Winter Solstice (Should be negative around -23.45 degrees , approx -0.41 radians)
  winsol = day_of_year("2017-12-21", format= "%Y-%m-%d")
  dec_angle_win_sols = dec_angle(winsol)
  expect_lte(dec_angle_win_sols, -.40)
  # Summer Equinox (Should be positive 23.45 degrees , approx 0.41 radians))
  summerequi =  day_of_year("2017-06-20", format= "%Y-%m-%d")
  expect_gte(summerequi, .40)
  
})