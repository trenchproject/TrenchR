context("Day length")
test_that("daylength function works as expected", {
  # Day Length - Norway - Should be at least 20 hours
  norway = day_of_year("2017-06-10", format= "%Y-%m-%d")
  norway_dl = daylength(58.88, norway)
  expect_gte(norway_dl,20)
  
  # Day Length - US (Seattle) - Should be at least 17 hours
  seattle = day_of_year("2017-06-10", format= "%Y-%m-%d")
  seattle_dl = daylength(47.43, seattle)
  expect_gte(seattle_dl,17)
  
  # TODO -
  # Day Length - Northern Sweden - Should be at least 23 hours
  # Currently an issue (to debug)
  # Warning message in acos((sin(6 * pi/180) + sin(lat) * sin(DecAng))/(cos(lat) * cos(DecAng))):
  #  “NaNs produced”
  
  sweden = day_of_year("2017-06-10", format= "%Y-%m-%d")
  sweden_dl = daylength(66.86, sweden)
  expect_gte(sweden_dl, 23)
  
})