context("Utility Functions::Solar noon")
test_that("solar_noon function works as expected", {
  # Vancouver - Solar noon in the month of June
  june10 = day_of_year("2017-06-10", format= "%Y-%m-%d")
  solar_noon_van = solar_noon(-123.117, june10)
  
  # Issue - Expect 12:12:02, but getting approx 11:45 am  
  # ^^ not sure if that's true. we would expect 1:11pm but getting 12:11pm because of the daylight saving?
  

})
