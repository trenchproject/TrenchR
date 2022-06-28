context("utility functions")
  
expect_similar <- function(input, expected) {
  eval(bquote(expect_lt(abs(input - expected), 0.01)))
}


test_that("Temperature conversion functions work as expected", {

  expect_similar(kelvin_to_celsius(temperature = 270), -3.15)
  expect_similar(fahrenheit_to_kelvin(temperature = 85), 302.5944)
  expect_similar(fahrenheit_to_celsius(temperature = 85), 29.44444)
  expect_similar(celsius_to_kelvin(temperature = -10), 263.15)
})


test_that("angle conversion functions work as expected", {

  expect_similar(radians_to_degrees(0.831),  47.61279)
  expect_similar(degrees_to_radians(47.608), 0.8309164)

})

