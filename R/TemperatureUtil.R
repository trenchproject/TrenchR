#' Converts Fahrenheit to Kelvin
#' 
#' (credit  https://swcarpentry.github.io)
#' 
#' @details This function allows you to convert temperature from Fahrenheit to Kelvin.
#' @param temp Temperature in Fahrenheit.
#' @keywords Fahrenheit Kelvin
#' @export
#' @examples
#' fahrenheit_to_kelvin(85)
#' 

fahrenheit_to_kelvin <- function(temp) {
  kelvin <- ((temp - 32) * (5/9)) + 273.15
  kelvin
}
#' Converts Kelvin to Celsius
#' 
#' (credit  https://swcarpentry.github.io)
#' 
#' @details This function allows you to convert temperature from Kelvin to Celsius.
#' @param temp Temperature in Fahrenheit.
#' @keywords Celsius Kelvin
#' @export
#' @examples
#' kelvin_to_celsius(270)
#' 

kelvin_to_celsius <- function(temp) {
  Celsius <- temp - 273.15
  Celsius
}
#' Converts Fahrenheit to Celsius
#' 
#' (credit  https://swcarpentry.github.io)
#' 
#' @details This function allows you to convert temperature from Fahrenheit to Celsius
#' @param temp Temperature in Fahrenheit.
#' @keywords Fahrenheit Celsius
#' @export
#' @examples
#' fahrenheit_to_celsius(85)
#' 

fahrenheit_to_celsius <- function(temp) {
  temp_k <- fahrenheit_to_kelvin(temp)
  result <- kelvin_to_celsius(temp_k)
  result
}