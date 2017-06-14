#' Calculate Zenith angle
#'
#' @details This function allows you to calculate zenith angle in degrees
#' @param J is Julian day.
#' @param lat is latitude.
#' @param lon is longitude.
#' @param Hr is hour of the day.
#' @keywords zenith
#' @export
#' @examples
#' \dontrun{
#' zenith()
#' }
zenith=function(J, lat, lon, Hr){
# J is Julian day
#lat and lon and latitude and longitude, degrees
#retuns zenith angle in degrees

rd=180/pi;  # factor to convert radians into degrees

RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + J))); # Revolution angle in radians
DecAng = asin(0.39795 * cos(RevAng));                          # Declination angle in radians           
  
f=(279.575+0.9856*J)/rd;  # f in radians
ET= (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(3*f)-12.7*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600;   #(11.4) Equation of time

LC= 1/15* (15 - lon%%15); # longitude correction, 1/15h for each degree e of standard meridian
t_0  =12-LC-ET; # solar noon 

latr = lat/rd; #latitude in radians
             Daylength = 24 - (24 / pi) * acos((sin(6 * pi / 180) + sin(latr) * sin(DecAng)) / (cos(latr) * cos(DecAng)));
             
cpsi_rad= sin(DecAng)*sin(latr) + cos(DecAng)*cos(latr)*cos(pi/12*(Hr-t_0)); #zenith angle in radians
psi=acos(cpsi_rad); #(11.1) zenith angle in radians

psi= psi*rd

return(psi)
}

#' Calculate Zenith in radians...
#'
#' This function allows you to calculate zenith in radians
#' @param J is Julian day.
#' @param lat is latitude.
#' @param lon is longitude.
#' @param Hr is hour of the day.
#' @keywords zenith radian
#' @export
#' @examples
#' \dontrun{
#' zenith.rad()
#' }

zenith.rad=function(J, lat, lon, Hr){
# J is Julian day
#lat and lon and latitude and longitude, degrees
#retuns zeniht angle in radians

rd=180/pi;  # factor to convert radians into degrees

RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + J))); # Revolution angle in radians
DecAng = asin(0.39795 * cos(RevAng));                          # Declination angle in radians           
  
f=(279.575+0.9856*J)/rd;  # f in radians
ET= (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(3*f)-12.7*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600;   #(11.4) Equation of time

LC= 1/15* (15 - lon%%15); # longitude correction, 1/15h for each degree e of standard meridian
t_0  =12-LC-ET; # solar noon 

latr = lat/rd; #latitude in radians
             Daylength = 24 - (24 / pi) * acos((sin(6 * pi / 180) + sin(latr) * sin(DecAng)) / (cos(latr) * cos(DecAng)));
             
cpsi_rad= sin(DecAng)*sin(latr) + cos(DecAng)*cos(latr)*cos(pi/12*(Hr-t_0)); #zenith angle in radians
psi=acos(cpsi_rad); #(11.1) zenith angle in radians

return(psi)
}

#------------------------------------
#' Estimate air pressure in kPa (Kilo Pascal)
#' Credit - #http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html
#'
#' @details Estimate air pressure (kPa) as a function of elevation
#' @description  This function allows you to calculate estimated air pressure (kPa) as afunction of elevation
#' @param h height in meters.
#' @keywords air pressure
#' @export
#' @examples
#' \dontrun{
#' airpressure_elev()
#' }
airpressure_elev<- function(h){  #H is height in meters 
  p= 101325* (1 - 2.25577*10^(-5)*h)^5.25588       
  p= p/1000 #convert to kPa
  return(p)
}

#--------------------------------------
#TEMPERATURE UTILITY FUNCTIONS

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