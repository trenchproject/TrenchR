#' Calculate day of the passed date
#' 
#'  
#' @details Calculate day of the year
#'
#' @description This function allows you to calculate day of year from text specifying a date
#' @param day day
#' @param format date format following "POSIXlt" conventions 
#' @return day number, for eg. 1 for January 1st
#' @keywords day
#' @export
#' @examples
#' \dontrun{
#' day_of_year("2017-04-22", format= "%Y-%m-%d")
#' }

day_of_year<- function(day, format="%Y-%m-%d"){
  day=  as.POSIXlt(day, format=format)
  return(as.numeric(strftime(day, format = "%j")))
}

#' Calculate solar declination in radians
#' 
#'  
#' @details Calculate solar declination in radians
#'
#' @description This function allows you to calculate solar declination, which is the angular distance of the sun north or south of the earth’s equator, based on the day of year
#' @param doy day of year
#' @return declination angle in radians
#' @keywords Declination angle
#' @export
#' @examples
#' \dontrun{
#' dec_angle(112)
#' }

dec_angle <- function(doy){
  RevAng = 0.21631 + 2 * atan (0.967 * tan (0.0086 * (-186 + doy))) # Revolution angle in radians, calculated per day
  DecAng = asin (0.39795 * cos (RevAng))                            # Declination angle in radians  
  return(DecAng)
}

#' Calculate day length
#' 
#' 
#' @details Calculate day length
#'
#' @description This function allows you to calculate daylength in hours as a function of latitude and day of year. Uses the CMB model (Forsythe et al. 1995).
#' @param lat latitude in degrees
#' @param doy day of year
#' @return hours 
#' 
#' @keywords Day length
#' @export
#' @examples
#' \dontrun{
#' daylength(47.61, 112)
#' }

daylength <- function(lat, doy){
  lat=lat*pi/180 #convert degrees to radians
  RevAng = 0.21631 + 2 * atan (0.967 * tan (0.0086 * (-186 + doy))) # Revolution angle in radians, calculated per day
  DecAng = asin (0.39795 * cos (RevAng))                            # Declination angle in radians  
  Daylength = 24 - (24 / pi) * acos ((sin (6 * pi / 180) + sin (lat) * sin (DecAng)) / (cos (lat) * cos (DecAng))) #hours of daylight
  return(Daylength)
}

#' Calculate time of solar noon
#' 
#' 
#' @description Calculate time of solar noon
#'
#' @details This function allows you to calculate the time of solar noon in hours as a function of the day of year and longitude
#' @param lon longitude in degrees 
#' @param doy day of year
#' @return time at solar noon
#' @keywords Solar noon time
#' @export
#' @examples
#' \dontrun{
#' solar_noon(-122.335, 112)
#' }

solar_noon <- function(lon, doy){
  # Calculate the time of solar noon for each day using longitude correction (LC), equation of time (ET), and a conversion (f)
  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f=f*pi/180 #convert f in degrees to radians
  ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
  LC= 1/15* (15 - lon%%15) # longitude correction, 1/15h for each degree of standard meridian
  t_0 = 12-LC-ET # solar noon
  return(t_0)
}

#' Calculate Zenith Angle
#' 
#' 
#' @details Calculate Zenith angle
#'
#' @description This function allows you to calculate the zenith angle, the location of the sun as an angle (in degrees) measured from vertical
#' @param doy is day of year.
#' @param lat is latitude in degrees.
#' @param lon is longitude in degrees.
#' @param hour is hour of the day.
#' @return Zenith angle in degrees
#' @keywords Zenith angle
#' @export
#' @examples
#' \dontrun{
#' zenith_angle(112, 47.61, -122.33, 12)
#' }

zenith_angle=function(doy, lat, lon, hour){

lat=lat*pi/180 #to radians
lon=lon*pi/180 #to radians
  
RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
DecAng = asin(0.39795 * cos(RevAng));                            # Declination angle in radians           
  
f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
f=f*pi/180 #convert f in degrees to radians
ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
LC= 1/15* (15 - lon%%15) # longitude correction, 1/15h for each degree e of standard meridian
t_0 = 12-LC-ET # solar noon
Daylength = 24 - (24 / pi) * acos ((sin (6 * pi / 180) + sin (lat) * sin (DecAng)) / (cos (lat) * cos (DecAng))) #hours of daylight
             
cos.zenith= sin(DecAng)*sin(lat) + cos(DecAng)*cos(lat)*cos(pi/12*(hour-t_0)); #cos of zenith angle in radians
zenith=acos(cos.zenith)*180/pi # zenith angle in degrees
if (zenith>90) zenith=90 # if measured from the vertical psi can't be greater than pi/2 (90 degrees)

return(zenith)
}

#' Calculate Azimuth angle
#' 
#' 
#' @details Calculate azimuth angle
#'
#' @description This function allows you to calculate the azimuth angle, the angle (in degrees) measured from true north or south measured in the horizontal plane. The azimuth angle is measured with respect to due south, increasing in the counter clockwise direction so 90 degrees is east.
#' @param doy is day of year.
#' @param lat is latitude in degrees.
#' @param lon is longitude in degrees.
#' @param hour is hour of the day.
#' @return Azimuth angle in degrees
#' @keywords Azimuth angle
#' @export
#' @examples
#' \dontrun{
#' azimuth_angle(112, 47.61, -122.33, 12)
#' }

azimuth_angle=function(doy, lat, lon, hour){
  
  lat=lat*pi/180 #to radians
  lon=lon*pi/180 #to radians
  
  RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
  DecAng = asin(0.39795 * cos(RevAng));                          # Declination angle in radians           
  
  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f=f*pi/180 #convert f in degrees to radians
  ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
  LC= 1/15* (15 - lon%%15) # longitude correction, 1/15h for each degree e of standard meridian
  t_0 = 12-LC-ET # solar noon
  Daylength = 24 - (24 / pi) * acos ((sin (6 * pi / 180) + sin (lat) * sin (DecAng)) / (cos (lat) * cos (DecAng))) #hours of daylight
  
  cos.zenith= sin(DecAng)*sin(lat) + cos(DecAng)*cos(lat)*cos(pi/12*(hour-t_0)); #cos of zenith angle in radians
  zenith=acos(cos.zenith) # zenith angle in radians
  if (zenith>pi/2) zenith=pi/2 # if measured from the vertical psi can't be greater than pi/2 (90 degrees)
  
  cos.azimuth= -(sin(DecAng)-cos(zenith)*sin(lat) )/ (cos(lat)*sin(zenith))
  azimuth= acos(cos.azimuth)*180/pi #azimuth angle in degrees
  
  return(azimuth)
}

#' Estimate air pressure in kPa (Kilo Pascal)
#' 
#' Credit - #http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html
#'
#' @details Estimate air pressure (kPa) as a function of elevation
#' @description  This function allows you to calculate estimated air pressure (kPa) as a function of elevation
#' @param elev elevation in meters.
#' @keywords Air Pressure
#' @return Air pressure in kPa
#' @export
#' @examples
#' \dontrun{
#' airpressure_elev(1500)
#' }
airpressure_elev<- function(elev){  
  p= 101325* (1 - 2.25577*10^(-5)*elev)^5.25588       
  p= p/1000 #convert to kPa
  return(p)
}

#UNIT CONVERSIONS

#' Converts Fahrenheit to Kelvin
#' 
#' (credit  https://swcarpentry.github.io)
#' 
#' @details This function allows you to convert temperature from Fahrenheit to Kelvin.
#' @param temp Temperature in Fahrenheit.
#' @keywords Fahrenheit Kelvin
#' @return Temperature in Kelvin
#' @export
#' @examples
#' \dontrun{
#' fahrenheit_to_kelvin(85)
#' }

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
#' @return Temperature in Celsius
#' @export
#' @examples
#' \dontrun{
#' kelvin_to_celsius(270)
#' }

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
#' @return Temperature in Celsius
#' @export
#' @examples
#' \dontrun{
#' fahrenheit_to_celsius(85)
#' }

fahrenheit_to_celsius <- function(temp) {
  temp_k <- fahrenheit_to_kelvin(temp)
  result <- kelvin_to_celsius(temp_k)
  result
}

#' Converts angle in radians to degrees
#'
#' @details Converts angles in radians to degrees
#' 
#' @description This function allows you to convert angle in radians to degrees
#' @param rad angle in radians
#' @keywords radians to degrees
#' @return angle in degrees
#' @export
#' @examples
#' \dontrun{
#' radians_to_degrees(0.831)
#' }
radians_to_degrees <- function(rad) {(rad * 180) / (pi)}

#' Converts angle in degrees to radians
#'
#' @details Converts angle in degrees to radians
#' 
#' @description This function allows you to convert angle in degrees to radians
#' @param deg angle in degrees
#' @keywords degrees to radians
#' @return angle in radians
#' @export
#' @examples
#' \dontrun{
#' degrees_to_radians(47.608)
#' }
degrees_to_radians <- function(deg) {(deg * pi) / (180)}