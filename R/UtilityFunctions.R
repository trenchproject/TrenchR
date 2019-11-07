#' Calculate day of the year from a passed date
#' 
#'  
#' @details Calculate day of the year
#'
#' @description This function allows you to calculate day of year from text specifying a date.
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
#' @description This function allows you to calculate solar declination, which is the angular distance of the sun north or south of the earth’s equator, based on the day of year. Source: Campbell and Norman. 1998. An Introduction to Environmental Biophysics.
#' @param doy day of year
#' @return declination angle in radians
#' @keywords Declination angle
#' @export
#' @examples
#' \dontrun{
#' dec_angle(doy=112)
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
#' @description This function allows you to calculate daylength in hours as a function of latitude and day of year. Uses the CMB model (Forsythe et al. 1995). Source: Campbell and Norman. 1998. An Introduction to Environmental Biophysics.
#' @param lat latitude in degrees
#' @param doy day of year
#' @return hours 
#' 
#' @keywords Day length
#' @export
#' @examples
#' \dontrun{
#' daylength(lat=47.61, doy=112)
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
#' @details Calculate time of solar noon
#'
#' @description This function allows you to calculate the time of solar noon in hours as a function of the day of year and longitude. Source: Campbell and Norman. 1998. An Introduction to Environmental Biophysics.
#' @param lon longitude in degrees 
#' @param doy day of year
#' @return time at solar noon
#' @keywords Solar noon time
#' @export
#' @examples
#' \dontrun{
#' solar_noon(lon=-122.335, doy=112)
#' }

solar_noon <- function(lon, doy){
  # Calculate the time of solar noon for each day using longitude correction (LC), equation of time (ET), and a conversion (f)
  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f[f>360]=f[f>360]-360 #ensure 0 to 360 degrees
  f=f*pi/180 #convert f in degrees to radians
  
  ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
  
  lon[lon<0]=360+lon[lon<0] #convert to 0 to 360
  LC= 1/15*lon%%15 # longitude correction, 1/15h for each degree of standard meridian
  LC[LC>0.5]= LC[LC>0.5]-1
  t_0 = 12-LC-ET # solar noon
  return(t_0)
}

#' Calculate Zenith Angle
#' 
#' 
#' @details Calculate Zenith angle
#'
#' @description This function allows you to calculate the zenith angle, the location of the sun as an angle (in degrees) measured from vertical. Source: Campbell and Norman. 1998. An Introduction to Environmental Biophysics.
#' @param doy is day of year.
#' @param lat is latitude in degrees.
#' @param lon is longitude in degrees.
#' @param hour is hour of the day.
#' @return Zenith angle in degrees
#' @keywords Zenith angle
#' @export
#' @examples
#' \dontrun{
#' zenith_angle(doy=112, lat=47.61, lon=-122.33, hour=12)
#' }

zenith_angle=function(doy, lat, lon, hour){

lat=lat*pi/180 #to radians
lon=lon*pi/180 #to radians
  
RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
DecAng = asin(0.39795 * cos(RevAng));                            # Declination angle in radians           
  
f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
f=f*pi/180 #convert f in degrees to radians
ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
LC= 1/15* (15 - lon%%15) # longitude correction, 1/15h for each degree of standard meridian
t_0 = 12-LC-ET # solar noon
Daylength = 24 - (24 / pi) * acos ((sin (6 * pi / 180) + sin (lat) * sin (DecAng)) / (cos (lat) * cos (DecAng))) #hours of daylight
             
cos.zenith= sin(DecAng)*sin(lat) + cos(DecAng)*cos(lat)*cos(pi/12*(hour-t_0)); #cos of zenith angle in radians
zenith=acos(cos.zenith)*180/pi # zenith angle in degrees
zenith[zenith>90]=90 # if measured from the vertical psi can't be greater than pi/2 (90 degrees)

return(zenith)
}

#' Calculate Azimuth angle
#' 
#' 
#' @details Calculate azimuth angle
#'
#' @description This function allows you to calculate the azimuth angle, the angle (in degrees) from which the sunlight in coming measured from true north or south measured in the horizontal plane. The azimuth angle is measured with respect to due south, increasing in the counter clockwise direction so 90 degrees is east. Source: Campbell and Norman. 1998. An Introduction to Environmental Biophysics.
#' @param doy is day of year.
#' @param lat is latitude in degrees.
#' @param lon is longitude in degrees.
#' @param hour is hour of the day.
#' @return Azimuth angle in degrees
#' @keywords Azimuth angle
#' @export
#' @examples
#' \dontrun{
#' azimuth_angle(doy=112, lat=47.61, lon=-122.33, hour=12)
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
#' 
#' @details Estimate air pressure (kPa) as a function of elevation. 
#' @description  This function allows you to calculate estimated air pressure (kPa) as a function of elevation. Source: http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html.
#' @param elev elevation in meters.
#' @keywords Air Pressure
#' @return Air pressure in kPa
#' @export
#' @examples
#' \dontrun{
#' airpressure_from_elev(1500)
#' }
airpressure_from_elev<- function(elev){  
 
  #p= 101325* (1 - 2.25577*10^(-5)*elev)^5.25588       
  #p= p/1000 #convert to kPa
  
  p_a=101.3* exp (-elev/8200)  #Campbell and Norman
  
  return(p_a)
}

#' Converts Fahrenheit to Kelvin
#' 
#' 
#' @details Converts Fahrenheit to Kelvin.
#' @description  This function allows you to convert temperature from Fahrenheit to Kelvin. Source: https://swcarpentry.github.io.
#' @param T Temperature in Fahrenheit.
#' @keywords Fahrenheit Kelvin
#' @return Temperature in Kelvin
#' @export
#' @examples
#' \dontrun{
#' fahrenheit_to_kelvin(85)
#' }

fahrenheit_to_kelvin <- function(T) {
  kelvin <- ((T - 32) * (5/9)) + 273.15
  return(kelvin)
}

#' Converts Kelvin to Celsius
#' 
#' 
#' @details Converts Kelvin to Celsius.
#' @description This function allows you to convert temperature from Kelvin to Celsius. Source: https://swcarpentry.github.io.
#' @param T Temperature in Fahrenheit.
#' @keywords Celsius Kelvin
#' @return Temperature in Celsius
#' @export
#' @examples
#' \dontrun{
#' kelvin_to_celsius(270)
#' }

kelvin_to_celsius <- function(T) {
  Celsius <- T - 273.15
  return(Celsius)
}

#' Converts Fahrenheit to Celsius
#' 
#' 
#' @details Converts Fahrenheit to Celsius.
#' @description This function allows you to convert temperature from Fahrenheit to Celsius. Source: https://swcarpentry.github.io.
#' @param T Temperature in Fahrenheit.
#' @keywords Fahrenheit Celsius
#' @return Temperature in Celsius
#' @export
#' @examples
#' \dontrun{
#' fahrenheit_to_celsius(85)
#' }

fahrenheit_to_celsius <- function(T) {
  temp_k <- fahrenheit_to_kelvin(T)
  result <- kelvin_to_celsius(temp_k)
  return(result)
}

#' Converts angle in radians to degrees
#'
#' @details Converts angles in radians to degrees.
#' 
#' @description This function allows you to convert angle in radians to degrees.
#' @param rad angle in radians
#' @keywords radians to degrees
#' @return angle in degrees
#' @export
#' @examples
#' \dontrun{
#' radian_to_degree(0.831)
#' }
radian_to_degree <- function(rad) {(rad * 180) / (pi)}

#' Converts angle in degrees to radians
#'
#'
#' @details Converts angle in degrees to radians.
#' @description This function allows you to convert angle in degrees to radians.
#' @param deg angle in degrees
#' @keywords degrees to radians
#' @return angle in radians
#' @export
#' @examples
#' \dontrun{
#' degree_to_radian(47.608)
#' }
degree_to_radian <- function(deg) {(deg * pi) / (180)}

