#' @title Utility: Julian Day from Date
#' 
#' @description Convert a date (day, month, year) to Julian Day (day of year).
#'
#' @param day \code{character} numerical date in standard format (e.g. "2017-01-02", "01-02", "01/02/2017" etc). 
#'
#' @param format \code{character} date format following \code{\link[base]{POSIXlt}}" conventions. Default value = "%Y-%m-%d" 
#'
#' @return \code{numeric} Julian day number, 1-366 (eg. 1 for January 1st)
#' 
#' @details
#' \cr \cr
#' 
#' @family utility functions  
#'  
#' @export
#'
#' @examples
#'   day_of_year(day = "2017-04-22", format = "%Y-%m-%d")
#'   day_of_year(day = "2017-04-22")
#'   day_of_year(day = "04/22/2017", format = "%m/%d/%Y")
#'
day_of_year <- function(day, format = "%Y-%m-%d"){
  
  day <- as.POSIXlt(day, format = format)
  as.numeric(strftime(day, format = "%j"))
  
}

#'  @title Calculate solar declination in radians
#' 
#' @description Calculate solar declination, which is the angular distance of the sun north or south of the earth’s equator, based on the day of year \insertCite{Campbell1998}{TrenchR}.
#' 
#' @param doy \code{numeric} day of year (1-366).
#' 
#' @return \code{numeric} declination angle in radians.
#' 
#' @family utility functions
#'
#' @references
#'   \insertAllCited{}
#' 
#' @export
#' 
#' @examples
#' dec_angle(doy=112)
#' 
dec_angle <- function(doy){
  stopifnot(doy > 0, doy < 367)
  
  RevAng <- 0.21631 + 2 * atan(0.967 * tan (0.0086 * (-186 + doy))) # Revolution angle in radians, calculated per day
  asin(0.39795 * cos (RevAng))                                      # Declination angle in radians  
  
}

#'  @title Calculate day length
#' 
#' @description Calculate daylength in hours as a function of latitude and day of year. Uses the CMB model \insertCite{Campbell1998}{TrenchR}.
#'
#' @param lat \code{numeric} latitude in decimal degrees.
#' 
#' @param doy \code{numeric} day of year (1-366).
#' 
#' @return \code{numeric} day length in hours.
#'
#' @references
#'   \insertAllCited{}
#' 
#' @family utility functions
#'
#' @export
#'
#' @examples
#'   daylength(lat=47.61, doy=112)
#'
daylength <- function(lat, doy){
  stopifnot(lat >= -90, lat <= 90, doy > 0, doy < 367)
  
  lat_rad <- degree_to_radian(lat)
  DecAng <- dec_angle(doy)
  subset <- (sin (6 * pi / 180) + sin (lat_rad) * sin (DecAng)) / (cos (lat_rad) * cos (DecAng))
  subset[which(subset>1)] <- 1
  subset[which(subset< -1)] <- -1
  24 - (24 / pi) * acos(subset) 
  
}

#' @title Calculate time of solar noon
#' 
#' @description  Calculate the time of solar noon in hours as a function of the day of year and longitude  \insertCite{Campbell1998}{TrenchR}.
#'
#' @param lon \code{numeric} longitude in decimal degrees. 
#'
#' @param doy \code{numeric} day of year.
#'
#' @param offset \code{numeric} number of hours to add to UTC to get local time (to improve accuracy but not always necessary).
#'
#' @return \code{numeric} time at solar noon.
#'
#' @references
#'   \insertAllCited{}
#'
#' @family utility functions
#'
#' @export
#'
#' @examples
#'   solar_noon(lon=-122.335, doy=112)
#'
solar_noon <- function(lon, doy, offset = NA){
  
  stopifnot(lon >= -180, lon <= 180, doy > 0, doy < 367)
  
  # Calculate the time of solar noon for each day using longitude correction (LC), equation of time (ET), and a conversion (f)
  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f[f>360]=f[f>360]-360 #ensure 0 to 360 degrees
  f=f*pi/180 #convert f in degrees to radians
  
  ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
  
  lon[lon<0]=360+lon[lon<0] #convert to 0 to 360
  LC= 1/15*(lon%%15) # longitude correction, 1/15h for each degree of standard meridian
  LC[LC>0.5]= LC[LC>0.5]-1
  t_0 = 12-LC-ET # solar noon
  
  #Check if offset is as expected. (Is the timezone of the location the same as that of the meridian 
  #that's within 7.5 degrees from that location?)
  lon[lon>180]=lon[lon>180]-360
  if (!is.na(offset)) {
    offset_theory <- as.integer(lon / 15) + lon / abs(lon) * as.integer(abs(lon) %% 15 / 7.5)
    t_0 = t_0 - offset_theory + offset
  }
  
  t_0
}


#' @title Calculate Zenith Angle
#' 
#' @description  calculate the zenith angle, the location of the sun as an angle (in degrees) measured from vertical \insertCite{Campbell1998}{TrenchR}.
#' 
#' @param doy \code{numeric}  day of year.
#' 
#' @param lat \code{numeric}  latitude in decimal degrees.
#' 
#' @param lon \code{numeric}  longitude in decimal degrees.
#' 
#' @param hour \code{numeric}  hour of the day.
#' 
#' @param offset \code{numeric}  the number of hours to add to UTC to get local time (to improve accuracy but not always necessary)
#' 
#' @return \code{numeric} zenith angle in degrees.
#' 
#' @family utility functions
#' 
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' 
#' zenith_angle(doy=112, lat=47.61, lon=-122.33, hour=12)
#' 
zenith_angle <- function(doy, lat, lon, hour, offset = NA){

  stopifnot(doy>0, doy<367, lat>=-90, lat<=90, lon>=-180, lon<=180, hour>=0, hour<=24)
  
  lat=lat*pi/180 #to radians
    
  RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
  DecAng = asin(0.39795 * cos(RevAng));                            # Declination angle in radians           
    
  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f=f*pi/180 #convert f in degrees to radians
  ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
  lon[lon<0]=360+lon[lon<0] #convert to 0 to 360
  LC= 1/15*(lon%%15) # longitude correction, 1/15h for each degree of standard meridian
  LC[LC>0.5]= LC[LC>0.5]-1
  t_0 = 12-LC-ET # solar noon
  
  #Check if offset is as expected. (Is the timezone of the location the same as that of the meridian 
  #that's within 7.5 degrees from that location?)
  lon[lon>180]=lon[lon>180]-360
  if (!is.na(offset)) {
    offset_theory <- as.integer(lon / 15) + lon / abs(lon) * as.integer(abs(lon) %% 15 / 7.5)
    t_0 = t_0 - offset_theory + offset
  }
  
  cos.zenith= sin(DecAng)*sin(lat) + cos(DecAng)*cos(lat)*cos(pi/12*(hour-t_0)); #cos of zenith angle in radians
  zenith=acos(cos.zenith)*180/pi # zenith angle in degrees
  zenith[zenith>90]=90 # if measured from the vertical psi can't be greater than pi/2 (90 degrees)

zenith
}

#'  @title Calculate Azimuth angle
#'
#' @description  calculate the azimuth angle, the angle (in degrees) from which the sunlight is coming measured from true north or south measured in the horizontal plane. The azimuth angle is measured with respect to due south, increasing in the counter clockwise direction so 90 degrees is east \insertCite{Campbell1998}{TrenchR}.
#' 
#' @param doy \code{numeric} day of year (1-366).
#' 
#' @param lat \code{numeric} latitude in decimal degrees.
#' 
#' @param lon \code{numeric} longitude in decimal degrees.
#' 
#' @param hour \code{numeric} hour of the day.
#' 
#' @param offset \code{numeric} number of hours to add to UTC to get local time (to improve accuracy but not always necessary)
#' 
#' @return \code{numeric} azimuth angle in degrees
#'
#' @references
#'   \insertAllCited{}
#'
#' @family utility functions
#' 
#' @export
#' 
#' @examples
#'   azimuth_angle(doy=112, lat=47.61, lon=-122.33, hour=12, offset = -8)
#' 
azimuth_angle <- function(doy, lat, lon, hour, offset = NA){
  
  stopifnot(doy>0, doy<367, lat>=-90, lat<=90, lon>=-180, lon<=180, hour>=0, hour<=24)
  
  lat=lat*pi/180 #to radians
  
  RevAng = 0.21631 + 2 * atan(0.967 * tan(0.0086 * (-186 + doy))); # Revolution angle in radians
  DecAng = asin(0.39795 * cos(RevAng));                          # Declination angle in radians           

  f=(279.575+0.9856*doy)  # f in degrees as a function of day of year, p.169 Campbell & Norman 2000
  f=f*pi/180 #convert f in degrees to radians
  
  ET= (-104.7*sin (f)+596.2*sin (2*f)+4.3*sin (3*f)-12.7*sin (4*f)-429.3*cos (f)-2.0*cos (2*f)+19.3*cos (3*f))/3600   # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day
  lon[lon<0]=360+lon[lon<0] #convert to 0 to 360

  ##Set up two booleans on whether we need to apply a correction on azimuth angle at the end
  azi_corr1 = FALSE
  azi_corr2 = TRUE
  
  LC= 1/15*(lon%%15) # longitude correction, 1/15h for each degree of standard meridian

  if (LC > 0.5) {
    LC = LC - 1
  } else {
    azi_corr1 = TRUE
  }
  
  t_0 = 12-LC-ET # solar noon
  
  #Check if offset is as expected. (Is the timezone of the location the same as that of the meridian 
  #that's within 7.5 degrees from that location?)
  lon[lon>180]=lon[lon>180]-360
  if (!is.na(offset)) {
    offset_theory <- as.integer(lon / 15) + lon / abs(lon) * as.integer(abs(lon) %% 15 / 7.5)
    if (offset_theory != offset) {
      t_0 = t_0 - offset_theory + offset
      azi_corr2 = FALSE
    }
  }
  
  cos.zenith = sin(DecAng) * sin(lat) + cos(DecAng) * cos(lat) * cos(pi / 12 * (hour - t_0)); #cos of zenith angle in radians
  zenith = acos(cos.zenith) # zenith angle in radians
  if (zenith > pi / 2) zenith = pi / 2 # if measured from the vertical psi can't be greater than pi/2 (90 degrees)
  
  cos.azimuth = -(sin(DecAng) - cos(zenith)*sin(lat)) / (cos(lat) * sin(zenith))
  azimuth = acos(cos.azimuth) * 180 / pi #azimuth angle in degrees
  
  if (azi_corr1 && azi_corr2) {
    azimuth = 360 - azimuth
  }
  
  azimuth
}

#'  @title Estimate air pressure in kPa (Kilo Pascal)
#'
#' @description Calculate estimated air pressure (kPa) as a function of elevation. Source: http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html.
#' 
#' @param elev \code{numeric} elevation in meters.
#' 
#' @family utility functions
#' 
#' @return \code{numeric} air pressure in kPa.
#' 
#' @export
#' 
#' @examples
#'   airpressure_from_elev(1500)
#'
airpressure_from_elev <- function(elev){  
 
  stopifnot(elev >= 0)
  
  #p= 101325* (1 - 2.25577*10^(-5)*elev)^5.25588       
  #p= p/1000 #convert to kPa
  
  101.3 * exp (-elev/8200)  

}

#' @title Converts Fahrenheit to Kelvin
#' 
#' @description Convert temperature from Fahrenheit to Kelvin. Source: https://swcarpentry.github.io.
#' 
#' @param T \code{numeric} temperature in Fahrenheit.
#' 
#' @family utility functions
#' 
#' @return \code{numeric}  temperature in Kelvin.
#' 
#' @export
#' 
#' @examples
#'   fahrenheit_to_kelvin(85)
#' 
fahrenheit_to_kelvin <- function(T) {
  ((T - 32) * (5/9)) + 273.15
}

#' @title Converts Kelvin to Celsius
#' 
#' 
#' @description Convert temperature from Kelvin to Celsius. Source: https://swcarpentry.github.io.
#' 
#' @param T \code{numeric}  temperature in Kelvin.
#' 
#' @keywords Celsius Kelvin
#' 
#' @family utility functions
#' 
#' @return \code{numeric} temperature in Celsius
#' 
#' @export
#' 
#' @examples
#' 
#' kelvin_to_celsius(270)
#' 
kelvin_to_celsius <- function(T) {
  T - 273.15
}

#' @title Converts Fahrenheit to Celsius
#' 
#' @description Convert temperature from Fahrenheit to Celsius. Source: https://swcarpentry.github.io.
#' 
#' @param T \code{numeric} temperature in Fahrenheit.
#' 
#' @family utility functions
#' 
#' @return \code{numeric} temperature in Celsius
#' 
#' @export
#' 
#' @examples
#'   fahrenheit_to_celsius(85)
#' 
fahrenheit_to_celsius <- function(T) {
  temp_k <- fahrenheit_to_kelvin(T)
  kelvin_to_celsius(temp_k)
}

#' @title Converts angle in radians to degrees
#' 
#' @description Convert angle in radians to degrees.
#' 
#' @param rad \code{numeric} angle in radians.
#' 
#' @return \code{numeric} angle in degrees.
#' 
#' @export
#' 
#' @examples
#'   radian_to_degree(0.831)
#' 
radian_to_degree <- function(rad) {
  (rad * 180) / (pi)
}

#'  @title Converts angle in degrees to radians
#'
#' @description Convert angle in degrees to radians.
#' 
#' @param deg \code{numeric} angle in degrees.
#' 
#' @family utility functions
#' 
#' @return \code{numeric} angle in radians.
#' 
#' @export
#' 
#' @examples
#'   degree_to_radian(47.608)
#' 
degree_to_radian <- function(deg) {
  (deg * pi) / (180)
}

