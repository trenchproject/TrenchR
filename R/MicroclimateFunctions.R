#' FIX HEADERS
#'
#' This function allows you to calculate zenith
#' @param J is Julian day.
#' @param lat is latitude.
#' @param lon is longitude.
#' @param Hr is hour of the day.
#' @keywords zenith
#' @export
#' @examples
#' zenith()
# 

#Microclimate functions from Kearney supplement

#FUNCTIONS TO WRITE:
#FUN: solar hour angle
#FUN: solar declination
#FUN: time of solar noon

#2.1 Extra-terrestrial Radiation

#wavelength-specific irradiance reaching plane perpendicular to the sun’s rays at the outer edge of the atmosphere
 

#J is Calendar day
#t_d is local standard time of day




Rad.Extraterrestrial= function(J, t_d  ){
  E=0.01675 #eccentricity of the earth’s orbit
  w= 2*pi/365
  a.r= 1+2*E*cos(w*J)
  t_sn #local standard time of true solar noon, is calculated by adding/subtracting 4 min for each degree of longitude west/east of the reference longitude for the local time zone
  sol.dec #solar declination
  h= 15*(t_d - t_sn)
  
  cosZ= cos(lat)*cos(sol.dec*h)+sin(lat)*sin(sol.dec)
  
  I.lambda=S.lambda*a.r*cosZ
}

#where S_λ is solar spectral irradiance reaching a plane perpendicular to the incoming radiation at one astronomical unit from the sun (Fig. 2, data stored internally in SOLRAD), a is the length of the long (semi-major) axis of the earth’s elliptical orbit around the sun, r is the distance between the earth and the sun and Z is the angle between the sun’s rays and a line extending perpendicular to the imaginary plane (so no direct radiation is received by this plane if Z > 90°, but see below for twilight conditions).

The factor 〖(a/r)〗^2 is approximated in the model as 1+2ε cos(ωd) where ω=2π/365,  ε is the eccentricity of the earth’s orbit (default value of 0.01675) and d is the day of the year (1-365). From the astronomical triangle, the term cosa〖Z=cos φ cosa〖δ cosa〖h+sina〖φ sinaδ 〗 〗 〗 〗, where φ is the latitude, δ is the solar declination and h is the solar hour angle. The solar declination δ=asina(0.39784993 sin ζ) where the ecliptic longitude of the earth in its orbit ζ = ω(d-80)+2ε[sina(ωd)-sina(ω80) ]. The hour angle h = 15(t¬d – t¬sn) degrees, with t¬d the local standard time of day and tsn the local standard time of true solar noon. The value of tsn is calculated by adding/subtracting 4 min for each degree of longitude west/east of the reference longitude for the local time zone. The hour angle at sunset H_+=arccosa[-tana〖δ tana〖φ]〗 〗 while the hour angle at sunrise H_-=-H_+. In the polar zones, where the (absolute) latitude φ>66°30', there may be no sunrise/sunset but rather a long day (H+ = π) or a long night (H+ = 0).