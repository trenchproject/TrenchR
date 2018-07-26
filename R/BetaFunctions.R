#' Estimate temperature at a specified height 
#' 
#'   
#' @details Calculates temperature at a specified height
#' @description This function allows you to calculate temperature (C) at a specified height (m).  
#' Estimates a three segment velocity and temperature profile based on user-specified, 
#' experimentally determined values for 3 roughness heights and reference heights.  
#' Multiple heights are appropriate in heterogenous areas with, for example, a meadow, bushes, and rocks.
#' Implements the MICROSEGMT routine from NicheMapR as described in Kearney and Porter 2016. 
#' 
#' NEED TO CHECK EQUATIONS.
#' 
#' @param T_r is a vector of temperature at the 3 reference heights in degrees C.
#' @param u_r is a vector of wind speeds at the 3 reference heights in m/s.
#' @param zr is a vector of 3 reference heights in m.
#' @param z0 is a vector of 3 experimentally determined roughness heights in m.
#' @param z is height to scale to in m.
#' @param T_s is surface temperatures in degrees C.
#' @keywords temperature profile
#' @export
#' @examples
#' \dontrun{
#' air_temp_profile_segment_in_beta(T_r=c(25,22,20),u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.15,0.2), z=0.3, T_s=27)
#'}
#'

air_temp_profile_segment_in_beta= function(T_r, u_r, zr, z0,z,T_s){
  
  #order roughness and segment heights so that z1>z2>z0 
  #!CHECK
  zr.ord= order(zr, decreasing = TRUE)
  zr= zr[zr.ord]
  z0= z0[zr.ord]
  u_r=u_r[zr.ord]
  T_r= T_r[zr.ord]
  
  #friction velocity
  u_star=  0.4*u_r/log(zr/z0 +1) #0.4 is von Karman constant
  #sublayer stanton number
  S_ts= 0.62/(z0[3]*u_star[2]/12)^0.45
  #bulk Stanton number
  S_tb= 0.64/log(zr[2]/z0[3]+1)
  
  #estimate u_Zloc  
  #! CHECK ORDER
  if(zr[1]<=z) {us_star=u_star[3]; z0s= z0[1]; T_rs= T_r[1]}
  if(zr[1]>z & zr[2]<=z) {us_star=u_star[1]; z0s= z0[2]; T_rs= T_r[2]}
  if(zr[1]>z & zr[2]>z) {us_star=u_star[2]; z0s= z0[3]; T_rs= T_r[3]}
  #estimate windspeed
  u_z= 2.5*us_star*log(z/z0s+1)
  
  #Temperature at roughness height, z0
  T_z0= (T_rs * S_tb +T_s * S_ts)/(S_tb+S_ts)
  #Temperature ar local height
  T_z= T_z0 + (T_rs - T_z0)*log(z/z0s+1)
  return(T_z)
}

#' Get windspeed at a certain height 
#' 
#' 
#' @details Calculates wind speed at a specified height
#' @description This function allows you to calculate wind speed (m/s) at a specified height (m). 
#' Estimates a three segment velocity and temperature profile based on user-specified, 
#' experimentally determined values for 3 roughness heights and reference heights.  
#' Multiple heights are appropriate in heterogenous areas with, for example, a meadow, bushes, and rocks. 
#' Implements the MICROSEGMT routine from NicheMapR as described in Kearney and Porter 2016. 
#' NEED TO CHECK NOTATION AND EQUATIONS.
#' 
#' @param u_r is a vector of wind speeds at the 3 reference heights in m/s.
#' @param zr is a vector of 3 reference heights in m.
#' @param z0 is a vector of 3 experimentally determined roughness heights in m.
#' @param z is height to scale to in m.
#' @keywords wind profile
#' @export
#' @examples
#' \dontrun{
#' wind_speed_profile_segment_in_beta(u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.15,0.2), z=0.3)
#'}

wind_speed_profile_segment_in_beta= function(u_r,zr,z0,z){
  #order roughness and segment heights so that z1>z2>z0 #!CHECK
  zr.ord= order(zr, decreasing = TRUE)
  zr= zr[zr.ord]
  z0= z0[zr.ord]
  u_r=u_r[zr.ord]
  
  #friction velocity
  u_star=  0.4*u_r/log(zr/z0 +1) #0.4 is von Karman constant
  #sublayer stanton number
  S_ts= 0.62/(z0[3]*u_star[2]/12)^0.45
  #bulk Stanton number
  S_tb= 0.64/log(zr[2]/z0[3]+1)
  #estimate u_Zloc  
  #! CHECK ORDER
  if(zr[1]<=z) {us_star=u_star[3]; z0s= z0[1]}
  if(zr[1]>z & zr[2]<=z) {us_star=u_star[1]; z0s= z0[2]}
  if(zr[1]>z & zr[2]>z) {us_star=u_star[2]; z0s= z0[3]}
  #estimate windspeed
  u_z= 2.5*us_star*log(z/z0s+1)
  return(u_z)
}

