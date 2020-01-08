#' Estimate surface roughness from empirical measurements.
#' 
#' 
#' @details Estimate surface roughness in m
#' @description This function allows you to estimate surface roughness in m from empirical wind speed (m/s) 
#' data collected at a vector of heights (m). Estimates surface roughness from empirical measurements. 
#' References: Kingsolver and Buckley. 2015. Climate variability slows evolutionary responses of Colias butterflies to recent climate change. PRSb;
#' G. S. Campbell, J. M. Norman, An introduction to environmental biophysics. (Springer Science, New York, 1998); W. P. Porter, J. W. Mitchell, W. A. Beckman, C. B. DeWitt, Behavioral implications of mechanistic
#' ecology. Thermal and behavioral modeling of desert ecotherms and their microenvironment. Oecologia 13, 1-54 (1973)
#' 
#' @param u_r is wind velocity at a vector of reference heights in m/s.
#' @param zr is the vector of reference heights in m.
#' @return surface roughness (m)
#' @keywords wind profile
#' @family microclimate functions
#' @export
#' @examples
#' \dontrun{
#' surface_roughness(u_r=c(0.01,0.025,0.05,0.1,0.2), zr=c(0.05,0.25,0.5,0.75,1))
#'}

surface_roughness<- function(u_r, zr){

 mod1= lm(log(zr)~u_r)
 d= exp(as.numeric(mod1$coefficients[1])) #Zero Plane displacement:height at which the wind speed is zero
 # can also assume d=0.63h (Monteith 1975)
 inds= which(zr-d>0)   #indices of measurements where zr-d>0
 
 z0=NA
 if(length(inds)>0){
 mod1= lm(u_r[inds]~log(zr[inds]-d))
 b= as.numeric(mod1$coefficients[1])
 n= as.numeric(mod1$coefficients[2])
 z0=exp(-b/n)}
 
 return(z0)
 }


#' Estimate wind speed at a specific height under neutral conditions.
#' 
#' @details Calculates wind speed at a specified height under neutral conditions
#' @description This function allows you to calculate wind speed (m/s) at a 
#' specified height (m) within a boundary layer near the surface.  The profile assumes neutral conditions.
#' The velocity profile is the neutral profile described by Sellers (1965). 
#' Function is equations (2) and (3) of Porter et al. (1973). Source: Porter WP et al. 1973. Behavioral implications of mechanistic ecology. Oecologia 13:1-54.
#' 
#' @param u_r is wind velocity at reference height in m/s.
#' @param zr is initial reference height in m.
#' @param z0 is surface roughness in m.
#' @param z is height to scale to in m.
#' @return windspeed in m/s
#' @keywords wind profile
#' @family microclimate functions
#' @export
#' @examples
#' \dontrun{
#' wind_speed_profile_neutral(u_r=0.1, zr=0.1, z0=0.2, z=0.15)
#'}
 
wind_speed_profile_neutral <- function(u_r, zr, z0, z) {

  stopifnot(u_r>=0, zr>=0, z0>=0, z>=0)
  
      u_z = u_r* log(z/z0 + 1) / log(zr/z0 + 1)
  return(u_z)
  }

#' Estimate temperature at a specified height under neutral conditions
#' 
#' @details Calculates temperature at a specified height under neutral conditions
#' @description This function allows you to calculate temperature (C) at a specified height (m) within a 
#' boundary layer near the surface.  The velocity profile is the neutral profile described by Sellers (1965). 
#' Function in equations (2) and (3) of Porter et al. (1973). Source: Porter WP et al. 1973. Behavioral implications of mechanistic ecology. Oecologia 13:1-54.
#' 
#' @param T_r is temperature at reference height in degrees C.
#' @param zr is initial reference height in m.
#' @param z0 is surface roughness in m.
#' @param z is height to scale to in m.
#' @param T_s is surface temperatures in degrees C.
#' @return temperature (degrees C)
#' @keywords temperature profile
#' @family microclimate functions
#' @export
#' @examples
#' \dontrun{
#' air_temp_profile_neutral(T_r=20, zr=0.1, z0=0.2, z=0.15, T_s=25)
#'}
#'
air_temp_profile_neutral<-function(T_r, zr, z0, z, T_s){

  stopifnot(zr>=0, z0>=0, z>=0)
  
  T_z= (T_r-T_s)*log(z/z0+1)/log(zr/z0+1)+T_s 
  return(T_z)
}

#' Estimate wind speed profile as in NicheMapR.
#' 
#' @details Calculates wind speed at a specified height
#' @description This function allows you to estimate wind speed (m/s) at a specified height (m).  
#' Estimates a single, unsegmented wind velocity using the MICRO routine from NicheMapR as described in Kearney and Porter 2016.
#' Source: Kearney MR and Porter WP. 2016. NicheMapR – an R package for biophysical modelling: the microclimate model. Ecography 40:664-674.
#' Section 5. Vertical air temperature and wind speed profiles, p11

#' @param u_r is wind velocity at reference height in m/s.
#' @param zr is initial reference height in m.
#' @param z0 is surface roughness in m.
#' @param z is height to scale to in m.
#' @return wind speed (m/s) 
#' @keywords wind profile
#' @family microclimate functions
#' @export
#' @examples
#' \dontrun{
#' wind_speed_profile(u_r=0.1, zr=0.1, z0=0.2, z=0.15)
#'}

wind_speed_profile<- function(u_r,zr,z0,z){
  
  stopifnot(u_r>=0, zr>=0, z0>=0, z>=0)
  
  #Friction velocity
  u_star=  0.4*u_r/log(zr/z0 +1)  #0.4 is von Karman constant
  u_z= 2.5*u_star*log(z/z0+1)
  return(u_z)
  }

#' Estimate air temperature profile as in NicheMapR
#' 
#' @details Estimate temperature at a specified height
#' @description This function allows you to estimate temperature (C) at a specified height (m).  
#' Estimates a single, unsegmented temperature profile using the MICRO routine from NicheMapR. Source: Kearney MR and Porter WP. 2016. NicheMapR – an R package for biophysical modelling: the microclimate model. Ecography 40:664-674.
#' 
#' @param T_r is temperature at reference height in degrees C.
#' @param u_r is windspeed at reference height in m/s.
#' @param zr is initial reference height in m.
#' @param z0 is surface roughness in m.
#' @param z is height to scale to in m.
#' @param T_s is surface temperatures in degrees C.
#' @return temperature (degrees C)
#' @keywords temperature profile
#' @family microclimate functions
#' @export
#' @examples
#' \dontrun{
#' air_temp_profile(T_r=20, u_r=0.1, zr=0.1, z0=0.2, z=0.15, T_s=25)
#'}
#'

air_temp_profile= function(T_r, u_r, zr, z0,z,T_s){
  
  stopifnot(u_r>=0, zr>=0, z0>=0, z>=0)
  
  #friction velocity
  u_star=  0.4*u_r/log(zr/z0 +1)  #0.4 is von Karman constant
  #sublayer stanton number
  S_ts= 0.62/(z0*u_star/12)^0.45
  #bulk Stanton number
  S_tb= 0.64/log(zr/z0+1)
  #Temperature at roughness height, z0
  T_z0= (T_r * S_tb +T_s * S_ts)/(S_tb+S_ts)
  #Temperature at local height
  #Inital from vignette: T_z= T_z0 + (T_r - T_z0)*log(z/z0+1)
  T_z= T_z0 + (T_r - T_z0)*log(z/z0+1)/log(zr/z0+1)
  return(T_z)
  }

#' Estimate temperature at a specified height 
#' 
#'   
#' @details Calculates temperature at a specified height
#' @description This function allows you to calculate temperature (C) at a specified height (m).  
#' Estimates a three segment velocity and temperature profile based on user-specified, 
#' experimentally determined values for 3 roughness heights and reference heights.  
#' Multiple heights are appropriate in heterogenous areas with, for example, a meadow, bushes, and rocks.
#' Implements the MICROSEGMT routine from NicheMapR as described in Kearney and Porter 2016. Source: Kearney MR and Porter WP. 2016. NicheMapR – an R package for biophysical modelling: the microclimate model. Ecography 40:664-674.
#' 
#' @param T_r is a vector of temperature at the 3 reference heights in degrees C.
#' @param u_r is a vector of wind speeds at the 3 reference heights in m/s.
#' @param zr is a vector of 3 reference heights in m.
#' @param z0 is a vector of 3 experimentally determined roughness heights in m.
#' @param z is height to scale to in m.
#' @param T_s is surface temperatures in degrees C.
#' @keywords temperature profile
#' @family microclimate functions
#' @export
#' @examples
#' \dontrun{
#' air_temp_profile_segment(T_r=c(25,22,20),u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.15,0.2), z=0.3, T_s=27)
#'}
#'

air_temp_profile_segment= function(T_r, u_r, zr, z0,z,T_s){
  
  stopifnot(z>=0)
  
  #order roughness and segment heights 
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
  if(zr[1]<=z) {us_star=u_star[1]; z0s= z0[1]; T_rs= T_r[1]; zrs= zr[1]}
  if(zr[1]>z & zr[2]<=z) {us_star=u_star[2]; z0s= z0[2]; T_rs= T_r[2]; zrs= zr[2]}
  if(zr[1]>z & zr[2]>z) {us_star=u_star[3]; z0s= z0[3]; T_rs= T_r[3]; zrs= zr[3]}
  #estimate windspeed
  u_z= 2.5*us_star*log(z/z0s+1)
  
  #Temperature at roughness height, z0
  T_z0= (T_rs * S_tb +T_s * S_ts)/(S_tb+S_ts)
  #Temperature ar local height
  T_z= T_z0 + (T_rs - T_z0)*log(z/z0s+1)/log(zrs/z0s+1)
  return(T_z)
}

#' Estimate windspeed at a specified height 
#' 
#'   
#' @details Calculates temperature at a specified height
#' @description This function allows you to calculate wind speed (m/s) at a specified height (m).   
#' Estimates a three segment velocity and temperature profile based on user-specified, 
#' experimentally determined values for 3 roughness heights and reference heights.  
#' Multiple heights are appropriate in heterogenous areas with, for example, a meadow, bushes, and rocks.
#' Implements the MICROSEGMT routine from NicheMapR as described in Kearney and Porter 2016. Source: Kearney MR and Porter WP. 2016. NicheMapR – an R package for biophysical modelling: the microclimate model. Ecography 40:664-674.
#' 
#' @param u_r is a vector of wind speeds at the 3 reference heights in m/s.
#' @param zr is a vector of 3 reference heights in m.
#' @param z0 is a vector of 3 experimentally determined roughness heights in m.
#' @param z is height to scale to in m.
#' @keywords wind speed profile
#' @family microclimate functions
#' @export
#' @examples
#' \dontrun{
#' wind_speed_profile_segment(u_r=c(0.01,0.025,0.05), zr=c(0.05,0.25,0.5), z0=c(0.01,0.15,0.2), z=0.3)
#'}
#'

wind_speed_profile_segment= function(u_r, zr, z0,z){
  
  stopifnot(z>=0)
  
  #order roughness and segment heights so that z1>z2>z0 
  zr.ord= order(zr, decreasing = TRUE)
  zr= zr[zr.ord]
  z0= z0[zr.ord]
  u_r=u_r[zr.ord]
  
  #friction velocity
  u_star=  0.4*u_r/log(zr/z0 +1) #0.4 is von Karman constant
  
  #estimate u_Zloc  
  if(z<=zr[3]) {us_star=u_star[3]; z0s= z0[3]; zrs= zr[3]}
  if(z>zr[3] & z<zr[2]) {us_star=u_star[2]; z0s= z0[2]; zrs= zr[2]}
  if(z>=zr[2]) {us_star=u_star[1]; z0s= z0[1]; zrs= zr[1]}
  #estimate windspeed
  u_z= 2.5*us_star*log(z/z0s+1)
  
  return(u_z)
}
