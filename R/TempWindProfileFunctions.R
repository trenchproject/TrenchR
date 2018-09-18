#' Estimate surface roughness from empirical measurements.
#' 
#' 
#' @details Estimate surface roughness in m
#' @description This function allows you to estimate surface roughness in m from empirical wind speed (m/s) 
#' data collected at a vector of heights (m). Estimates surface roughness from empirical measurements.
#' 
#' @param u_r is wind velocity at a vector of reference heights in m/s.
#' @param zr is the vector of reference heights in m.
#' @return surface roughness
#' @keywords wind profile
#' @export
#' @examples
#' \dontrun{
#' surface_roughness(u_r=c(0.01,0.025,0.05,0.1,0.2), zr=c(0.05,0.25,0.5,0.75,1))
#'}

surface_roughness<- function(u_r, zr){
 mod1= lm(u_r~log(zr))
 d= as.numeric(mod1$coefficients[1]) #Zero Plane displacement:height at which the wind speed is zero
 # can also assume d=0.63h (Monteith 1975)
 inds= which(zr-d>0)   #indices of measurements where zr-d>0
 mod1= lm(u_r[inds]~log(zr[inds]-d))
 b= as.numeric(mod1$coefficients[1])
 n= as.numeric(mod1$coefficients[2])
 
 z0=exp(-b/n)
 return(z0)
 }


#' Estimate wind speed at a specific height under neutral conditions.
#' 
#' @details Calculates wind speed at a specified height under neutral conditions
#' @description This function allows you to calculate wind speed (m/s) at a 
#' specified height (m) within a boundary layer near the surface.  
#' The velocity profile is the neutral profile described by Sellers (1965). 
#' Function in equations (2) and (3) of Porter et al. (1973)
#' Profiles in neutral conditions.
#' 
#' @param u_r is wind velocity at reference height in m/s.
#' @param zr is initial reference height in m.
#' @param z0 is surface roughness in m.
#' @param z is height to scale to in m.
#' @return windspeed in m/s
#' @keywords wind profile
#' @export
#' @examples
#' \dontrun{
#' wind_speed_profile_neutral(u_r=0.1, zr=0.1, z0=0.2, z=0.15)
#'}
 
wind_speed_profile_neutral <- function(u_r, zr, z0, z) {
    u_z = u_r* log(z/z0 + 1) / log(zr/z0 + 1)
  return(u_z)
  }


#' Estimate temperature at a specified height under neutral conditions
#' 
#' @details Calculates temperature at a specified height under neutral conditions
#' @description This function allows you to calculate temperature (C) at a specified height (m) within a 
#' boundary layer near the surface.  The velocity profile is the neutral profile described by Sellers (1965). 
#' Function in equations (2) and (3) of Porter et al. (1973)
#' 
#' @param T_r is temperature at reference height in degrees C.
#' @param zr is initial reference height in m.
#' @param z0 is surface roughness in m.
#' @param z is height to scale to in m.
#' @param T_s is surface temperatures in degrees C.
#' @return temperature (degrees C)
#' @keywords temperature profile
#' @export
#' @examples
#' \dontrun{
#' air_temp_profile_neutral(T_r=20, zr=0.1, z0=0.2, z=0.15, T_s=25)
#'}
#'
air_temp_profile_neutral<-function(T_r, zr, z0, z, T_s){
  T_z= (T_r-T_s)*log(z/z0+1)/log(zr/z0+1)+T_s 
  return(T_z)
}


#' Estimate wind speed profile as in NicheMapR.
#' 
#' @details Calculates wind speed at a specified height
#' @description This function allows you to estimate wind speed (m/s) at a specified height (m).  
#' Estimates a single, unsegmented wind velocity using the MICRO routine from NicheMapR as described in Kearney and Porter 2016.
#' Adapted from Kearney and Porter 2016. NicheMapR – an R package for biophysical modelling: the microclimate model
#' Section 5. Vertical air temperature and wind speed profiles, p11

#' @param u_r is wind velocity at reference height in m/s.
#' @param zr is initial reference height in m.
#' @param z0 is surface roughness in m.
#' @param z is height to scale to in m.
#' @return wind speed (m/s) 
#' @keywords wind profile
#' @export
#' @examples
#' \dontrun{
#' wind_speed_profile(u_r=0.1, zr=0.1, z0=0.2, z=0.15)
#'}

wind_speed_profile<- function(u_r,zr,z0,z){
  #Friction velocity
  u_star=  0.4*u_r/log(zr/z0 +1)  #0.4 is von Karman constant
  u_z= 2.5*u_star*log(z/z0+1)
  return(u_z)
  }

#' Estimate air temperature profile as in NicheMapR
#' 
#' @details Estimate temperature at a specified height
#' @description This function allows you to estimate temperature (C) at a specified height (m).  
#' Estimates a single, unsegmented temperature profile using the MICRO routine from 
#' NicheMapR as described in Kearney and Porter 2016.
#' 
#' @param T_r is temperature at reference height in degrees C.
#' @param u_r is windspeed at reference height in m/s.
#' @param zr is initial reference height in m.
#' @param z0 is surface roughness in m.
#' @param z is height to scale to in m.
#' @param T_s is surface temperatures in degrees C.
#' @return temperature (degrees C)
#' @keywords temperature profile
#' @export
#' @examples
#' \dontrun{
#' air_temp_profile(T_r=20, u_r=0.1, zr=0.1, z0=0.2, z=0.15, T_s=25)
#'}
#'

air_temp_profile= function(T_r, u_r, zr, z0,z,T_s){
  #friction velocity
  u_star=  0.4*u_r/log(zr/z0 +1)  #0.4 is von Karman constant
  #sublayer stanton number
  S_ts= 0.62/(z0*u_star/12)^0.45
  #bulk Stanton number
  S_tb= 0.64/log(zr/z0+1)
  #Temperature at roughness height, z0
  T_z0= (T_r * S_tb +T_s * S_ts)/(S_tb+S_ts)
  #Temperature at local height
  T_z= T_z0 + (T_r - T_z0)*log(z/z0+1)
  return(T_z)
  }

