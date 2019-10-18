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
  #estimate u_Zloc  
  #! CHECK ORDER
  if(zr[1]<=z) {us_star=u_star[3]; z0s= z0[1]}
  if(zr[1]>z & zr[2]<=z) {us_star=u_star[1]; z0s= z0[2]}
  if(zr[1]>z & zr[2]>z) {us_star=u_star[2]; z0s= z0[3]}
  #estimate windspeed
  u_z= 2.5*us_star*log(z/z0s+1)
  return(u_z)
}

#-----------------
#Functions for TPCs add?
TPC.beta= function(T, shift=-1, breadth=0.1, aran=0, tolerance= 43, skew=0.7){ 
  T = T + 273.15 #Convert temperature in degrees Celsius to Kelvin
  shift= shift + 273.15 #Convert temperature in degrees Celsius to Kelvin         
  z=rep(0.01, length(T))
  z[which(is.na(T))]=NA  #account for NAs
  sel= which(T-shift>=0 & T-shift<=tolerance)
  z[sel]= ((T[sel]-shift)/tolerance)^(skew/breadth-1)*(1-(T[sel]-shift)/tolerance)^((1-skew)/breadth-1) / beta(skew/breadth,(1-skew)/breadth) 
  if(aran==1) z[sel]=z[sel]*exp(-0.6/(T[sel]*8.61734*10^(-5)))*10^10 #add scaling factor
  return(z)
}

#Performance Curve Function from Deutsch et al. 2008
TPC= function(T,Topt,CTmin, CTmax){
  F=T
  F[]=NA
  sigma= (Topt-CTmin)/4
  F[T<=Topt & !is.na(T)]= exp(-((T[T<=Topt & !is.na(T)]-Topt)/(2*sigma))^2) 
  F[T>Topt & !is.na(T)]= 1- ((T[T>Topt & !is.na(T)]-Topt)/(Topt-CTmax))^2
  #set negetative to zero
  F[F<0]<-0
  
  return(F)
}

