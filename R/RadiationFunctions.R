#' Partition radiation into direct, diffuse, and reflected components using models from Campbell & Norman 1988 
#'
#' @details This function allows you to Partition radiation into direct, diffuse, and reflected components
#' @param J Julian Day
#' @param psi zenith angle in radians
#' @param tau transmissivity (%)
#' @param elev Elevation (m)
#' @param rho albedo (%)
#' @keywords radiation
#' @export
#' @examples
#' radiation()

#=============================================================
#USE CAMPBELL AND NORMAL MODEL TO ESTIMATE RADIATION
# Biophysical models from Campbell & Norman 1998
# constants

radiation=function(J, psi, tau, elev, rho=0.7){
  
  sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
  c_p=29.3 # specific heat of air, J/mol degrees K or C
  S_p0=1360 # extraterrestrial flux density, W/m^2 (p159)
  
  # Radiation
  # adjust for elevation
  p_a=101.3* exp (-elev/8200)  # atmospheric pressure
  m_a=p_a/(101.3*cos(psi))  # (11.12) optical air mass
  m_a[which(psi>(80*pi/180))]=5.66
  
  # Flux densities
  #dd2= 1+2*0.1675*cos(2*pi*J/365) #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  
  #S_p is direct radiation reaching earth's surface
  #S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  S_p=S_p0*tau^m_a *cos(psi) #Use initial Cambell and Norman
  
  #S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)*dd2 #with correction factor
  S_d=0.3*(1-tau^m_a)* S_p0*cos(psi)
  #S_t=S_p*cos (psi)+S_d # solar irradience 
  S_r= rho*(S_p+S_d) # (11.10) reflected radiation
  
  #return direct, diffuse, reflected
  return( c(S_p, S_d, S_r))
}

