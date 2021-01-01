#' Predicts body temperatures (operative environmental temperature) of a lizard in °C.
#' 
#' @details Predicts body temperatures (operative environmental temperature) of a lizard in °C.
#' @description Predicts body temperature (operative environmental temperature) of a lizard in °C. 
#' Based on Campbell and Norman (1998, An introduction to environmental biophysics). 
#' Designed for Sceloporus lizards and described in Buckley (2008, Linking traits to energetics and population dynamics to predict lizard ranges in changing environments. American Naturalist 171: E1-E19).
#' 
#' @param T_a is air temperature in °C
#' @param T_g  is surface temperature in °C
#' @param u is wind speed in m/s
#' @param svl is lizard snout vent length in mm
#' @param m is lizard mass in g, note that it can be estimated as massfromsvl=function(svl) 3.55*10^-5*(svl)^3.00 (Tinkle and Ballinger 1972)
#' @param psi is solar zenith angle in degrees
#' @param rho_S is surface albedo (proportion). ~0.25 for grass, ~0.1 for dark soil, >0.75 for fresh snow (Campbell & Norman 1998)
#' @param elev is elevation in m
#' @param doy is day of year (1-366)
#' @param sun indicates whether lizard is in sun (TRUE) or shade (FALSE)
#' @param surface indicates whether lizard is on ground surface (TRUE) or above surface (FALSE, e.g. in a tree)
#' @param alpha_S is lizard solar absorptivity, alpha_S=0.9 (Gates 1980, Table 11.4)
#' @param alpha_L is lizard thermal absoptivity, alpha_L=0.965 (Bartlett & Gates 1967) 
#' @param epsilon_s is surface emisivity of lizards, epsilon_s=0.965 (Bartlett & Gates 1967)
#' @param F_d is the view factor between the surface of the lizard and diffuse solar radiation (Bartlett & Gates 1967)
#' @param F_r is the view factor between the surface of the lizard and reflected solar radiation
#' @param F_a is the view factor between the surface of the lizard and atmospheric radiation
#' @param F_g is the view factor between the surface of the lizard and ground thermal radation
#' @return T_e Operative temperature (°C)
#' @keywords body temperature biophysical model
#' @family biophysical models
#' @export 
#' @examples
#' \dontrun{
#' Tb_lizard(
#'   T_a=25, 
#'   T_g=30, 
#'   u=0.1, 
#'   svl=60, 
#'   m=10, 
#'   psi=34, 
#'   rho_S=0.24, 
#'   elev=500, 
#'   doy=200, 
#'   sun=TRUE, 
#'   surface=TRUE, 
#'   alpha_S=0.9, 
#'   alpha_L=0.965, 
#'   epsilon_s=0.965, 
#'   F_d=0.8, 
#'   F_r=0.5, 
#'   F_a=0.5, 
#'   F_g=0.5)
#'}
#' 
Tb_lizard=function(T_a, T_g, u, svl, m, psi, rho_S, elev, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5){
  
  stopifnot(u>=0, svl>=0, m>=0, rho_S>=0, rho_S<=1, elev>=0, doy>0, doy<367, sun %in% c(TRUE, FALSE), surface %in% c(TRUE, FALSE), alpha_S>=0, alpha_S<=1, alpha_L>=0, alpha_L<=1, epsilon_s>=0, epsilon_s<=1, F_d>=0, F_d<=1, F_r>=0, F_r<=1, F_a>=0, F_a<=1, F_g>=0, F_g<=1)
  
  psi= psi*pi/180 #convert zenith angle to radians
  
  # constants
  sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
  c_p=29.3 # specific heat of air, J/mol °C (p.279) Parentheses all from Campbell & Norman 1998
  
  tau=0.65 # atmospheric transmisivity
  S_p0=1360 # extraterrestrial flux density, W/m^2 (p.159)
  
  # Calculate radiation
  # view angles, parameterize for animal suspended above ground (p181), on ground- adjust F_e, F_r, and F_g
  h=svl/1000 # length of svl in m
  
  A=0.121*m^0.688   # total lizard area, Roughgarden (1981)
  A_p= (-1.1756810^-4*psi^2-9.2594*10^-2*psi+26.2409)*A/100      # projected area
  F_p=A_p/A
  
  # radiation
  p_a=101.3* exp (-elev/8200)  # atmospheric pressure
  m_a=p_a/(101.3*cos (psi))  # (11.12) optical air mass
  m_a[(psi>(80*pi/180))]=5.66
  
  # Flux densities
  epsilon_ac= 9.2*10^-6*(T_a+273)^2 # (10.11) clear sky emissivity
  L_a=sigma*(T_a+273)^4  # (10.7) long wave flux densities from atmosphere 
  L_g=sigma*(T_g+273)^4  # (10.7) long wave flux densities from ground
  
  S_d=0.3*(1-tau^m_a)* S_p0 * cos(psi)  # (11.13) diffuse radiation
  
  dd2= 1+2*0.1675*cos(2*pi*doy/365)
  S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  S_b = S_p * cos(psi)
  S_t = S_b + S_d
  S_r= rho_S*S_t # (11.10) reflected radiation
  
  #__________________________________________________
  # conductance
  
  dim=svl/1000 # characteristic dimension in meters
  g_r= 4*epsilon_s*sigma*(T_a+273)^3/c_p # (12.7) radiative conductance
  
  g_Ha=1.4*0.135*sqrt(u/dim) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976)
  
  #__________________________________________________
  # operative environmental temperature
  
  #calculate with both surface and air temp (on ground and in tree)
  
  sprop=1 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  Te=T_a+(R_abs-epsilon_s*sigma*(T_a+273)^4)/(c_p*(g_r+g_Ha))         # (12.19) Operative temperature            
  Te_surf= T_g+(R_abs-epsilon_s*sigma*(T_g+273)^4)/(c_p*(g_r+g_Ha))        
  
  # calculate in shade, no direct radiation
  sprop=0 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  TeS=T_a+(R_abs-epsilon_s*sigma*(T_a+273)^4)/(c_p*(g_r+g_Ha))         # (12.19) Operative temperature                        
  TeS_surf=T_g+(R_abs-epsilon_s*sigma*(T_g+273)^4)/(c_p*(g_r+g_Ha))  
  
  #Select Te to return
  if(sun==TRUE & surface==TRUE) Te= Te_surf
  if(sun==TRUE & surface==FALSE) Te= Te
  if(sun==FALSE & surface==TRUE) Te= TeS_surf
  if(sun==FALSE & surface==FALSE) Te= TeS
  
  return(Te) 
}
