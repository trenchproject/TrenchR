#' Predicts body temperature (operative environmental temperature) of a mussel bed in °C.
#' 
#' 
#' @details Predicts body temperature of a mussel in °C.
#' @description Predicts body temperature of a mussel in °C. Implements a steady‐state model, which assumes unchanging environmental conditions. Based on Helmuth 1999. Thermal biology of rocky intertidal mussels: quantifying body temperatures using climatological data. Ecology 80:15-34.
#' @param L mussel length (anterior/posterior axis) (m)
#' @param T_a air temperature (°C) at 4m
#' @param S direct solar flux density (W/m2)
#' @param k_d diffuse fraction, proportion of solar radiation that is diffuse
#' @param u wind speed (m/s) at 4m
#' @param evap Are mussels gaping to evaporatively cool? TRUE of FALSE (default), If TRUE, assumes constant mass loss rate of 5% of initial body mass per hour 
#' @param cl fraction of the sky covered by cloud, optional
#' @return predicted body temperature (°C)
#' @keywords body temperature, biophysical model
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Tbed_mussel(L = 0.1, T_a = 25, S=500, k_d=0.2, u = 1, evap = FALSE)
#' }

Tbed_mussel = function(L, T_a, S, k_d, u, evap=FALSE, cl=NA){
  
  stopifnot(L >= 0, S >= 0, k_d>=0, k_d<=1, u>0, evap %in% c("TRUE","FALSE") )
  
  T_a = T_a + 273.15   # conversion to kelvin
  
  # constants
  sigma = 5.67 * 10^-8   # stefan-boltzmann constant (W m^-2 K^-4)
  lambda = 2.48         # latent heat of vaporization of water (J/kg)
  c = 4180               # specific heat of water (J kg^-1 K^-1)
  
  #Areas
  A = 1.08 * L^2 + 0.0461 * L - 0.0016   # total mussel shell surface area (m^2)
  #projected area for tightly packed bed
  Ap= 0.15*A
  #assume 40% exposed to ground, 40% exposed to sky, and 20% to other mussels
  A_radSky= 0.4*A
  A_radGround= 0.4*A
  #Area exposed to diffuse solar radiation
  A_solDiff=0.4*A
  #Area exposed to convection
  Aconv= 0.5*A
  
  #convection 
  u_star= 0.03*u #m/s, shear velocity
  c_prho=1200 #c_p*rho=1200 J m^{-3}*K^{-1}
  k=0.4 #von Karman constant
  z=4 #environmental input is at height of 4m
  z0=0.0017 #m, roughness height
  
  C= (Aconv/Ap)*(u_star*c_prho*k)/(log(z/z0))
  
  #conduction is considered negligible due to small area of contact
  
  # emissivities
  eps_ac = 0.72 +0.005*(T_a-273) # Helmuth 1999 from Idso and Jackson 1969
  eps_sky = ifelse( is.na(cl), 0.9, eps_ac + cl*(1-eps_ac-8/T_a))  # Helmuth 1999, assume 0.9 if no cloudiness data
  
  eps_org = 1.0         # infrared emissivity of organism (same as above, p.163)
  
  #evaporation
  #assume constant mass loss rate of 5% initial body mass per hour, assuming a density of 700 mussels/m2, and L=0.075
  mflux= ifelse(evap==FALSE, 0, 1.99*10^{-4} )  #kg/sm^2
  
  #radiation
  alpha=0.75   # solar absorptivity
  
  S_dir= S*(1-k_d) #direct radiation
  S_diff= S*(k_d) #diffuse radiation
  
  solar= alpha*(S_dir + (A_solDiff/Ap)*S_diff)
  Rad_s= 4*sigma*(A_radSky/Ap)*eps_sky^0.75
  Rad_g= 4*sigma*(A_radGround/Ap)
  
  #-----
  # Steady-state heat balance model
 
  # Solve steady state energy balance equation:
  # T_b*mflux*c= Q_rad,sol +- Q_rad,sky +- Q_rad,ground +- Q_conduction +- Qconvection -Qevaporation
  
  T_bed= (solar+T_a^4*(Rad_s+Rad_g)+C*T_a-lambda*mflux)/(mflux*c+T_a^3*(Rad_s+Rad_g)+C)
 
  return (T_bed - 273.15)
}  
