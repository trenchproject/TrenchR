#' Predicts body temperature (operative environmental temperature) of a mussel in °C.
#' 
#' 
#' @details Predicts body temperature of a mussel in °C.
#' @description Predicts body temperature of a mussel in °C. Implements a steady‐state model, which assumes unchanging environmental conditions. Based on Helmuth 1998, INTERTIDAL MUSSEL MICROCLIMATES: PREDICTING THE BODY TEMPERATURE OF A SESSILE INVERTEBRATE
#' @param L mussel length (anterior/posterior axis) (m)
#' @param H mussel height (dorsal/ventral axis) (m), reasonable to assume 0.5*L
#' @param T_a air temperature (°C)
#' @param T_g ground temperature (°C)
#' @param S direct solar flux density (W/m2)
#' @param k_d diffuse fraction, proportion of solar radiation that is diffuse
#' @param u wind speed (m/s)
#' @param psi solar zenith angle (degrees): can be calculated from zenith_angle function
#' @param evap Are mussels gaping to evaporatively cool? TRUE of FALSE (default), If TRUE, assumes constant mass loss rate of 5% of initial body mass per hour 
#' @param cl fraction of the sky covered by cloud 
#' @param group options are "aggregated": mussels living in beds, "solitary": mussels individuals, anterior or posterior end facing upwind, 
#'              and "solitary_valve": solitary individuals, valve facing upwind
#' @return predicted body temperature (°C)
#' @keywords body temperature, biophysical model
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Tb_mussel(L = 0.1, H = 0.05, T_a = 25, T_g = 30, S=500, k_d=0.2, u = 2, psi = 30, evap = FALSE, cl = 0.5, group = "solitary")
#' }

Tb_mussel = function(L, H, T_a, T_g, S, k_d, u, psi, evap=FALSE, cl, group = "solitary"){
  
  stopifnot(L > 0, H > 0, u >= 0, psi >= 0, psi <= 90, evap %in% c("TRUE","FALSE"), cl >= 0, cl <= 1, group %in% c("aggregated", "solitary", "solitary_valve"))
  
  T_a = T_a + 273.15   # conversion to kelvin
  T_g = T_g + 273.15   # conversion to kelvin
  A = 1.08 * L^2 + 0.0461 * L - 0.0016   # total mussel shell surface area (m^2)
  m= 191*L^3.53  #mussel body mass, kg
  psi = psi * pi / 180  # conversion to radians
  
  #____________________________________________________________
  # constants
  sigma = 5.67 * 10^-8   # stefan-boltzmann constant (W m^-2 K^-4)
  lambda = 2.48         # latent heat of vaporization of water (J/kg)
  c = 4180               # specific heat of water (J kg^-1 K^-1)
  
  #___________________________________________________________
  #Short-wave solar flux  
  alpha = 0.75           # solar absorptivity
  k1 = alpha / sin(psi)
  
  S_p= S*(1-k_d) #direct radiation
  S_d= S*(k_d) #diffuse radiation
  #omit reflected radiation
  
  #____________________________________________________________
  # Long-wave radiaion
  
  # emissivities
  eps_ac = 0.72 +0.005*(T_a-273) # Helmuth 1999 from Idso and Jackson 1969
  eps_sky = eps_ac + cl*(1-eps_ac-8/T_a)  # Helmuth 1999
  eps_org = 1.0         # infrared emissivity of organism (same as above, p.163)
  
  #Estimate lumped coefficients
  k2 = 4 * sigma * eps_org * eps_sky^(3/4)
  k3 = eps_sky^(1/4)
  k4 = 4 * sigma * eps_org
  
  # Conduction (Coefficient)
  kb = 0.6      # thermal conductivity of heat in body (W m^-2 K^-1). Approximated to that of water because mussels are mostly made of water
  k5 = kb / (0.5 * H)
  
  #_______________________________________________________________
  # Convection
  # Denny and Harley. 2006, Hot limpets: predicting body temperature in a conductance-mediated thermal system 
  Ka = 0.00501 + 7.2 * 10^-5 * T_a        # Conductivity of air (W m^-1 K^-1)
  v = -1.25 * 10^-5 + 9.2 * 10^-8 * T_a   # Kinematic viscosity of air (m^2 s^-1)
  
  d = L * 2 / 3   # average body dimensions (Helmuth 1998 p.74)
  Re = u * d / v  # Reynolds number
  
  if (group == "aggregated") {
    a = 0.67
    b = 0.42
  } else if (group == "solitary"){
    a = 0.38
    b = 0.51
  } else {
    a = 0.63
    b = 0.47
  }
  Nu = a * Re^b    # Nusselt number
  hc = Nu * Ka / d     # heat transfer coefficient (W m^-2 K^-1)
  
  #evaporative mass loss
  mflux= ifelse(evap==FALSE, 0, m*0.05/(60*60) )
  # set maximum mflux rate to 5% of body mass over 1 hour, level that results in dessication
  # see Helmuth 1998 for a more detailed evaporation model based on mussel gaping
  
  #____________________________________________________________
  # calculating areas of interest
  A_radSky = A / 2    # surface area subject to long-wave radiation from sky (m^2). Half facing the sky, the other half facing the ground
  A_radGround = A / 2 # surface area subject to long-wave radiation from ground (m^2)
  A_cond = 0.05 * A   # area of contact between mussel and ground (m^2)
  A_conv = A          # surface area exposed to convective heat loss (m^2)
  A_d = A / 2         # surface area exposed to diffuse and albedo flux (m^2)
  
  if (group == "aggregated") {
    A_sol = 0.15 * A   # projected area in direction of sun (m^2)
  } else {
    A_sol = 0.25 * A
  }
  
  # Steady-state heat balance model
 
  # Solve steady state energy balance equation:
  # T_b*mflux*c= Q_rad,sol +- Q_rad,sky +- Q_rad,ground +- Q_conduction +- Qconvection -Qevaporation
  
  T_b = (k1 * (A_sol * S_p + A_d * S_d) + k2 * A_radSky * k3 * T_a^4 + k4 * A_radGround * T_g^4 + k5 * A_cond * T_g + 
           hc * A_conv * T_a - lambda * mflux) / (k2 * A_radSky * T_a^3 + k4 * A_radGround * T_g^3 + k5 * A_cond + 
                                                    hc * A_conv + mflux * c)
  
  return (T_b - 273.15)
}  

t.seq <- lapply(20:40, FUN = Tb_mussel, L = 0.1, H = 0.05, T_g = 30, S=500, k_d=0, u = 1, psi =60, evap = FALSE, cl = 0, group = "solitary")
plot(20:40, t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)")

t.seq <- lapply(20:40, FUN = Tb_mussel, L = 0.1, H = 0.05, T_g = 30, S=500, k_d=0, u = 1, psi =60, evap = TRUE, cl = 0, group = "solitary")
points(20:40, t.seq, type = "l", lty="dashed")

abline(a=0,b=1, col="gray")

#----
t.seq <- lapply(seq(0.02,0.14,0.01), FUN = Tb_mussel, T_a=25, H = 0.05, T_g = 30, S=500, k_d=0, u = 1, psi =60, evap = FALSE, cl = 0, group = "solitary")
plot(seq(0.02,0.14,0.01), t.seq, type = "l", xlab = "ambient temperature (°C)", ylab = "body temperature (°C)", ylim=c(25,32))

t.seq <- lapply(seq(0.02,0.14,0.01), FUN = Tb_mussel, T_a=25, H = 0.05, T_g = 30, S=500, k_d=0, u = 0.5, psi =60, evap = FALSE, cl = 0, group = "solitary")
points(seq(0.02,0.14,0.01), t.seq, type = "l", lty="dashed")

t.seq <- lapply(seq(0.02,0.14,0.01), FUN = Tb_mussel, T_a=25, H = 0.05, T_g = 30, S=500, k_d=0, u = 3, psi =60, evap = FALSE, cl = 0, group = "solitary")
points(seq(0.02,0.14,0.01), t.seq, type = "l", lty="dotted")

t.seq <- lapply(seq(0.02,0.14,0.01), FUN = Tb_mussel, T_a=25, H = 0.05, T_g = 30, S=500, k_d=0, u = 1, psi =60, evap = TRUE, cl = 0, group = "solitary")
points(seq(0.02,0.14,0.01), t.seq, type = "l", col="red")
