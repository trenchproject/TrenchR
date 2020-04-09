# Main issue with this function is that the mflux is not calculated properly.
# changing rho_diff from 0.0001 to 0.001 to 0.01 produces an outcome of 45, 439, -335 °C respectively.

# The potential reason for this flaw is that hm is set to be equal to hc as the text just says
# "values of hm are generally very similar to those of hc due to similarities in the diffusivities of 
# heat and water vapor in air."
# To begin with, it's hard to imagine that users will be able to get information on "rho_diff" and "evap", 
# which all matter to the value of mflux.
# When gaping is ignored, the function behaves reasonably.

#' Predicts body temperature (operative environmental temperature) of a mussel in °C.
#' @details Predicts body temperature of a mussel in °C.
#' @description Predicts body temperature of a mussel in °C. Implements a steady‐state model, which assumes unchanging environmental conditions. Based on Helmuth 1998, INTERTIDAL MUSSEL MICROCLIMATES: PREDICTING THE BODY TEMPERATURE OF A SESSILE INVERTEBRATE
#' @param L mussel length (anterior/posterior axis) (m)
#' @param H mussel height (dorsal/ventral axis) (m)
#' @param T_a air temperature (°C)
#' @param T_g ground temperature (°C)
#' @param u wind speed (m/s)
#' @param p atmospheric vapor pressure (atm)
#' @param psi solar zenith angle (degrees): can be calculated from zenith_angle function
#' @param elev elevation (m)
#' @param evap proportion of area of mass exchange to total surface area, determined by mussel gape
#' @param rho_diff vapor density of mussel body - that of air (kg m^-3)
#' @param cl fraction of the sky covered by cloud 
#' @param group options are "aggregated": mussels living in beds, "solitary": mussels individuals, anterior or posterior end facing upwind, 
#'              and "solitary_valve": solitary individuals, valve facing upwind
#' @return predicted body temperature (°C)
#' @keywords body temperature, biophysical model
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Tb_mussel(L = 0.1, H = 0.05, T_a = 25, T_g = 30, u = 2, p = 0.03, psi = 30, elev = 500, evap = 0.2, rho_diff = 0.0001, cl = 0, group = "solitary")
#' }

Tb_mussel = function(L, H, T_a, T_g, u, p, psi, elev, evap, rho_diff = 0.0001, cl, group = "solitary"){
  
  stopifnot(L > 0, H > 0, u >= 0, p > 0, psi >= 0, psi <= 90, evap >= 0, evap <= 1, rho_diff >= 0, cl >= 0, cl <= 0, group %in% c("aggregated", "solitary", "solitary_valve"))
  
  T_a = T_a + 273.15   # conversion to kelvin
  T_g = T_g + 273.15   # conversion to kelvin
  A = 1.08 * L^2 + 0.0461 * L - 0.0016   # total mussel shell surface area (m^2)
  psi = psi * pi / 180  # conversion to radians
  
  #____________________________________________________________
  # constants
  sigma = 5.67 * 10^-8   # stefan-boltzmann constant (W m^-2 K^-4)
  lambda = 2.48          # latent heat of vaporization of water (J/kg)
  c = 4180               # specific heat of water (J kg^-1 K^-1)
  
  #___________________________________________________________
  #Short-wave solar flux  
  alpha = 0.75           # solar absorptivity
  k1 = alpha * sin(pi / 4 - psi)
  rho_s = 0.08  # albedo (Campbell and Norman 1998 p.172, soil, wet dark)
  
  p_a = 101.3 * exp (-elev / 8200)  # atmospheric pressure
  m_a = p_a / (101.3 * cos(psi))  # (same as above 11.12) optical air mass
  m_a[(psi > (80 * pi / 180))] = 5.66
  
  # atmospheric transmisivity  (same as above p.173)
  if (cl > 0.7) {
    tau = 0.4
  } else if (cl < 0.1) {
    tau = 0.75
  } else {
    tau = 0.65
  }
  
  S_p0 = 1360 # extraterrestrial flux density, W/m^2 (Campbell and Norman 1998 p.159)
  
  S_d = 0.3 * (1 - tau^m_a) * S_p0 * cos(psi)  # (same as above 11.13) diffuse radiation
  
  S_p = S_p0 * tau^m_a * cos(psi)    
  S_b = S_p * cos(psi)
  S_t = S_b + S_d
  
  S_r = rho_s * S_t # albedo flux density (same as above 11.10)
  
  #____________________________________________________________
  # Long-wave radiaion
  
  # emissivities
  eps_ac = 9.2 * 10^-6 * T_a^2 # clear sky emissivity (Campbell and Norman 1998, 10.11)
  eps_sky = (1 - 0.84 * cl) * eps_ac + 0.84 * cl  # functional infrared emissivity of sky (same as above, 10.12)
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
  
  
  # hm: a coefficient of mass transfer (m/s)
  # "values of hm are generally very similar to those of hc due to similarities in the diffusivities of 
  # heat and water vapor in air"
  hm = hc
  A_evap = A * evap   # area of mass exchange (m^2)
  mflux = hm * A_evap * rho_diff
  
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
  # No need to separate shell and body if we are thinking in steady state. All it matters is the change in mass (mflux)
  
  # Solve steady state energy balance equation:
  # T_b*mflux*c= Q_rad,sol +- Q_rad,sky +- Q_rad,ground +- Q_conduction +- Qconvection -Qevaporation
  
  T_b = (k1 * (A_sol * S_p + A_d * (S_r + S_d)) + k2 * A_radSky * k3 * T_a^4 + k4 * A_radGround * T_g^4 + k5 * A_cond * T_g + 
           hc * A_conv * T_a - lambda * mflux) / (k2 * A_radSky * T_a^3 + k4 * A_radGround * T_g^3 + k5 * A_cond + 
                                                    hc * A_conv - mflux * c)
  
  return (T_b - 273.15)
}

Tb_mussel(L = 0.1, H = 0.05, T_a = 25, T_g = 30, u = 2, p = 0.03, psi = 30, elev = 500, evap = 0.2, rho_diff = 0.01, cl = 0, group = "solitary")
