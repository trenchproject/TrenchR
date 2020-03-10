#' Predicts body temperature (operative environmental temperature) of a mussel in °C.
#' @details Predicts body temperature of a mussel in °C.
#' @description Predicts body temperature of a mussel in °C. Based on Helmuth 1998, INTERTIDAL MUSSEL MICROCLIMATES: PREDICTING THE BODY TEMPERATURE OF A SESSILE INVERTEBRATE
#' @param L mussel length (anterior/posterior axis) (m)
#' @param H mussel height (dorsal/ventral axis) (m)
#' @param T_a air temperature (°C)
#' @param T_g ground temperature (°C)
#' @param u wind speed (m/s)
#' @param p atmospheric vapor pressure (atm)
#' @param psi solar zenith angle (degrees): can be calculated from zenith_angle function
#' @param elev elevation (m)
#' @param A_evap area of mass exchange, determined by mussel gape (m^2)
#' @param rho_body vapor density of the mussel body (kg m^-3)
#' @param cl fraction of the sky covered by cloud 
#' @param group options are "aggregated": mussels living in beds, "solitary": mussels individuals, anterior or posterior end facing upwind, 
#'              and "solitary_valve": solitary individuals, valve facing upwind
#' @return predicted body temperature (°C)
#' @keywords body temperature, biophysical model
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Tb_mussel(L = 0.1, H = 0.05, T_a = 25, T_g = 30, u = 2, p = 0.03, psi = 30, elev == 500, A_evap = , rho_body = , cl = 0, group = "solitary)
#' }

Tb_mussel = function(L, H, T_a, T_g, u, p, psi, elev, A_evap, rho_body, cl, group = "solitary"){
  
  #stopifnot(L > 0, H > 0, u >= 0, psi >= 0, psi <= 90, S > 0, c >= 0, c <= 0, group %in% c(TRUE, FALSE))
  
  T_a = T_a + 273.15   # conversion to kelvin
  T_g = T_g + 273.15   # conversion to kelvin
  A = pi * (L / 2)^2   # total mussel shell surface area (m^2)
  
  #____________________________________________________________
  # constants
  sigma = 5.66 * 10^-8   # stefan-boltzmann constant (W m^-2 K^-4)
  lambda = 2.48          # latent heat of vaporization of water (J/kg)
  c = 4180               # specific heat of mussel body (J kg^-1 K^-1)
  
  #___________________________________________________________
  #Short-wave solar flux  
  alpha = 0.75           # solar absorptivity
  k1 = alpha * sin((90 - psi)*pi / 180)
  rho_s = 0.08  # albedo (p.172, soil, wet dark)
  
  p_a = 101.3 * exp (-elev / 8200)  # atmospheric pressure
  m_a = p_a / (101.3 * cos(psi))  # (11.12) optical air mass
  m_a[(psi > (80 * pi / 180))] = 5.66
  
  # atmospheric transmisivity
  if (cl > 0.7) {
    tau = 0.4
  } else if (cl < 0.1) {
    tau = 0.75
  } else {
    tau = 0.65
  }
  
  S_p0 = 1360 # extraterrestrial flux density, W/m^2 (p.159)
  
  S_d = 0.3 * (1 - tau^m_a) * S_p0 * cos(psi)  # (11.13) diffuse radiation
  
  S_p = S_p0 * tau^m_a * cos(psi)    
  S_b = S_p * cos(psi)
  S_t = S_b + S_d
  
  S_r = rho_s * S_t # albedo flux density (11.10)

  
  #____________________________________________________________
  # Long-wave radiaion

  # emissivities
  eps_ac = 9.2 * 10^-6 * T_a^2 # clear sky emissivity (Campbell and Norman 1998, 10.11)
  eps_sky = (1 - 0.84 * cl) * eps_ac + 0.84 * cl  # functional infrared emissivity of sky (same as above, 10.12)
  eps_org = 0.97         # infrared emissivity of organism (Campbell and Norman 1998, p.163)
  
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
  
  # hc: coefficient for forced convection
  d = L * 2 / 3  # average body dimensions
  if (group == "aggregated") {        # derived from the relationship between Nusselt number and Reynolds number
    hc = 0.67 * Ka / d * (u * d / v)^0.42
  } else if (group == "solitary"){
    hc = 0.38 * Ka / d * (u * d / v)^0.51
  } else {
    hc = 0.63 * Ka / d * (u * d / v)^0.47
  }
  
  #___________________________________________________________
  # Evaporation
  # calculating vapor density
  # From pV = nRT, n/V = p/RT (moles/L) where p is vapor pressure (atm), 
  # R is the ideal gas constant 0.0821 (L atm/K mol), T is the air temperature (K)
  # moles/L * 1000 m^3/L * 18 g/moles * 0.001 kg/g = kg/m^3

  rho_air = p / 0.0821 / T_a * 1000 * 18 * 0.001
  
  # hm: a coefficient of mass transfer
  # "values of hm are generally very similar to those of hc due to similarities in the diffusivities of 
  # heat and water vapor in air"
  hm = hc
  
  mflux = hm * A_evap * (rho_body - rho_air)
  
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
  
  T_b = (k1 * (A_sol * S_p + A_d * (S_r + S_d)) + k2 * A_radSky * k3 * T_a^4 + k4 * A_radGround * T_g^4 + k5 * A_cond * T_g + 
    hc * A_conv * T_a - lambda * mflux) / (k2 * A_radSky * T_a^3 + k4 * A_radGround * T_g^3 + k5 * A_cond + 
                                             hc * A_conv - mflux * c)
  
  return (T_b - 273.15)
}
