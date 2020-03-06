#' Predicts body temperature (operative environmental temperature) of a mussel in °C.
#' @details Predicts body temperature of a mussel in °C.
#' @description Predicts body temperature of a mussel in °C. Based on Helmuth 1998, INTERTIDAL MUSSEL MICROCLIMATES: PREDICTING THE BODY TEMPERATURE OF A SESSILE INVERTEBRATE
#' @param L mussel length (anterior/posterior axis) (m)
#' @param H mussel height (dorsal/ventral axis) (m)
#' @param u wind speed (m/s)
#' @param psi solar zenith angle (degrees): can be calculated from zenith_angle function
#' @param S direct solar flux density acting upon that area (W m^-2)
#' @param c fraction of the sky covered by cloud 
#' @param group TRUE if aggregated, FALSE if solitary 
#' @return predicted body temperature (°C)
#' @keywords body temperature, biophysical model
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Tb_mussel(L = 0.1, H = 0.05, T_a = 25, T_g = 30, u = 2, psi = 30, S = 500, c = 0, group = FALSE)
#' }

Tb_mussel = function(L, H, T_a, T_g, u, psi, S, c, group = TRUE){
  
  stopifnot(L > 0, H > 0, u >= 0, psi >= 0, psi <= 90, S > 0, c >= 0, c <= 0, group %in% c(TRUE, FALSE))
  
  T_a = T_a + 273.15   # conversion to kelvin
  T_g = T_g + 273.15   # conversion to kelvin
  A = pi * (L / 2)^2   # total mussel shell surface area (m^2)
  alpha = 0.75         # solar absorptivity
  k1 = alpha * sin((90 - psi)*pi / 180)
  
  sigma = 5.66 * 10^-8   # stefan-boltzmann constant (W m^-2 K^-4)
  eps_org = 0.97         # infrared emissivity of organism (Campbell and Norman 1998, p.163)
  
  eps_ac = 9.2 * 10^-6 * T_a^2 # clear sky emissivity (same as above, 10.11)
  eps_sky = (1 - 0.84 * c) * eps_ac + 0.84 * c  # functional infrared emissivity of sky (same as above, 10.12)

  
  k2 = 4 * sigma * eps_org * eps_sky^(3/4)
  k3 = eps_sky^(1/4)
  k4 = 4 * sigma * eps_org
  
  kb = 0.6  # thermal conductivity of heat in body (W m^-2 K^-1). Approximated to that of water because mussels are mostly made of water
  k5 = kb / (0.5 * H)
  
  if (group) {
    A_sol = 0.15 * A   # projected area in direction of sun (m^2)
  } else {
    A_sol = 0.25 * A
  }
  
  A_radSky = A / 2    # surface area subject to long-wave radiation from sky (m^2). Half facing the sky, the other half facing the ground
  A_radGround = A / 2 # surface area subject to long-wave radiation from ground (m^2)
  A_cond = 0.05 * A   # area of contact between mussel and ground (m^2)
  A_conv = A          # surface area exposed to convective heat loss (m^2)
  
  lambda = 2.48 # latent heat of vaporization of water (J/kg)
  
  Ka = 0.00501 + 7.2 * 10^-5 * T_a      # Denny and Harly. 2006, Hot limpets: predicting body temperature in a conductance-mediated thermal system 
  v = -1.25 * 10^-5 + 9.2 * 10^-8 * T_a
  
  if (group) {        # derived from the relationship between Nusselt number and Reynolds number
    hc = 0.67 * Ka / L * (u * L / v)^0.42
  } else {
    hc = 0.38 * Ka / L * (u * L / v)^0.51
  }
  
  T_b = (k1 * A_sol * S + k2 * A_radSky * k3 * T_a^4 + k4 * A_radGround * T_g^4 + k5 * A_cond * T_g + 
    hc * A_conv * T_a) / (k2 * A_radSky * T_a^3 + k4 * A_radGround * T_g^3 + k5 * A_cond + hc * A_conv)
  
  return (T_b - 273.15)
}
