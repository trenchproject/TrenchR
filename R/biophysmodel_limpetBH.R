#' Predicts body temperature (operative environmental temperature) of a limpet in °C.
#' @details Predicts body temperature of a limpet in °C.
#' @description Predicts body temperature of a limpet in °C. Based on Denny and Harley 2006. Hot limpets: predicting body temperature in a conductance-mediated thermal system. Function provided by Helmuth Lab. Radiation and convection is altered from original model.
#' @param T_a air temperature (°C)
#' @param T_r rock temperature (°C)
#' @param L limpet length (anterior/posterior axis) (m)
#' @param H limpet height (dorsal/ventral axis) (m)
#' @param I solar irradiance (W m^-2)
#' @param u wind speed (m/s)
#' @param s_aspect solar aspect angle (degree), the angle between the limpet's length dimension and the vector to the Sun. Between 70 and 110 degrees
#' @param s_slope solar elevation angle (degree), the altitude of the Sun, which is the angle between the horizon and the sun
#' @param c fraction of the sky covered by cloud 
#' @param position body position
#' @return predicted body temperature (°C)
#' @family biophysical models
#' @export
#' @author Brian Helmuth lab 
#' @examples
#' \dontrun{
#' Tb_limpetBH(
#'   T_a = 25, 
#'   T_r = 30, 
#'   L = 0.0176, 
#'   H = 0.0122, 
#'   I = 1300, 
#'   u = 1, 
#'   s_aspect = 90, 
#'   s_slope=60, c = 1)
#' }

Tb_limpetBH = function(T_a, T_r, L, H, I, u, s_aspect, s_slope, c, position = "anterior"){
  
  stopifnot(L > 0, H > 0, I > 0, u >= 0, s_slope >= 0, s_slope <= 90, c >= 0, c <= 1)
  
  if(s_aspect<70 | s_aspect>110)stop("Solar aspect angle should be between 70 and 110 degrees")
  
  s_aspect = s_aspect * pi / 180 # covert to radians
  s_slope = s_slope * pi / 180 # covert to radians
  T_a = T_a + 273.15   # convert to kelvin
  T_r = T_r + 273.15   # convert to kelvin
  r = L / 2            # radius
  
  #______________________________________________________________
  ## Adjust solar radiation for sun angles
  ##slope and solar angle
  r_aspect <- 257* pi / 180
  r_slope <- 44.5* pi / 180
  
  delta_i <- cos(r_slope)*cos(s_slope)*cos(s_aspect-r_aspect)+sin(r_slope)*sin(s_slope)
  
  I <- I*delta_i
  
  #______________________________________________________________
  # Short wave heat transfer
  
  # Area of the limpet’s shell (m^2) projected in the direction at which sunlight strikes the organism (Pennell and Deignan 1989)
  Ap = pi * r^2 
  
  ## short-wave absorptivity of the shell (the fraction of light energy that is absorbed) 0.615, 0.68, 0.689
  if (L >=0.037){  # Absorptivity from Luke Miller 
    alpha_sw <- 0.615
  } else {if (L <= 0.02225 ){alpha_sw <-0.689
  } else alpha_sw <- 0.68 } 

  q1 = Ap * alpha_sw * I
  
  #_______________________________________________________________
  # Long-wave energy transfer
  
  # View factor. (Campbell and Norman 1998) simulating limpets as a cone.
  Vs = 0.7
    
  Al = pi * r * sqrt(H^2 + r^2) # lateral area of a limpet shell (m^2)
  eps_ws = 0.97  #  long-wave emissivity of the shell
  sigma = 5.67 * 10^-8   # stefan-boltzmann constant (W m^-2 K^-4)

  eps_ac = 9.2 * 10^-6 * T_a^2 # clear sky emissivity (Campbell and Norman 1998, 10.11)
  eps_wa = (1 - 0.84 * c) * eps_ac + 0.84 * c  # emissivity of air with clouds (same as above, 10.12)

  q2 = Vs * Al * eps_ws * sigma * T_a^4 * (eps_wa - 1)
  q3 = 4 * Vs * Al* eps_ws * sigma * T_a^3
  
  #____________________________________________________________
  # Convective heat transfer
  
  Ka = 0.00501 + 7.2 * 10^-5 * T_a       # conductivity of air (W m^-1 K^-1) Denny and Harley. 2006, Hot limpets: predicting body temperature in a conductance-mediated thermal system 
  v = -1.25 * 10^-5 + 9.2 * 10^-8 * T_a  # kinematic viscosity of air (m^2 s^-1)
  
  Re = u * L / v  # Reynolds number
  
  if (L >=0.037){  # Absorptivity from Luke Miller 
    a <- 0.447
  } else {if (L <= 0.02225 ){a <-0.1515
  } else a <- 0.1658 } 
  
  b <- ##0.516, 0.6206, 0.6184
    if (L >=0.037){  # Absorptivity from Luke Miller 
      b <- 0.516
    } else {if (L <= 0.02225 ){b <-0.6184
    } else b <- 0.6206 } 
  
  Nu = a * Re^b    # Nusselt number
  hc = a*Ka*((u/v)^b)*(L^(b-1)) # Heat transfer coefficient (W m^-2 K^-1)
  
  A_cv = Al  # area of the shell in convective contact with the air (m^2)
  q4 = hc * A_cv
  
  #______________________________________________________________
  # Conductive heat transfer
  # Original equation uses a finite-difference approach where they divide the rock into series of chunks,
  # and calculate the temperature at each node to derive the conductive heat.
  # For simplification, here it takes the rock temperature as a parameter, and conductive heat is calculated by
  # the product of the area, thermal conductivity of rock and the difference in temperatures of the rock and the body.
  
  A_cd = pi * r^2  # area of conductive contact between the limpet’s foot and the rock (m^2)
  Kr = 3.06        # thermal conductivity of rock (W m^-1 K^-1)
  q5 = A_cd * Kr
  
  # Calculating for body temperature using the coefficients q1 ~ q5.
  T_b = (q1 + q2 + (q3 + q4)* T_a + q5 * T_r) / (q3 + q4 + q5)
  
  return (T_b - 273.15)
}
