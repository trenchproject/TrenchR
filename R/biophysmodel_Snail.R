#' Predicts body temperature (operative environmental temperature) of a marine snail in °C.
#' 
#' 
#' @details Predicts body temperature of a snail in °C.
#' @description Predicts body temperature of a snail in °C. Implements a steady‐state model, which assumes unchanging environmental conditions. Based on Iacarella and Helmuth 2012. Body temperature and desiccation constrain the activity of Littoraria irrorata within the Spartina alterniflora canopy. Function was provided by the Helmuth lab and is a simplified version of model in publication. 
#' @param temp air temperature (°C)
#' @param Len snail length (m)
#' @param solar direct solar flux density (W/m2)
#' @param WS wind speed (m/s)
#' @param CC fraction of the sky covered by cloud (0-1)
#' @param WL water loss rate (kg/s), 5 percent loss of body mass over one hour is a reasonable maximum level (Helmuth 1999)
#' @param WSH wind sensor height (m)
#' @return predicted body temperature (°C)
#' @keywords body temperature biophysical model
#' @family biophysical models
#' @export
#' @author Brian Helmuth Lab
#' @examples
#' \dontrun{
#' Tb_snail(temp = 25, Len = 0.012, solar=800, WS=1, CC=0.5, WL=0, WSH=10)
#' }

Tb_snail = function(temp, Len, solar, WS, CC, WL, WSH){
  
  stopifnot(Len>0, solar>=0, WS>= 0, CC>=0, CC<=1, WL>=0, WSH>=0)
  
  #temperatures
  Ktemp <- temp + 273 #temperature in Kelvin
  Gtemp <- Ktemp #ground temperature, assume equal to air temperature
  
  #areas
  PSA <- 3.1415*((Len/2)^2) ##3.14*wid*Len # Projected SA (Short-wave) snails SA of circle
  SA <- 4*3.15*((Len/2)^2) # Surface Area (Rad/Conv) for snails SA of sphere
  
  #Aradsky is surface area subject to long-wave radiation from sky (m^2)
  #Aradground is surface area subject to radiation from the ground (m^2)
  #Aproj is projected surface area (m^2)
  #Aground is surface area in contact with the ground (m^2)
  A1 <- (SA/2)/PSA # Aradsky/Aproj
  A2 <- 0.05 # Aground/Aproj`
  A3 <- (SA/2)/PSA # Aradground/Aproj
  
  #constants
  SB <- 5.67E-08 # Stephan Boltzman constant
  
  #convection
  u <- WS*0.03 # U* m/s, shear velocity
  c_prho=1200 #c_p*rho=1200 J m^{-3}*K^{-1}
  k=0.4 #von Karman constant
  z0=0.0017 #m, roughness height
  C <- (A1*2*u*c_prho*k)/(log(WSH/z0))
  
  #absorptivities
  if (Len >=0.037){  # Absorptivity from Luke Miller 
    Abs <- 0.615
  } else {if (Len <= 0.02225 ){Abs <-0.689
  } else Abs <- 0.68 } 
  
  # emissivities
  eskyclear = 0.72 +0.005*temp # Helmuth 1999 from Idso and Jackson 1969
  esky <- eskyclear + CC*(1 - eskyclear - (8 / Ktemp))  # Helmuth 1999
  Emm <- 0.97 # Emmissivity # long-wave emissivity shell
  
  # Steady-state heat balance model
  # Solve steady state energy balance equation:
  # T_b*mflux*c= Q_rad,sol +- Q_rad,sky +- Q_rad,ground +- Q_conduction +- Qconvection -Qevaporation
  com1 <- Abs * solar + 4 * Emm * SB * esky * A1 *(Ktemp^4)  + (Gtemp^4) * 4 * Emm * SB * A3  + C * Ktemp + 0.6 * A2 * Gtemp / (Len/2) + 2.48 * WL
  com2 <- 4180 * WL + (Ktemp^3) * 4 * Emm * SB *(esky^0.75) * A1 + 4 * Emm * SB * A3 *(Gtemp^3) + C + 0.6 * A2/(0.5*Len)
  T_b <- com1 / com2
  
  return (T_b - 273.15)
}  
