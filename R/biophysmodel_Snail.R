#' @title Operative Environmental Temperature of a Marine Snail
#' 
#' @description The function estimates body temperature (C, operative environmental temperature) of a marine snail. The function implements a steady-state model, which assumes unchanging environmental conditions and is based on \insertCite{Iacarella2012}{TrenchR}. Body temperature and desiccation constrain the activity of Littoraria irrorata within the Spartina alterniflora canopy. The function was provided by Brian Helmuth and is a simplified version of the published model. 
#' 
#' @param temp \code{numeric} air temperature (C).
#' 
#' @param l \code{numeric} snail length (m).
#' 
#' @param S \code{numeric} direct solar flux density (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#' 
#' @param u \code{numeric} wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param CC \code{numeric} fraction of the sky covered by cloud (0-1).
#' 
#' @param WL \code{numeric} water loss rate (\ifelse{html}{\out{kg s<sup>-1</sup>}}{\eqn{kg s^-1}{ASCII}}), 5 percent loss of body mass over one hour is a reasonable maximum level \insertCite{Helmuth1999}{TrenchR}. 
#' 
#' @param WSH \code{numeric} wind sensor height (m).
#' 
#' @return \code{numeric} predicted body (operative environmental) temperature (C).
#'
#' @details Thermal radiative flux is calculated following \insertCite{Helmuth1998;textual}{TrenchR}, \insertCite{Helmuth1999;textual}{TrenchR}, and \insertCite{Idso1969;textual}{TrenchR}.
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Brian Helmuth et al.
#' 
#' @examples
#'   Tb_snail(temp  = 25, 
#'            l     = 0.012, 
#'            S = 800, 
#'            u    = 1, 
#'            CC    = 0.5, 
#'            WL    = 0, 
#'            WSH   = 10)
#' 
Tb_snail <- function (temp, 
                      l, 
                      S, 
                      u, 
                      CC, 
                      WL,
                      WSH) {
  
  stopifnot(l     >  0, 
            S >= 0, 
            u    >= 0, 
            CC    >= 0, 
            CC    <= 1, 
            WL    >= 0, 
            WSH   >= 0)
  
  sigma <- stefan_boltzmann_constant()   

  Ktemp <- celsius_to_kelvin(temp)
  Gtemp <- Ktemp # ground temperature, assume equal to air temperature
  
  # Areas

    PSA <- 3.1415 * (l / 2)^2   # 3.14*wid*Len # Projected SA (Short-wave) snails SA of circle
    SA <- 4 * 3.15 * (l / 2)^2  # Surface Area (Rad/Conv) for snails SA of sphere
  
  # Aradsky is surface area subject to long-wave radiation from sky (m^2)
  # Aradground is surface area subject to radiation from the ground (m^2)
  # Aproj is projected surface area (m^2)
  # Aground is surface area in contact with the ground (m^2)

  A1 <- (SA / 2) / PSA # Aradsky/Aproj
  A2 <- 0.05           # Aground/Aproj
  A3 <- (SA / 2) / PSA # Aradground/Aproj
  
  # Convection
  U      <- u * 0.03 # U * m/s, shear velocity
  c_prho <- 1200      # c_p * rho = 1200 J m^{-3}*K^{-1}
  k      <-von_karman_constant()
  z0 <- 0.0017       # m, roughness height
  C <- (A1 * 2 * U * c_prho * k) / (log(WSH / z0))
  
  # Absorptivities

  if (l >= 0.037){  # Absorptivity from Luke Miller 
    
    Abs <- 0.615
    
  } else {
    
    if (l <= 0.02225) {
      
      Abs <- 0.689
    
    } else {
      
      Abs <- 0.68 
      
    }
    
  } 
  
  # Emissivities

    eskyclear <- 0.72 + 0.005 * temp # Helmuth 1999 from Idso and Jackson 1969
    esky      <- eskyclear + CC * (1 - eskyclear - (8 / Ktemp))  # Helmuth 1999
    Emm       <- 0.97                                            # Emmissivity # long-wave emissivity shell
  
  # Steady-state heat balance model
  # Solve steady state energy balance equation:
  # T_b*mflux*c= Q_rad,sol +- Q_rad,sky +- Q_rad,ground +- Q_conduction +- Qconvection -Qevaporation

    com1 <- Abs * S + 4 * Emm * sigma * esky * A1 *(Ktemp^4)  + (Gtemp^4) * 4 * Emm * sigma * A3  + C * Ktemp + 0.6 * A2 * Gtemp / (l / 2) + 2.48 * WL
    com2 <- 4180 * WL + (Ktemp^3) * 4 * Emm * sigma *(esky^0.75) * A1 + 4 * Emm * sigma * A3 *(Gtemp^3) + C + 0.6 * A2 / (0.5 * l)
    T_b  <- com1 / com2
  
  kelvin_to_celsius(T_b)
  
}  



