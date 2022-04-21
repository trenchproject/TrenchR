#' @title Operative Environmental Temperature of a Mussel
#' 
#' @description The function estimates body temperature (C, operative environmental temperature) of a mussel. The function implements a steady-state model, which assumes unchanging environmental conditions. 
#' 
#' @param l \code{numeric} mussel length (anterior/posterior axis, m).
#' 
#' @param h \code{numeric} mussel height (dorsal/ventral axis, m). It is reasonable to assume \code{h = 0.5 * l}.
#' 
#' @param T_a \code{numeric} air temperature (C).
#' 
#' @param T_g \code{numeric} ground temperature (C).
#' 
#' @param S \code{numeric} direct solar flux density (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#' 
#' @param k_d \code{numeric} diffuse fraction, proportion of solar radiation that is diffuse.
#' 
#' @param u \code{numeric} wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param psi \code{numeric} solar zenith angle (degrees): can be calculated from \code{\link{zenith_angle}}.
#' 
#' @param evap \code{logical} Whether mussel is gaping to evaporatively cool. If \code{TRUE}, the function assumes a constant mass loss rate of 5 percent of the initial body mass per hour. 
#' 
#' @param cl \code{numeric} fraction of the sky covered by cloud. 
#' 
#' @param group \code{character}; options are \code{"aggregated"}: mussels living in beds; \code{"solitary"}: solitary individual, anterior or posterior end facing upwind; and \code{"solitary_valve"}: solitary individual, valve facing upwind.
#'
#' @details Thermal radiative flux is calculated following \insertCite{Helmuth1998;textual}{TrenchR}, \insertCite{Helmuth1999;textual}{TrenchR}, and \insertCite{Idso1969;textual}{TrenchR}.
#'
#' @return \code{numeric} predicted body (operative environmental) temperature (C).
#' 
#' @family biophysical models
#' 
#' @references
#'  \insertAllCited{}
#'
#' @export
#' 
#' @examples
#' 
#' Tb_mussel(l     = 0.1, 
#'           h     = 0.05, 
#'           T_a   = 25, 
#'           T_g   = 30, 
#'           S     = 500, 
#'           k_d   = 0.2, 
#'           u     = 2, 
#'           psi   = 30, 
#'           evap  = FALSE, 
#'           cl    = 0.5, 
#'           group = "solitary")
#' 
Tb_mussel <- function (l, 
                       h, 
                       T_a, 
                       T_g, 
                       S, 
                       k_d, 
                       u, 
                       psi, 
                       cl, 
                       evap  = FALSE, 
                       group = "solitary") {
  
  stopifnot(l   >  0, 
            h   >  0, 
            u   >= 0, 
            S   >= 0, 
            psi >= 0, 
            psi <= 90, 
            cl  >= 0, 
            cl  <= 1, 
            k_d >= 0, 
            k_d <= 1, 
            is.logical(evap), 
            length(group) == 1,
            group %in% c("aggregated", "solitary", "solitary_valve"))
  
  T_a <- celsius_to_kelvin(T_a)
  T_g <- celsius_to_kelvin(T_g)

  # total mussel shell surface area (m^2)

    A <- 1.08 * l^2 + 0.0461 * l - 0.0016   

  # mussel body mass, kg
  
    m <- 191 * l^3.53  

  psi <- degrees_to_radians(psi)
  
  # constants

    sigma <- stefan_boltzmann_constant()  
    lambda <- latent_heat_vaporization_h2o()                   
  
  #  Short-wave solar flux  

    # solar absorptivity

      alpha <- 0.75           
      k1 <- alpha / sin(psi)
  
  # direct radiation

    S_p <- S * (1 - k_d) 

  # diffuse radiation

    S_d <- S * (k_d)

  # omit reflected radiation
  
  # Long-wave radiaion
  
    # emissivities

      # Helmuth 1999 from Idso and Jackson 1969

        eps_ac <- 0.72 + 0.005 * (T_a - 273) 

      # Helmuth 1999

        eps_sky <- eps_ac + cl * (1 - eps_ac - 8 / T_a)

      # infrared emissivity of organism (same as above, p.163)

        eps_org <- 1.0         
  
  # Estimate lumped coefficients

    k2 <- 4 * sigma * eps_org * eps_sky^(3/4)
    k3 <- eps_sky^(1/4)
    k4 <- 4 * sigma * eps_org
  
  # Conduction (Coefficient)
  #   thermal conductivity of heat in body (W m^-2 K^-1). 
  #   Approximated to that of water because mussels are mostly made of water

    kb <- 0.6      
    k5 <- kb / (0.5 * h)
  
  # Convection
  # Denny and Harley. 2006, Hot limpets: predicting body temperature in a conductance-mediated thermal system 

  # Conductivity of air (W m^-1 K^-1)

    Ka <- 0.00501 + 7.2 * 10^-5 * T_a        

  # Kinematic viscosity of air (m^2 s^-1)

    v <- -1.25 * 10^-5 + 9.2 * 10^-8 * T_a   
  
  # average body dimensions (Helmuth 1998 p.74)

    d <- l * 2 / 3   

  # Reynolds number

    Re <- u * d / v
  
  if (group == "aggregated") {

    a <- 0.67
    b <- 0.42

  } else if (group == "solitary"){

    a <- 0.38
    b <- 0.51

  } else {

    a <- 0.63
    b <- 0.47

  }

  # Nusselt number

    Nu <- a * Re^b   

  # heat transfer coefficient (W m^-2 K^-1)

    hc <- Nu * Ka / d     
  
  # evaporative mass loss
  #
  #   set maximum mflux rate to 5% of body mass over 1 hour, level that results in dessication
  #   see Helmuth 1998 for a more detailed evaporation model based on mussel gaping

    mflux <- ifelse(evap == FALSE, 0, m * 0.05 / (60 * 60))

  # Calculating areas of interest

    # surface area subject to long-wave radiation from sky (m^2). Half facing the sky, the other half facing the ground

      A_radSky <- A / 2    

    # surface area subject to long-wave radiation from ground (m^2)

      A_radGround <- A / 2 

    # area of contact between mussel and ground (m^2)

      A_cond <- 0.05 * A

    # surface area exposed to convective heat loss (m^2)

      A_conv <- A          

    # surface area exposed to diffuse and albedo flux (m^2)

      A_d <- A / 2         
  
  if (group == "aggregated") {

    # projected area in direction of sun (m^2)
    
    A_sol <- 0.15 * A

  } else {

    A_sol <- 0.25 * A

  }
  
  # Steady-state heat balance model
 
  # Solve steady state energy balance equation:

  # T_b*mflux*c <- Q_rad,sol +- Q_rad,sky +- Q_rad,ground +- Q_conduction +- Qconvection -Qevaporation
  
  T_b <- (k1 * (A_sol * S_p + A_d * S_d) + k2 * A_radSky * k3 * T_a^4 + k4 * A_radGround * T_g^4 + k5 * A_cond * T_g + hc * A_conv * T_a - lambda * mflux) / 
         (k2 * A_radSky * T_a^3 + k4 * A_radGround * T_g^3 + k5 * A_cond + hc * A_conv + mflux * specific_heat_h2o())
  
  kelvin_to_celsius(T_b)

}
