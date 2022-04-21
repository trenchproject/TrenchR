#' @title Operative Environmental Temperature of a Mussel Bed
#' 
#' @description The function estimates body temperature of a mussel (C). The function implements a steady-state model, which assumes unchanging environmental conditions. Based on \insertCite{Helmuth1999;textual}{TrenchR}.
#' 
#' @param l \code{numeric} mussel length (anterior/posterior axis) (m).
#' 
#' @param T_a \code{numeric} air temperature at 4 m above ground (C).
#' 
#' @param S \code{numeric} direct solar flux density (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#' 
#' @param k_d \code{numeric} diffuse fraction, proportion of solar radiation that is diffuse.
#' 
#' @param u \code{numeric} wind speed at 4 m above ground (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param evap \code{logical} Are mussels gaping to evaporatively cool? If \code{TRUE}, assumes constant mass loss rate of 5 percent of initial body mass per hour. 
#' 
#' @param cl \code{numeric} fraction of the sky covered by cloud, optional.
#' 
#' @details Conduction is considered negligible due to a small area of contact.
#'   \cr \cr
#'   Thermal radiative flux is calculated following \insertCite{Helmuth1998;textual}{TrenchR}, \insertCite{Helmuth1999;textual}{TrenchR}, and \insertCite{Idso1969;textual}{TrenchR}.
#'
#' 
#' @return \code{numeric} predicted body (operative environmental) temperature (C).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'  Tbed_mussel(l    = 0.1, 
#'              T_a  = 25, 
#'              S    = 500, 
#'              k_d  = 0.2, 
#'              u    = 1, 
#'              evap = FALSE)
#' 
Tbed_mussel <- function(l, 
                        T_a, 
                        S, 
                        k_d, 
                        u, 
                        evap = FALSE, 
                        cl   = NA) {
  
  stopifnot(l   >= 0,
            S   >= 0, 
            k_d >= 0, 
            k_d <= 1, 
            u   >  0, 
            is.logical(evap))

  if (!is.na(cl)) {

    stopifnot(cl  >= 0, 
              cl  <= 1)
  }
  
  T_a <- celsius_to_kelvin(T_a)
  
  sigma  <- stefan_boltzmann_constant()
  lambda <- latent_heat_vaporization_h2o()
  c      <- specific_heat_h2o()
  
  # Areas

    # total mussel shell surface area (m^2)

      A <- 1.08 * l^2 + 0.0461 * l - 0.0016   

    # projected area for tightly packed bed

      Ap <- 0.15 * A 
  
  
    # Assume 40% exposed to ground

      A_radSky <- 0.4 * A 

    # Assume 40% exposed to sky

      A_radGround <- 0.4 * A 

    # Assume 40% exposed to diffuse solar radiation

      A_solDiff <- 0.4 * A 

    # Assume 50% exposed to convection

      Aconv <- 0.5 * A 
  
  # Convection 

    # m/s, shear velocity

      u_star <- 0.03 * u 

    # c_p * rho <- 1200 J m^{-3}*K^{-1}

      c_prho <- 1200

      k      <- von_karman_constant()

    # environmental input is at height of 4m

      z  <- 4 
  
    # m, roughness height

      z0 <- 0.0017
  
    C <- (Aconv / Ap) * (u_star * c_prho * k) / (log(z / z0))
  
   
  
  # Emissivities

    eps_ac <- 0.72 + 0.005 * (T_a - 273) # Helmuth 1999 from Idso and Jackson 1969
    eps_sky <- ifelse( is.na(cl), 0.9, eps_ac + cl * (1 - eps_ac - 8 / T_a))  # Helmuth 1999, assume 0.9 if no cloudiness data
    eps_org <- 1.0   # infrared emissivity of organism (same as above, p.163)
  
  # Evaporation - assume constant mass loss rate of 5% initial body mass per hour, assuming a density of 700 mussels/m2 and L=0.075

    mflux <- ifelse(evap == FALSE, 0, 1.99 * 10^{-4})  #kg/sm^2
  
  # Radiation

    alpha <- 0.75   # solar absorptivity
  
    S_dir  <- S * (1 - k_d) # direct radiation
    S_diff <- S * (k_d) # diffuse radiation
  
    solar <- alpha * (S_dir + (A_solDiff / Ap) * S_diff)
    Rad_s <- 4 * sigma * (A_radSky / Ap) * eps_sky^0.75
    Rad_g <- 4 * sigma * (A_radGround / Ap)
  

  # Steady-state heat balance model
 
  # Solve steady state energy balance equation:
  # T_b*mflux*c= Q_rad,sol +- Q_rad,sky +- Q_rad,ground +- Q_conduction +- Qconvection -Qevaporation
  
  T_bed <- (solar + T_a^4 * (Rad_s + Rad_g) + C * T_a - lambda * mflux) / (mflux * c + T_a^3 * (Rad_s + Rad_g) + C)
 
  kelvin_to_celsius(T_bed)
  
}  
