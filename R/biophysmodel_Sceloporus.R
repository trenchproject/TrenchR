#' @title Operative Environmental Temperature of a Lizard
#' 
#' @description The function estimates body temperature (C, operative environmental temperature) of a lizard based on \insertCite{Campbell1998;textual}{TrenchR}. The function was designed for Sceloporus lizards and described in \insertCite{Buckley2008;textual}{TrenchR}.
#' 
#' @details The proportion of radiation that is direct is determined following \insertCite{Sears2011;textual}{TrenchR}.
#'   \cr \cr 
#'   Boundary conductance uses a factor of 1.4 to account for increased convection \insertCite{Mitchell1976}{TrenchR}.
#'
#' @param T_a \code{numeric} air temperature (C).
#' 
#' @param T_g \code{numeric} surface temperature (C).
#' 
#' @param u \code{numeric} wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param svl \code{numeric} lizard snout vent length (mm).
#' 
#' @param m \code{numeric} lizard mass (g); note that it can be estimated as in \code{\link{mass_from_length}}: \ifelse{html}{\out{3.55 x 10<sup>-5</sup> x length<sup>3</sup>}}{\eqn{3.55*10^-5 * length^3}{ASCII}}
#' 
#' @param psi \code{numeric} solar zenith angle (degrees).
#' 
#' @param rho_S \code{numeric} surface albedo (proportion). ~ 0.25 for grass, ~ 0.1 for dark soil, > 0.75 for fresh snow \insertCite{Campbell1998}{TrenchR}.
#' 
#' @param elev \code{numeric} elevation (m).
#' 
#' @param doy \code{numeric} day of year (1-366).
#' 
#' @param sun \code{logical} indicates whether lizard is in sun (\code{TRUE}) or shade (\code{FALSE}).
#' 
#' @param surface \code{logical} indicates whether lizard is on ground surface (\code{TRUE}) or above the surface (\code{FALSE}, e.g. in a tree).
#' 
#' @param alpha_S \code{numeric} lizard solar absorptivity (proportion), \code{alpha_S = 0.9} \insertCite{Gates1980}{TrenchR} (Table 11.4).
#' 
#' @param alpha_L \code{numeric} lizard thermal absorptivity (proportion), \code{alpha_L = 0.965} \insertCite{Bartlett1967}{TrenchR}.
#' 
#' @param epsilon_s \code{numeric} surface emissivity of lizards (proportion), \code{epsilon_s = 0.965} \insertCite{Bartlett1967}{TrenchR}.
#' 
#' @param F_d \code{numeric} the view factor between the surface of the lizard and diffuse solar radiation (proportion). i.e., the portion of the lizard surface that is exposed to diffuse solar radiation \insertCite{Bartlett1967}{TrenchR}.
#' 
#' @param F_r \code{numeric} the view factor between the surface of the lizard and reflected solar radiation (proportion).
#' 
#' @param F_a \code{numeric} the view factor between the surface of the lizard and atmospheric radiation (proportion).
#' 
#' @param F_g \code{numeric} the view factor between the surface of the lizard and ground thermal radiation (proportion).
#' 
#' @return T_e \code{numeric} predicted body (operative environmental) temperature (C).
#' 
#' @family biophysical models
#' 
#' @export 
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Tb_lizard(T_a       = 25, 
#'             T_g       = 30, 
#'             u         = 0.1, 
#'             svl       = 60, 
#'             m         = 10, 
#'             psi       = 34, 
#'             rho_S     = 0.24, 
#'             elev      = 500, 
#'             doy       = 200, 
#'             sun       = TRUE, 
#'             surface   = TRUE, 
#'             alpha_S   = 0.9, 
#'             alpha_L   = 0.965, 
#'             epsilon_s = 0.965, 
#'             F_d       = 0.8, 
#'             F_r       = 0.5, 
#'             F_a       = 0.5, 
#'             F_g       = 0.5)
#' 
Tb_lizard <- function (T_a, 
                       T_g, 
                       u, 
                       svl, 
                       m, 
                       psi, 
                       rho_S, 
                       elev, 
                       doy, 
                       sun       = TRUE, 
                       surface   = TRUE, 
                       alpha_S   = 0.9, 
                       alpha_L   = 0.965, 
                       epsilon_s = 0.965, 
                       F_d       = 0.8, 
                       F_r       = 0.5, 
                       F_a       = 0.5, 
                       F_g       = 0.5) {
  
  stopifnot(u         >= 0, 
            svl       >= 0, 
            m         >= 0, 
            rho_S     >= 0, 
            rho_S     <= 1, 
            elev      >= 0, 
            doy       >  0, 
            doy       <  367, 
            alpha_S   >= 0, 
            alpha_S   <= 1, 
            alpha_L   >= 0, 
            alpha_L   <= 1, 
            epsilon_s >= 0, 
            epsilon_s <= 1, 
            F_d       >= 0, 
            F_d       <= 1, 
            F_r       >= 0, 
            F_r       <= 1, 
            F_a       >= 0, 
            F_a       <= 1, 
            F_g       >= 0, 
            F_g       <= 1,
            is.logical(sun),
            is.logical(surface))
  
  psi <- degrees_to_radians(psi) 
  
  # constants

    sigma <- stefan_boltzmann_constant() 
    c_p   <- 29.3 # specific heat of air, J/mol C (p.279) Parentheses all from Campbell & Norman 1998
    tau   <- 0.65 # atmospheric transmissivity
    S_p0  <- 1360 # extraterrestrial flux density, W/m^2 (p.159)
  
  # Calculate radiation
  # view angles, parameterized for animal suspended above ground (p181), on ground- adjust F_e, F_r, and F_g

    h <- svl / 1000 # convert snout vent length to m
  
    A <- 0.121 * m^0.688   # total lizard area, Roughgarden (1981)
    A_p <- (-1.1756810^-4 * psi^2 - 9.2594 * 10^-2 * psi + 26.2409) * A / 100 # projected area
    F_p <- A_p / A
  
  # Radiation

    p_a <- 101.3 * exp (-elev / 8200)  # atmospheric pressure
    m_a <- p_a / (101.3 * cos(psi))  # (11.12) optical air mass
    m_a[(psi > (80 * pi / 180))] <- 5.66
  
  # Flux densities

    epsilon_ac <- 9.2 * 10^-6 * (T_a + 273)^2 # (10.11) clear sky emissivity
    L_a <- sigma * (T_a + 273)^4  # (10.7) long wave flux densities from atmosphere 
    L_g <- sigma * (T_g + 273)^4  # (10.7) long wave flux densities from ground
  
    S_d <- 0.3 * (1 - tau^m_a) * S_p0 * cos(psi)  # (11.13) diffuse radiation
  
    dd2 <- 1 + 2 * 0.1675 * cos(2 * pi * doy / 365)
    S_p <- S_p0 * tau^m_a * dd2 * cos(psi)  # Sears and Angilletta 2012 #dd is correction factor accounting for orbit
    S_b <- S_p * cos(psi)
    S_t <- S_b + S_d
    S_r <- rho_S * S_t # (11.10) reflected radiation
  
  # Conductance
  
    # characteristic dimension in meters

      dim <- svl / 1000 

      g_r <- 4 * epsilon_s * sigma * (T_a + 273)^3 / c_p # (12.7) radiative conductance
  
      g_Ha <- 1.4 * 0.135 * sqrt(u / dim) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976)
  
  # Operative environmental temperature
  
  # Calculate with both surface and air temp (on ground and in tree)

    sprop <- 1 # proportion of radiation that is direct, Sears and Angilletta 2012
    R_abs <- sprop * alpha_S * (F_p * S_p + F_d * S_d + F_r * S_r) + alpha_L * (F_a * L_a + F_g * L_g) # (11.14) Absorbed radiation

    Te      <- T_a + (R_abs - epsilon_s * sigma * (T_a + 273)^4) / (c_p * (g_r + g_Ha)) # (12.19) Operative temperature            
    Te_surf <- T_g + (R_abs - epsilon_s * sigma * (T_g + 273)^4) / (c_p * (g_r + g_Ha))        
  
  # Calculate in shade, no direct radiation

    sprop <- 0 # proportion of radiation that is direct, Sears and Angilletta 2012
    R_abs <- sprop * alpha_S * (F_p * S_p + F_d * S_d + F_r * S_r) + alpha_L * (F_a * L_a + F_g * L_g) # (11.14) Absorbed radiation

    TeS      <- T_a + (R_abs - epsilon_s * sigma * (T_a + 273)^4) / (c_p * (g_r + g_Ha)) # (12.19) Operative temperature                        
    TeS_surf <- T_g + (R_abs - epsilon_s * sigma * (T_g + 273)^4) / (c_p * (g_r + g_Ha))  
  

  if (sun & surface) {
    
    Te <- Te_surf
    
  } else if (sun & !surface) {
    
    Te <- Te
    
  } else if (!sun & surface) {
    
    Te <- TeS_surf
    
  } else if (!sun & !surface) {
    
    Te <- TeS
    
  }
  
  Te
  
}
