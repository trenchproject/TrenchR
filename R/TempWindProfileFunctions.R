#' @title Surface Roughness from Empirical Measurements
#' 
#' @description The function estimates surface roughness (m) from empirical wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}) data collected at a vector of heights (m) \insertCite{Kingsolver2015,Campbell1998,Porter1979}{TrenchR}.
#' 
#' @param u_r \code{numeric} wind velocity (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}) at a vector of reference heights.
#' 
#' @param zr \code{numeric} vector of reference heights (m).
#' 
#' @return \code{numeric} surface roughness (m).
#' 
#' @family microclimate functions
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @export
#' 
#' @examples
#'   surface_roughness(u_r = c(0.01, 0.025, 0.05, 0.1, 0.2), 
#'                     zr  = c(0.05, 0.25, 0.5, 0.75, 1))
#' 
surface_roughness <- function (u_r, 
                               zr) {
 
 stopifnot(zr  >  0,
           u_r >= 0)
  
 mod1 <- lm(log(zr) ~ u_r)
 d <- exp(as.numeric(mod1$coefficients[1])) # Zero Plane displacement:height at which the wind speed is zero
 # can also assume d=0.63h (Monteith 1975)
 inds <- which(zr - d > 0)   # indices of measurements where zr-d>0
 
 z0 <- NA
 if(length(inds) > 0){
   
   mod1 <- lm(u_r[inds] ~ log(zr[inds] - d))
   b <- as.numeric(mod1$coefficients[1])
   n <- as.numeric(mod1$coefficients[2])
   z0 <- exp(-b / n)
 
 }
 
 z0
 
}


#' @title Wind Speed at a Specific Height Under Neutral Conditions
#' 
#' @description The function calculates wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}) at a specified height (m) within a boundary layer near the surface.  The profile assumes neutral conditions. The velocity profile is the neutral profile described by \insertCite{Sellers1965;textual}{TrenchR}. Function is equations (2) and (3) of \insertCite{Porter1973;textual}{TrenchR}. 
#' 
#' @param u_r \code{numeric} wind velocity (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}) at reference height.
#' 
#' @param zr \code{numeric} initial reference height (m).
#' 
#' @param z0 \code{numeric} surface roughness (m).
#' 
#' @param z \code{numeric} height to scale (m).
#' 
#' @return \code{numeric} windspeed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @family microclimate functions
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @export
#' 
#' @examples
#'   wind_speed_profile_neutral(u_r = 0.1, 
#'                              zr  = 0.1, 
#'                              z0  = 0.2, 
#'                              z   = 0.15)
#'
 
wind_speed_profile_neutral <- function (u_r, 
                                        zr, 
                                        z0, 
                                        z) {

  stopifnot(u_r >= 0, 
            zr  >= 0, 
            z0  >= 0, 
            z   >= 0)
  
  u_r * log(z / z0 + 1) / log(zr / z0 + 1)

}

#' @title Air Temperature at a Specified Height Under Neutral Conditions
#' 
#' @description The function calculates air temperature (C) at a specified height (m) within a boundary layer near the surface.  The velocity profile is the neutral profile described by \insertCite{Sellers1965;textual}{TrenchR}. Function is included as equations (2) and (3) of \insertCite{Porter1973;textual}{TrenchR}.
#' 
#' @param T_r \code{numeric} air temperature (C) at reference height.
#' 
#' @param zr \code{numeric} initial reference height (m).
#' 
#' @param z0 \code{numeric} surface roughness (m).
#' 
#' @param z \code{numeric} height to scale to (m).
#' 
#' @param T_s \code{numeric} surface temperatures (C).
#' 
#' @return \code{numeric} air temperature (C).
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @examples
#'   air_temp_profile_neutral(T_r = 20, 
#'                            zr  = 0.1, 
#'                            z0  = 0.2, 
#'                            z   = 0.15, 
#'                            T_s = 25)
#'
#'
air_temp_profile_neutral <- function (T_r, 
                                      zr, 
                                      z0, 
                                      z, 
                                      T_s) {

  stopifnot(zr >= 0, 
            z0 >= 0, 
            z  >= 0)
  
  (T_r - T_s) * log(z / z0 + 1) / log(zr / z0 + 1) + T_s 

}

#' @title Air Temperature Profile using MICRO Routine
#' 
#' @description  The function estimates air temperature (C) at a specified height (m). Estimates a single, unsegmented temperature profile using the MICRO routine from NicheMapR \insertCite{Kearney2017}{TrenchR}.
#' 
#' @param T_r \code{numeric} air temperature (C) at reference height.
#' 
#' @param u_r \code{numeric} windspeed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}) at reference height.
#' 
#' @param zr \code{numeric} initial reference height (m).
#' 
#' @param z0 \code{numeric} surface roughness (m).
#' 
#' @param z \code{numeric} height to scale (m).
#' 
#' @param T_s \code{numeric} surface temperatures (C).
#' 
#' @return \code{numeric} air temperature (C).
#' 
#' @family microclimate functions
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @export
#' 
#' @examples
#'   air_temp_profile(T_r = 20,
#'                    u_r = 0.1, 
#'                    zr  = 0.1, 
#'                    z0  = 0.2, 
#'                    z   = 0.15, 
#'                    T_s = 25)
#'
air_temp_profile <- function (T_r, 
                              u_r, 
                              zr, 
                              z0,
                              z,
                              T_s) {
  
  stopifnot(u_r >= 0, 
            zr  >= 0, 
            z0  >= 0, 
            z   >= 0)
  
  k <- von_karman_constant()

  # friction velocity

    u_star <- k * u_r / log(zr / z0 + 1)  

  # sublayer Stanton number

    S_ts <- 0.62 / (z0 * u_star / 12)^0.45

  # bulk Stanton number

    S_tb <- 0.64 / log(zr / z0 + 1)

  # Temperature at roughness height, z0

    T_z0 <- (T_r * S_tb + T_s * S_ts) / (S_tb + S_ts)

  # Temperature at local height
  # Initial from Ecography paper but fixed in vignette: T_z= T_z0 + (T_r - T_z0)*log(z/z0+1)

    T_z0 + (T_r - T_z0) * log(z / z0 + 1) / log(zr / z0 + 1)

}

#' @title Air Temperature at a Specified Height 
#' 
#' @description The function calculates air temperature (C) at a specified height (m). Estimates a three segment velocity and temperature profile based on user-specified, experimentally determined values for 3 roughness heights and reference heights. Multiple heights are appropriate in heterogenous areas with, for example, a meadow, bushes, and rocks. Implements the MICROSEGMT routine from NicheMapR as described in \insertCite{Kearney2017;textual}{TrenchR}.
#' 
#' @param T_r \code{numeric} a vector of air temperatures (C) at the 3 reference heights.
#' 
#' @param u_r \code{numeric} a vector of wind speeds (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}) at the 3 reference heights.
#' 
#' @param zr \code{numeric} a vector of 3 reference heights (meters).
#' 
#' @param z0 \code{numeric} a vector of 3 experimentally determined roughness heights (meters).
#' 
#' @param z \code{numeric} height for air temperature estimation (meters).
#' 
#' @param T_s \code{numeric} surface temperatures (C).
#' 
#' @return \code{numeric} air temperature (C).
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   air_temp_profile_segment(T_r = c(25, 22, 20), 
#'                            u_r = c(0.01, 0.025, 0.05), 
#'                            zr  = c(0.05, 0.25, 0.5), 
#'                            z0  = c(0.01, 0.15, 0.2), 
#'                            z   = 0.3, 
#'                            T_s = 27)
#'
air_temp_profile_segment <- function(T_r,
                                     u_r,
                                     zr,
                                     z0,
                                     z,
                                     T_s) {
  
  # Input validation
  if (!all(lengths(list(T_r, u_r, zr, z0)) == 3))
    stop("'T_r', 'u_r', 'zr', and 'z0' must each be length 3 ",
         "(one value per canopy segment).")
  if (!all(u_r >= 0, na.rm = TRUE))
    stop("All values of 'u_r' must be >= 0.")
  if (!all(zr >= 0, na.rm = TRUE))
    stop("All values of 'zr' must be >= 0.")
  if (!all(z0 > 0, na.rm = TRUE))
    stop("All values of 'z0' must be > 0 (roughness length cannot be zero).")
  if (!all(z >= 0, na.rm = TRUE))
    stop("'z' must be >= 0.")
  if (!all(zr > z0, na.rm = TRUE))
    stop("All reference heights 'zr' must be greater than roughness lengths 'z0'.")
  
  # Order segments by decreasing reference height
  zr.ord <- order(zr, decreasing = TRUE)
  zr     <- zr[zr.ord]
  z0     <- z0[zr.ord]
  u_r    <- u_r[zr.ord]
  T_r    <- T_r[zr.ord]
  
  # Friction velocity (von Karman constant = 0.4)
  u_star <- 0.4 * u_r / log(zr / z0 + 1)
  
  # Sublayer Stanton number (surface-to-air, uses lowest segment)
  S_ts <- 0.62 / (z0[3] * u_star[2] / 12)^0.45
  
  # Bulk Stanton number (air-to-air, uses mid segment)
  S_tb <- 0.64 / log(zr[2] / z0[3] + 1)
  
  # Select segment containing height z
  if (z >= zr[1]) {
    us_star <- u_star[1]
    z0s     <- z0[1]
    T_rs    <- T_r[1]
    zrs     <- zr[1]
  } else if (z >= zr[2]) {
    us_star <- u_star[2]
    z0s     <- z0[2]
    T_rs    <- T_r[2]
    zrs     <- zr[2]
  } else if (z >= 0) {
    us_star <- u_star[3]
    z0s     <- z0[3]
    T_rs    <- T_r[3]
    zrs     <- zr[3]
  } else {
    stop("Could not assign height 'z' to any canopy segment. ",
         "Check that 'z' and 'zr' values are consistent.")
  }
  
  # Wind speed at height z (m/s) 
  u_z <- 2.5 * us_star * log(z / z0s + 1)
  
  # Temperature at roughness height z0 (blended surface/air temp)
  T_z0 <- (T_rs * S_tb + T_s * S_ts) / (S_tb + S_ts)
  
  # Temperature at height z (°C)
  T_z0 + (T_rs - T_z0) * log(z / z0s + 1) / log(zrs / z0s + 1)
}


#' @title Wind Speed at a Specified Height 
#' 
#' @description The function calculates wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}) at a specified height (m). The function estimates a three segment velocity and temperature profile based on user-specified, experimentally determined values for 3 roughness heights and reference heights. Multiple heights are appropriate in heterogenous areas with, for example, a meadow, bushes, and rocks. Implements the MICROSEGMT routine from NicheMapR as described in \insertCite{Kearney2017;textual}{TrenchR}.
#' 
#' @param u_r \code{numeric} a vector of wind speeds (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}) at the 3 reference heights.
#' 
#' @param zr \code{numeric} a vector of 3 reference heights (m).
#' 
#' @param z0 \code{numeric} a vector of 3 experimentally determined roughness heights (m).
#' 
#' @param z \code{numeric} height to scale (m).
#' 
#' @return \code{numeric} wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @family microclimate functions
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @export
#' 
#' @examples
#'   wind_speed_profile_segment(u_r = c(0.01, 0.025, 0.05), 
#'                              zr  = c(0.05, 0.25, 0.5), 
#'                              z0  = c(0.01, 0.15, 0.2), 
#'                              z   = 0.3)
#'
wind_speed_profile_segment <- function(u_r,
                                       zr,
                                       z0,
                                       z) {
  
  # Input validation
  if (!all(lengths(list(u_r, zr, z0)) == 3))
    stop("'u_r', 'zr', and 'z0' must each be length 3 ",
         "(one value per canopy segment).")
  if (!all(u_r >= 0, na.rm = TRUE))
    stop("All values of 'u_r' must be >= 0.")
  if (!all(zr >= 0, na.rm = TRUE))
    stop("All values of 'zr' must be >= 0.")
  if (!all(z0 > 0, na.rm = TRUE))
    stop("All values of 'z0' must be > 0 (roughness length cannot be zero).")
  if (!all(z >= 0, na.rm = TRUE))
    stop("'z' must be >= 0.")
  if (!all(zr > z0, na.rm = TRUE))
    stop("All reference heights 'zr' must be greater than roughness lengths 'z0'.")
  
  k <- von_karman_constant()
  
  # Order segments by decreasing reference height (zr[1] > zr[2] > zr[3])
  zr.ord <- order(zr, decreasing = TRUE)
  zr     <- zr[zr.ord]
  z0     <- z0[zr.ord]
  u_r    <- u_r[zr.ord]
  
  # Friction velocity per segment
  u_star <- k * u_r / log(zr / z0 + 1)
  
  # Select segment containing height z (consistent with air_temp_profile_segment)
  if (z >= zr[1]) {
    us_star <- u_star[1]
    z0s     <- z0[1]
  } else if (z >= zr[2]) {
    us_star <- u_star[2]
    z0s     <- z0[2]
  } else if (z >= 0) {
    us_star <- u_star[3]
    z0s     <- z0[3]
  } else {
    stop("Could not assign height 'z' to any canopy segment. ",
         "Check that 'z' and 'zr' values are consistent.")
  }
  
  # Log-linear wind profile: u_z = (u_star / k) * log(z / z0 + 1)
  # FIX: was 0.5 * us_star — should be (1/k) * us_star = 2.5 * us_star
  (1 / k) * us_star * log(z / z0s + 1)
}