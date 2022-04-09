#' @title Estimate Surface Roughness from Empirical Measurements
#' 
#' @description estimate surface roughness (m) from empirical wind speed (m/s) data collected at a vector of heights (m). Estimates surface roughness from empirical measurements. References: \insertCite{Kingsolver2015}{TrenchR}, \insertCite{Campbell1998}{TrenchR}, and \insertCite{Porter1979}{TrenchR}.
#' 
#' @param u_r \code{numeric} wind velocity at a vector of reference heights (m/s).
#' 
#' @param zr \code{numeric} vector of reference heights (meters)
#' 
#' @return surface roughness (meters)
#' 
#' @keywords wind profile
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
 
 stopifnot(zr > 0)
  
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


#' @title Estimate Wind Speed at a Specific Height Under Neutral Conditions
#' 
#' @description Calculate wind speed (m/s) at a specified height (m) within a boundary layer near the surface.  The profile assumes neutral conditions. The velocity profile is the neutral profile described by \insertCite{Sellers1965}{TrenchR}. Function is equations (2) and (3) of \insertCite{Porter1973}{TrenchR}. 
#' 
#' @param u_r \code{numeric} wind velocity at reference height (m/s)
#' 
#' @param zr \code{numeric} initial reference height (meters).
#' 
#' @param z0 \code{numeric} surface roughness (meters).
#' 
#' @param z \code{numeric} height to scale (meters).
#' 
#' @return windspeed (m/s)
#' 
#' @keywords wind profile
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

  stopifnot(u_r >= 0, zr >= 0, z0 >= 0, z >= 0)
  
  u_r * log(z / z0 + 1) / log(zr / z0 + 1)

}

#' @title Estimate Temperature at a Specified Height Under Neutral Conditions
#' 
#' @description  calculate temperature (C) at a specified height (m) within a boundary layer near the surface.  The velocity profile is the neutral profile described by \insertCite{Sellers1965}{TrenchR}. Function in equations (2) and (3) of \insertCite{Porter1973}{TrenchR}.
#' 
#' @param T_r \code{numeric} temperature at reference height (C).
#' 
#' @param zr \code{numeric} initial reference height (meters).
#' 
#' @param z0 \code{numeric} surface roughness (meters).
#' 
#' @param z \code{numeric} height to scale to (meters).
#' 
#' @param T_s \code{numeric} surface temperatures (C).
#' 
#' @return temperature (C)
#' 
#' @keywords temperature profile
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

  stopifnot(zr >= 0, z0 >= 0, z >= 0)
  
  (T_r - T_s) * log(z / z0 + 1) / log(zr / z0 + 1) + T_s 

}

#' @title Estimate Air Temperature Profile as in NicheMapR
#' 
#' @description  estimate temperature (C) at a specified height (meters). Estimates a single, unsegmented temperature profile using the MICRO routine from NicheMapR. Source: \insertCite{Kearney2017}{TrenchR}.
#' 
#' @param T_r \code{numeric} temperature at reference height (C).
#' 
#' @param u_r \code{numeric} windspeed at reference height (m/s).
#' 
#' @param zr \code{numeric} initial reference height (meters).
#' 
#' @param z0 \code{numeric} surface roughness (meters).
#' 
#' @param z \code{numeric} height to scale (meters).
#' 
#' @param T_s \code{numeric} surface temperatures (C).
#' 
#' @return temperature (C)
#' 
#' @keywords temperature profile
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
  
  stopifnot(u_r >= 0, zr >= 0, z0 >= 0, z >= 0)
  
  # friction velocity
  u_star <- 0.4 * u_r / log(zr / z0 + 1)  #0.4 is von Karman constant
  # sublayer stanton number
  S_ts <- 0.62 / (z0 * u_star / 12)^0.45
  # bulk Stanton number
  S_tb <- 0.64 / log(zr / z0 + 1)
  # Temperature at roughness height, z0
  T_z0 <- (T_r * S_tb + T_s * S_ts) / (S_tb + S_ts)
  # Temperature at local height
  # Inital from Ecography paper but fixed in vignette: T_z= T_z0 + (T_r - T_z0)*log(z/z0+1)
  T_z0 + (T_r - T_z0) * log(z / z0 + 1) / log(zr / z0 + 1)

}

#' @title Estimate Temperature at a Specified Height 
#' 
#' @description calculate temperature (C) at a specified height (m). Estimates a three segment velocity and temperature profile based on user-specified, experimentally determined values for 3 roughness heights and reference heights. Multiple heights are appropriate in heterogenous areas with, for example, a meadow, bushes, and rocks. Implements the MICROSEGMT routine from NicheMapR as described in \insertCite{Kearney2017}{TrenchR}.
#' 
#' @param T_r \code{numeric} a vector of temperature at the 3 reference heights (C).
#' 
#' @param u_r \code{numeric} a vector of wind speeds at the 3 reference heights (m/s).
#' 
#' @param zr \code{numeric} a vector of 3 reference heights (meters).
#' 
#' @param z0 \code{numeric} a vector of 3 experimentally determined roughness heights (meters).
#' 
#' @param z \code{numeric} height to scale to (meters).
#' 
#' @param T_s \code{numeric} surface temperatures (C).
#' 
#' @keywords temperature profile
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
air_temp_profile_segment <- function (T_r, 
                                      u_r, 
                                      zr, 
                                      z0, 
                                      z, 
                                      T_s){
  
  stopifnot(z >= 0)
  
  #order roughness and segment heights 
  zr.ord <- order(zr, decreasing = TRUE)
  zr <- zr[zr.ord]
  z0 <- z0[zr.ord]
  u_r <- u_r[zr.ord]
  T_r <- T_r[zr.ord]
  
  # friction velocity
  u_star <- 0.4 * u_r / log(zr / z0 + 1) #0.4 is von Karman constant
  # sublayer stanton number
  S_ts <- 0.62 / (z0[3] * u_star[2] / 12)^0.45
  # bulk Stanton number
  S_tb <- 0.64 / log(zr[2] / z0[3] + 1)
  
  # estimate u_Zloc  
  if(zr[1] <= z) {
    
    us_star <- u_star[1]
    z0s <- z0[1]
    T_rs <- T_r[1]
    zrs <- zr[1]
    
  }
  
  if(zr[1] > z & zr[2] <= z) {
    
    us_star <- u_star[2]
    z0s <- z0[2]
    T_rs <- T_r[2]
    zrs <- zr[2]
    
  }
  
  if(zr[1] > z & zr[2] > z) {
    
    us_star <- u_star[3]
    z0s <- z0[3]
    T_rs <- T_r[3]
    zrs <- zr[3]
    
  }
  
  # Estimate windspeed
  u_z <- 2.5 * us_star * log(z / z0s + 1)
  
  # Temperature at roughness height, z0
  T_z0 <- (T_rs * S_tb + T_s * S_ts) / (S_tb + S_ts)
  
  # Temperature ar local height
  T_z0 + (T_rs - T_z0) * log(z / z0s + 1) / log(zrs / z0s + 1)

}


#' @title Estimate Windspeed at a Specified Height 
#' 
#' @description Calculate wind speed (m/s) at a specified height (meters). Estimates a three segment velocity and temperature profile based on user-specified, experimentally determined values for 3 roughness heights and reference heights. Multiple heights are appropriate in heterogenous areas with, for example, a meadow, bushes, and rocks. Implements the MICROSEGMT routine from NicheMapR as described in \insertCite{Kearney2017}{TrenchR}
#' 
#' @param u_r \code{numeric} a vector of wind speeds at the 3 reference heights (m/s).
#' 
#' @param zr \code{numeric} a vector of 3 reference heights (meters).
#' 
#' @param z0 \code{numeric} a vector of 3 experimentally determined roughness heights (meters).
#' 
#' @param z \code{numeric} height to scale (meters).
#' 
#' @keywords wind speed profile
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
wind_speed_profile_segment <- function (u_r, 
                                        zr, 
                                        z0, 
                                        z) {
  
  stopifnot(z >= 0, length(u_r) == 3, length(zr) == 3, length(z0) == 3)
  
  # order roughness and segment heights so that z1>z2>z0 
  zr.ord <- order(zr, decreasing = TRUE)
  zr <- zr[zr.ord]
  z0 <- z0[zr.ord]
  u_r <- u_r[zr.ord]
  
  # friction velocity
  u_star <- 0.4 * u_r / log(zr / z0 + 1) #0.4 is von Karman constant
  
  # estimate u_Zloc  
  if(z <= zr[3]) {
    
    us_star <- u_star[3]
    z0s <- z0[3]
    zrs <- zr[3]
    
  }
  
  if(z > zr[3] & z < zr[2]) {
    
    us_star <- u_star[2]
    z0s <- z0[2]
    zrs <- zr[2]
    
  }
  
  if(z >= zr[2]) {
    
    us_star <- u_star[1]
    z0s <- z0[1]
    zrs <- zr[1]
    
  }
  
  #estimate windspeed
  .5 * us_star * log(z / z0s + 1)

}
