#' @title Diffuse Fraction for Partitioning Solar Radiation
#' 
#' @description The function partitions solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}) into direct and diffuse components by estimating the diffuse fraction (k_d). The function uses the models presented in \insertCite{Wong2001;textual}{TrenchR}.
#' 
#' @param method \code{character} method to use for estimating the diffuse fraction, currently available: \code{"Liu_Jordan"}, \code{"Orgill_Hollands"}, \code{"Erbs"}, \code{"Olyphant"}, \code{"Spencer"}, \code{"Reindl-1"}, \code{"Reindl-2"}, \code{"Lam_Li"}.
#' 
#' @param kt \code{numeric} the clearness index (dimensionless), which is the ratio of the global solar radiation measured at the surface to the total solar radiation at the top of the atmosphere. (0-1)
#' 
#' @param lat \code{numeric} latitude (degrees). Needed only if method is \code{"Spencer"}.
#' 
#' @param sol.elev \code{numeric} the solar elevation angles (degrees). Needed only if method is \code{"Reindl-2"}. 
#' 
#' @return \code{numeric} diffuse fraction.
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   partition_solar_radiation(method   = "Erbs", 
#'                             kt       = 0.5, 
#'                             lat      = 40, 
#'                             sol.elev = 60)
#'
partition_solar_radiation <- function (method, 
                                       kt, 
                                       lat = NA, 
                                       sol.elev = NA){  
 
  stopifnot(length(method) == 1,
            method %in% c("Liu_Jordan", "Orgill_Hollands", "Erbs", "Olyphant", "Spencer", "Reindl-1", "Reindl-2", "Lam_Li"), 
            kt >= 0, 
            kt <= 1)
  
  #6.1 Liu and Jordan 
  if (method == "Liu_Jordan") {
    
    kd <- (0.271 - 0.294 * kt) / kt
    
    if (kd > 1) { 
 
      kd <- 1

    }

  }
    
  #6.2 Orgill and Hollands
  if (method == "Orgill_Hollands") {
    
    if (kt < 0.35) {

     kd <- 1 - 0.249 * kt

    }

    if (kt >= 0.35 & kt <= 0.75) {

      kd <- 1.577 - 1.84 * kt
    
    }

    if (kt >= 0.75) {

      kd <- 0.177 
    
    }


  }
  
  #6.3 Erbs et al.
  if (method == "Erbs") {
    
    if(kt <= 0.22) {

      kd <- 1 - 0.09 * kt
    
    }

    if(kt > 0.22 & kt < 0.8) {

      kd <- 0.9511 - 0.1604 * kt + 4.388 * kt^2 - 16.638 * kt^3 + 12.336 * kt^4
    
    }

    if(kt >= 0.8) {

      kd <- 0.165 # Correction from 0.125 for CO from Olyphant 1984
  
    }

  }
  
  if(method == "Olyphant") { 
    # Correction for Colorado from Olyphant 1984
    
    if(kt <= 0.22) {

      kd <- 1 - 0.09 * kt
    
    }

    if(kt > 0.22 & kt < 0.8) {

      kd <- 0.9511 - 0.1604 * kt + 4.388 * kt^2 - 16.638 * kt^3 + 12.336 * kt^4
    
    }

    if(kt >= 0.8) {

      kd <- 0.125 
  
    }

  }
  
  #6.4 Spencer
  if(method=="Spencer"){
    
    a3 <- 0.94 + 0.0118 * abs(lat)
    
    b3 <- 1.185 + 0.0135 * abs(lat)
    
    # method assumes constant kd if kt outside the below range
    kd <- NA
    
    if(kt >= 0.35 & kt <= 0.75) {

      kd <- a3 - b3 * kt
    
    }

  }
  
  #6.5 Reindl et al.
  if(method == "Reindl-1"){
    
    if(kt <= 0.3) {

      kd <- 1.02 - 0.248 * kt
    
    }

    if(kt > 0.3 & kt < 0.78) {

      kd <- 1.45 - 1.67 * kt
    
    }

    if(kt >= 0.78) {

      kd <- 0.147
  
    }

  }
  
  if(method == "Reindl-2") {
    
    if(kt <= 0.3) {

      kd <- 1.02 - 0.254 * kt
    
    }

    if(kt > 0.3 & kt < 0.78) {

      kd <- 1.4 - 1.749 * kt + 0.177 * sin(sol.elev * 180 / pi)
    
    }

    if(kt >= 0.78) {

      kd <- 0.486 * kt - 0.182 * sin(sol.elev * 180 / pi)
  
    }

  }
  
  #6.6 Lam and Li
  if(method == "Lam_Li") {
    
    if(kt <= 0.15) {

      kd <- 0.977
    
    }

    if(kt > 0.15 & kt <= 0.7) {

      kd <- 1.237 - 1.361 * kt
    
    }

    if(kt > 0.7) {

      kd <- 0.273
  
    }

  }
  
  #direct and diffuse radiation is c(rad*(1-kd),rad*(kd))
  
  kd

}  

#' @title Ratio of Diffuse to Direct Solar Radiation
#' 
#' @description The function estimates the ratio of diffuse to direct solar radiation based on the approximation of the SOLRAD model \insertCite{McCullough1971}{TrenchR} described in \insertCite{Tracy1983;textual}{TrenchR}.
#' 
#' @param psi \code{numeric} Zenith angle of the sun (degrees).
#' 
#' @param p_a \code{numeric} Atmospheric pressure (kPa).
#' 
#' @param A \code{numeric} albedo of the substrate (fraction of 1).
#' 
#' @return \code{numeric} diffuse fraction.
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   proportion_diffuse_solar_radiation(psi = 60, 
#'                                      p_a = 86.1, 
#'                                      A   = 0.25)
#'
proportion_diffuse_solar_radiation <- function (psi, 
                                                p_a, 
                                                A) {  
  
  stopifnot(psi >= 0, 
            psi <= 89.5, 
            p_a >  0, 
            A   >= 0, 
            A   <= 1)
 
  if (psi <= 50) {
    
    prop <- (5.67 * 10^-2 + 1.698 * 10^-5 * psi + 1.917 * 10^-6 * psi^2 + 1.028 * 10^-7 * psi^3) * 
            (1 + 0.01 * (p_a - 86.1) + 0.12 * (A - 0.25))
    
  }
  
  if (psi > 50) {
    
    prop <- (5.83819968 - 0.390636004 * psi + 9.79200778 * 10^-3 * psi^2 - 1.0786077 * 10^-4 * psi^3 + 4.42915464 * 10^-7 * psi^4) * 
            (1 + 0.009 * (p_a - 86.1) + (0.8 - 0.015 * psi) * (A - 0.25))
     
  }
    
  prop
  
}
