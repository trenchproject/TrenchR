#' @title Construct a Gaussian-quadratic thermal performance curve. 
#'     
#' @details Construct a Gaussian-quadratic thermal performance curve.
#' 
#' @description Constructs a thermal performance curve by combining as a gaussian function to describe the rise in performance up to the optimal temperature and a quadratic decline to zero performance at critical thermal maxima and higher temperatures. Reference: \insertCite{Deutsch2008}{TrenchR} 
#' 
#' @param T \code{numeric} vector of temperature range (C)
#' 
#' @param Topt \code{numeric} thermal optima (C), the temperature at which peak performance occurs
#' 
#' @param CTmin \code{numeric} critical thermal minima (C), the lower temperature limit for performance
#' 
#' @param CTmax \code{numeric} critical thermal maxima (C), the upper temperature limit for performance
#' 
#' @return performance
#' 
#' @keywords thermal performance curve
#' 
#' @export
#' 
#' @examples
#' TPC(T = 0:60, Topt = 30, CTmin = 10, CTmax = 40)
#'

TPC <- function(T, Topt, CTmin, CTmax){
  
  F <- T
  F[] <- NA
  sigma <- (Topt - CTmin) / 4
  F[T <= Topt & !is.na(T)] <- exp(-((T[T <= Topt & !is.na(T)] - Topt) / (2 * sigma))^2) 
  F[T > Topt & !is.na(T)] <- 1 - ((T[T > Topt & !is.na(T)] - Topt) / (Topt - CTmax))^2 
  F[F < 0] <- 0 # set negatives to zero
  
  F
  
}


#' @title Construct a thermal performance curve based on a beta function. 
#'  
#' @details Construct a thermal performance curve based on a beta function.
#' 
#' @description Construct a thermal performance curve based on a beta function. Reference: \insertCite{Asbury2010}{TrenchR}
#' 
#' @param T \code{numeric} temperature (C)
#' 
#' @param shift \code{numeric} mode of the thermal performance curve. Defaults to -1
#' 
#' @param breadth \code{numeric} breadth of the thermal performance curve. Defaults to 0.1
#' 
#' @param aran \code{numeric} scale performance value. if aran=0, no scaling; if aran=1, include a thermodynamic effect on mean performance. Defaults to 0.
#' 
#' @param tolerance \code{numeric} maximal breath (C) of the thermal performance curve. Defaults to 43
#' 
#' @param skew \code{numeric} skewness of the thermal performance curve (0-1). Defaults to 0.7
#' 
#' @return performance
#' 
#' @keywords thermal performance curve
#' 
#' @export
#' 
#' @examples
#' TPC.beta(T = 0:60, shift = -1, breadth = 0.1, aran = 0, tolerance = 43, skew = 0.7)
#'

TPC.beta <- function(T, shift = -1, breadth = 0.1, aran = 0, tolerance = 43, skew = 0.7){ 
  
  stopifnot(breadth > 0, aran %in% c(0, 1), tolerance > 0, skew >= 0, skew <= 1)
  
  T <- T + 273.15 # Convert temperature in degrees Celsius to Kelvin
  shift <- shift + 273.15 # Convert temperature in degrees Celsius to Kelvin         
  z <- rep(0.01, length(T))
  z[which(is.na(T))] <- NA  # Account for NAs
  
  sel <- which(T - shift >= 0 & T - shift <= tolerance)
  z[sel] <- ((T[sel] - shift) / tolerance)^(skew / breadth - 1) * (1 - (T[sel] - shift) / tolerance)^((1 - skew) / breadth - 1) / beta(skew / breadth, (1 - skew) / breadth) 
  
  if(aran==1) {
    
    z[sel] <- z[sel] * exp(-0.6 / (T[sel] * 8.61734 * 10^(-5))) * 10^10 # scaling factor
    
  }
  
  z
  
}
