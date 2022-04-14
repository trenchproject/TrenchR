#' @title Construct a Gaussian-Quadratic Thermal Performance Curve 
#'     
#' @description Constructs a thermal performance curve by combining as a Gaussian function to describe the rise in performance up to the optimal temperature and a quadratic decline to zero performance at critical thermal maxima and higher temperatures \insertCite{Deutsch2008}{TrenchR}.
#' 
#' @param T \code{numeric} vector of temperature range (C).
#' 
#' @param T_opt \code{numeric} thermal optima (C), the temperature at which peak performance occurs.
#' 
#' @param CT_min,CT_max \code{numeric} critical thermal minimum and maximum(C), the lower and upper temperature limits for performance.
#' 
#' @return performance
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @export
#' 
#' @examples
#'   TPC(T      = 0:60, 
#'       T_opt  = 30, 
#'       CT_min = 10, 
#'       CT_max = 40)
#'
TPC <- function (T, 
                 T_opt, 
                 CT_min, 
                 CT_max) {

  stopifnot(CT_max >= CT_min)
  
  F   <- T
  F[] <- NA
  sigma <- (T_opt - CT_min) / 4

  F[T <= T_opt & !is.na(T)] <- exp(-((T[T <= T_opt & !is.na(T)] - T_opt) / (2 * sigma))^2) 
  F[T > T_opt & !is.na(T)]  <- 1 - ((T[T > T_opt & !is.na(T)] - T_opt) / (T_opt - CT_max))^2 
  F[F < 0]                  <- 0
  
  F
  
}


#' @title Construct a Beta Function Thermal Performance Curve 
#'  
#' @description Construct a thermal performance curve based on a beta function \insertCite{Asbury2010}{TrenchR}.
#' 
#' @param T \code{numeric} temperature (C).
#' 
#' @param shift \code{numeric} mode of the thermal performance curve.
#' 
#' @param breadth \code{numeric} breadth of the thermal performance curve. 
#' 
#' @param aran \code{numeric} scale performance value. If \code{0}, no scaling; if \code{1}, include a thermodynamic effect on mean performance.
#' 
#' @param tolerance \code{numeric} maximal breath (C) of the thermal performance curve.
#' 
#' @param skew \code{numeric} skewness of the thermal performance curve (0-1). 
#' 
#' @return \code{numeric} performance.
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @export
#' 
#' @examples
#'   TPC.beta(T         = 0:60, 
#'            shift     = -1, 
#'            breadth   = 0.1, 
#'            aran      = 0, 
#'            tolerance = 43, 
#'            skew      = 0.7)
#'
TPC.beta <- function (T, 
                      shift     = -1, 
                      breadth   = 0.1, 
                      aran      = 0, 
                      tolerance = 43, 
                      skew      = 0.7) { 
  
  stopifnot(breadth   > 0, 
            tolerance > 0, 
            skew      >= 0, 
            skew      <= 1, 
            aran %in% c(0, 1))
  
  T <- celsius_to_kelvin(T)
  shift <- celsius_to_kelvin(shift)
  z <- rep(0.01, length(T))
  z[which(is.na(T))] <- NA 
  
  sel <- which(T - shift >= 0 & T - shift <= tolerance)
  z[sel] <- ((T[sel] - shift) / tolerance)^(skew / breadth - 1) * (1 - (T[sel] - shift) / tolerance)^((1 - skew) / breadth - 1) / beta(skew / breadth, (1 - skew) / breadth) 
  
  if (aran == 1) {
    
    z[sel] <- z[sel] * exp(-0.6 / (T[sel] * 8.61734 * 10^(-5))) * 10^10 # scaling factor
    
  }
  
  z
  
}
