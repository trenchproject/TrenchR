#' @title Gaussian-Quadratic Function Thermal Performance Curve 
#'     
#' @description The function constructs a thermal performance curve by combining as a Gaussian function to describe the rise in performance up to the optimal temperature and a quadratic decline to zero performance at critical thermal maxima and higher temperatures \insertCite{Deutsch2008}{TrenchR}.
#' 
#' @param T_b \code{numeric} vector of temperature range (C).
#' 
#' @param T_opt \code{numeric} thermal optima (C), the temperature at which peak performance occurs.
#' 
#' @param CT_min,CT_max \code{numeric} critical thermal minimum and maximum (C), the lower and upper temperature limits for performance.
#' 
#' @return performance
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @export
#' 
#' @examples
#'   TPC(T_b    = 0:60, 
#'       T_opt  = 30, 
#'       CT_min = 10, 
#'       CT_max = 40)
#'
TPC <- function (T_b, 
                 T_opt, 
                 CT_min, 
                 CT_max) {

  stopifnot(CT_max >= CT_min)
  
  perf   <- T_b
  perf[] <- NA
  sigma <- (T_opt - CT_min) / 4

  perf[T_b <= T_opt & !is.na(T_b)] <- exp(-((T_b[T_b <= T_opt & !is.na(T_b)] - T_opt) / (2 * sigma))^2) 
  perf[T_b > T_opt & !is.na(T_b)]  <- 1 - ((T_b[T_b > T_opt & !is.na(T_b)] - T_opt) / (T_opt - CT_max))^2 
  perf[perf < 0]                  <- 0
  
  perf
  
}


#' @title Beta Function Thermal Performance Curve 
#'  
#' @description The function constructs a thermal performance curve based on a beta function \insertCite{Asbury2010}{TrenchR}.
#' 
#' @param T_b \code{numeric} temperature (C).
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
#'   TPC_beta(T_b       = 0:60, 
#'            shift     = -1, 
#'            breadth   = 0.1, 
#'            aran      = 0, 
#'            tolerance = 43, 
#'            skew      = 0.7)
#'
TPC_beta <- function (T_b, 
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
  
  T_b <- celsius_to_kelvin(T_b)
  shift <- celsius_to_kelvin(shift)
  z <- rep(0.01, length(T_b))
  z[which(is.na(T_b))] <- NA 
  
  sel <- which(T_b - shift >= 0 & T_b - shift <= tolerance)
  z[sel] <- ((T_b[sel] - shift) / tolerance)^(skew / breadth - 1) * (1 - (T_b[sel] - shift) / tolerance)^((1 - skew) / breadth - 1) / beta(skew / breadth, (1 - skew) / breadth) 
  
  if (aran == 1) {
    
    z[sel] <- z[sel] * exp(-0.6 / (T_b[sel] * 8.61734 * 10^(-5))) * 10^10 # scaling factor
    
  }
  
  z
  
}
