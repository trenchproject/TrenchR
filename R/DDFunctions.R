#' @title Degree Days
#' 
#' @description The function calculates degree days using the following approximations: single or double sine wave, single or double triangulation \insertCite{ucipm}{TrenchR}. Double approximation methods assume symmetry, such that a day's thermal minimum is equal to that of the previous day. Double sine wave approximation of degree days from \insertCite{Allen1976;textual}{TrenchR}.
#' 
#' @param T_min \code{numeric} Minimum temperature of the day (C).
#' 
#' @param T_max \code{numeric} Maximum temperature of the day (C).
#' 
#' @param LDT \code{numeric} lower developmental threshold (C).
#' 
#' @param UDT \code{numeric} upper developmental threshold (C).
#' 
#' @param method \code{character} type of method being used. Current choices: \code{"single.sine"}, \code{"double.sine"}, \code{"single.triangulation"}, and \code{"double.triangulation"}.
#' 
#' @return \code{numeric} degree days (C).
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   degree_days(T_min  = 7, 
#'               T_max  = 14, 
#'               LDT    = 12, 
#'               UDT    = 33, 
#'               method = "single.sine")
#'   degree_days(T_min  = 7, 
#'               T_max  = 14, 
#'               LDT    = 12, 
#'               UDT    = 33, 
#'               method = "single.triangulation")
#' 
degree_days <- function (T_min, 
                         T_max, 
                         LDT    = NA, 
                         UDT    = NA, 
                         method = "single.sine") {
  
  stopifnot(T_max >= T_min, 
            UDT   >= LDT, 
            length(method) == 1,
            method %in% c("single.sine", "double.sine", "single.triangulation", "double.triangulation"))
  
  # See http://ipm.ucanr.edu/WEATHER/ddconcepts.html
  
  # Amplitude
  alpha <- (T_max - T_min) / 2 
  dd <- 0
  
  if (method == "single.sine") {
    
    if (T_min >= UDT && T_max >= UDT) { # entirely above both thresholds
      
      dd <- (T_max - T_min)
      
    } else  if ( T_min >= LDT  && T_max >= UDT) { #Intercepted by upper threshold 
      
      theta2 <- asin((UDT - (T_max + T_min) / 2) / alpha)
      
      dd <- 1 / pi * (((T_max + T_min) / 2 - LDT) * (theta2 + pi / 2) + (UDT - LDT) * (pi / 2 - theta2) - alpha * cos(theta2))
    
    } else  if (T_min <= LDT &&  T_max >= UDT ) {  #Intercepted by both thresholds
      
      theta2 <- asin((UDT - (T_max + T_min) / 2) / alpha)
      theta1 <- asin((LDT - (T_max + T_min) / 2) / alpha)
      
      dd <- 1 / pi * ((  ((T_max + T_min) / 2) - LDT) * (theta2 - theta1) + alpha * (cos(theta1) - cos(theta2)) + (UDT - LDT) * (pi / 2 - theta2))
      
    } else if (T_min >= LDT &&  T_max <= UDT ) { #Entirely between both thresholds
      
      dd <- ((T_max + T_min) / 2) - LDT
      
     } else if (T_min <= LDT && T_max >= LDT) {  # intercepted by LDT  
       
      #theta1=asin((LDT-(T_max+T_min)/2)/alpha)
      # credit - http://stackoverflow.com/questions/20998460/unexpected-behavior-in-asin
      #It's a floating point issue. The way floating point numbers work is that all 
      #numbers need to be mapped to the nearest one which can be expressed as a finite 
      #sum of powers of two and this may lead to small inaccuracies in the expected output 
      #and can be dependent upon how the numbers are calculated
       
      theta1 <- asin(pmax(-1, pmin(1, (LDT - (T_max + T_min) / 2) / alpha)))
      
      dd <- 1 / pi * (( ((T_max + T_min) / 2) - LDT) * ( (pi / 2) - theta1) + alpha * cos(theta1))
      
    } else if (T_min < LDT && T_max <= LDT) { # entirely below both thresholds
      
      dd <- 0
      
    }
    
  } else if (method == "double.sine") {
    
    if (T_min >= UDT && T_max >= UDT) { # entirely above both thresholds
      
      dd <- (UDT - LDT) / 2

    } else if (T_min >= LDT  && T_max >= UDT) { #Intercepted by upper threshold

      theta2 <- asin((UDT - (T_max + T_min) / 2) / alpha)
      dd     <- 1 / (2 * pi) * (((T_max + T_min) / 2 - LDT) * (theta2 + pi / 2) + (UDT - LDT) * (pi / 2 - theta2) - alpha * cos(theta2))

    } else if (T_min <= LDT &&  T_max >= UDT) { #Intercepted by both thresholds

      theta2 <- asin((UDT - (T_max + T_min) / 2) / alpha)
      theta1 <- asin((LDT - (T_max + T_min) / 2) / alpha)
      dd     <- 1 / (2 * pi) * (((T_max + T_min) / 2 - LDT) * (theta2 - theta1) + alpha * (cos(theta1) - cos(theta2)) + (UDT - LDT) * (pi / 2 - theta2))

    } else if (T_min >= LDT &&  T_max <= UDT) { #Entirely between both thresholds

      dd <- 0.5 * ((T_max + T_min) / 2 - LDT)

    } else if (T_min <= LDT && T_max >= LDT) { # intercepted by LDT

      theta1 <- asin(pmax(-1, pmin(1, (LDT - (T_max + T_min) / 2) / alpha)))
      dd     <- 1 / (2 * pi) * (((T_max + T_min) / 2 - LDT) * (pi / 2 - theta1) + alpha * cos(theta1))

    } else if (T_min <= LDT && T_max <= LDT) { # entirely below both thresholds

      dd <- 0
    }

    dd <- dd*2

  } else if (method == "single.triangulation") {
    
    MT <- (T_max + T_min) / 2
    
    if (T_min >= UDT && T_max >= UDT) { # entirely above both thresholds
      
      dd <- UDT - LDT
      
    } else  if ( T_min >= LDT  && T_max >= UDT) { # Intercepted by upper threshold 
      
      dd <- (MT - LDT) - ((T_max - UDT)^2 / ((T_max - T_min) * 2))
      
    } else  if (T_min <= LDT &&  T_max >= UDT ) {  # Intercepted by both thresholds
      
      dd <- ((T_max - LDT)^2 - (T_max - UDT)^2) / ((T_max - T_min) * 2)
      
    } else if (T_min >= LDT &&  T_max <= UDT ) { # Entirely between both thresholds
      
      dd <- MT - LDT
      
    } else if (T_min <= LDT && T_max >= LDT) {  # intercepted by LDT  
      
      dd <- (T_max - LDT)^2 / ((T_max - T_min) * 2)
      
    } else if (T_min <= LDT && T_max <= LDT) { # entirely below both thresholds
      
      dd <- 0
      
    }
  } else if (method == "double.triangulation") {
    
    MT <- (T_max + T_min) / 2
    
    if (T_min >= UDT && T_max >= UDT) { # entirely above both thresholds
      
      dd <- (UDT - LDT) / 2
      
    } else if ( T_min >= LDT  && T_max >= UDT) { # Intercepted by upper threshold 
      
      dd <- (MT - LDT) - ((T_max - UDT)^2 / ((T_max - T_min) * 4))
      
    } else if (T_min <= LDT &&  T_max >= UDT ) {  # Intercepted by both thresholds
      
      dd <- ((T_max - LDT)^2 - (T_max - UDT)^2) / ((T_max - T_min) * 4)
      
    } else if (T_min >= LDT &&  T_max <= UDT ) { # Entirely between both thresholds
      
      dd <- (MT / 4) - (LDT / 2)
      
    } else if (T_min <= LDT && T_max >= LDT) {  # intercepted by LDT 
      
      dd <- (T_max - LDT)^2 / ((T_max - T_min) * 4)
      
    } else if (T_min <= LDT && T_max <= LDT) { # entirely below both thresholds
      
      dd <- 0
      
    }
    
   dd <- dd * 2 
    
  }
  
  round(dd, 2)
  
}
