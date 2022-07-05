#' @title Hourly Temperature Variation assuming Sine and Exponential Components
#'
#' @description The function estimates temperature across hours using a diurnal temperature variation function incorporating sine and exponential components \insertCite{Parton1981}{TrenchR}. 
#'
#' @details Default \code{alpha}, \code{beta}, and \code{gamma} values are the average of 5 North Carolina sites \insertCite{Wann1985}{TrenchR}. 
#'  \cr \cr
#'  Other \code{alpha}, \code{beta}, and \code{gamma} parameterizations include values for Denver, Colorado from \insertCite{Parton1981;textual}{TrenchR}: \itemize{
#'   \item{150 cm air temperature}{: \code{alpha} = 1.86, \code{beta} = 2.20, \code{gamma} = -0.17}
#'   \item{10 cm air temperature}{: \code{alpha} = 1.52, \code{beta} = 2.00, \code{gamma} = -0.18}
#'   \item{soil surface temperature}{: \code{alpha} = 0.50, \code{beta} = 1.81, \code{gamma} = 0.49}
#'   \item{10cm soil temperature}{: \code{alpha} = 0.45, \code{beta} = 2.28, \code{gamma} = 1.83}
#'  }
#'
#' @param T_max,T_min \code{numeric} maximum and minimum daily temperatures (C).
#'  
#' @param t_r,t_s \code{numeric} times of sunrise and sunset (hour).
#' 
#' @param t \code{numeric} time for temperature estimate (hour).
#' 
#' @param alpha \code{numeric} time difference between \code{t_x} (time of maximum temperature) and noon (hour). 
#' 
#' @param gamma \code{numeric} decay parameter for rate of \code{t} change from sunset to \code{t_n} (time of minimum temp). 
#' 
#' @param beta \code{numeric} time difference between \code{t_x} and sunrise (hour).
#' 
#' @return \code{numeric} temperature (C) at a specified hour. 
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   diurnal_temp_variation_sineexp(T_max = 30, 
#'                                  T_min = 10, 
#'                                  t     = 11, 
#'                                  t_r   = 6, 
#'                                  t_s   = 18, 
#'                                  alpha = 2.59, 
#'                                  beta  = 1.55, 
#'                                  gamma = 2.2)
#' 
diurnal_temp_variation_sineexp <- function (T_max, 
                                            T_min, 
                                            t, 
                                            t_r, 
                                            t_s, 
                                            alpha = 2.59, 
                                            beta  = 1.55, 
                                            gamma = 2.2) {

  stopifnot(T_max >= T_min, 
            t_s   >= 0, 
            t_s   <= 24,
            t_r   >= 0, 
            t_r   <= 24,
            t     >= 0, 
            t     <= 24, 
            alpha >= 0, 
            alpha <= 24,
            gamma >= 0, 
            gamma <= 24,
            beta  >= 0, 
            beta  <= 24)
   
  # daylength

    l <- t_s - t_r 
  
  # time of maximum, minimum temperatures

   t_x <- 0.5 * (t_r + t_s) + alpha 
   t_n <- t_r +  beta 
  
  # if night or day

    if(!(t > (t_r + beta) & t < t_s)) { 

      T_sn <- T_min + (T_max - T_min) * sin((pi * (t_s - t_r - beta)) / (l + 2 * (alpha - beta)))
    
      if (t <= (t_r + beta)) {
      
        t_as <- t + 24 - t_s
      
      }
      
      if (t >= t_s) {
      
        t_as <- t - t_s  #time after sunset
      
      }
    
      Temp <- T_min + (T_sn - T_min) * exp(-(gamma * t_as) / (24 - l + beta))
    
    } else {
   
      Temp <- T_min + (T_max - T_min) * sin((pi * (t - t_r - beta)) / (l + 2 * (alpha - beta)))
    
    }
  
  Temp
  
}


#' @title Hourly Temperature Variation assuming a Sine Interpolation
#'
#' @description The function estimates temperature for a specified hour using the sine interpolation in \insertCite{Campbell1998;textual}{TrenchR}.
#' 
#' @param T_max,T_min \code{numeric} maximum and minimum daily temperatures (C). 
#' 
#' @param t \code{numeric} time for temperature estimate (hour).
#' 
#' @return \code{numeric} temperature (C) at a specified hour. 
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   diurnal_temp_variation_sine(T_max = 30, 
#'                               T_min = 10, 
#'                               t     = 11)
#' 
diurnal_temp_variation_sine <- function (T_max, 
                                         T_min, 
                                         t) {
  
  stopifnot(t     >= 0, 
            t     <= 24, 
            T_max >= T_min)
  
  W <- pi / 12
  gamma <- 0.44 - 0.46 * sin(0.9 + W * t) + 0.11 * sin(0.9 + 2 * W * t)   
    
  T_max * gamma + T_min * (1 - gamma)
  
}


#' @title Hourly Temperature Variation using Sine and Square Root Functions
#'
#' @description  The function estimates temperature for a specified hour using sine and square root functions \insertCite{Cesaraccio2001}{TrenchR}.
#' 
#' @param t \code{numeric} hour or hours for temperature estimate.
#' 
#' @param t_r,t_s \code{numeric} sunrise and sunset hours (0-23).
#' 
#' @param T_max,T_min \code{numeric} maximum and minimum temperatures of current day (C).
#' 
#' @param T_minp \code{numeric} minimum temperature of following day (C).
#' 
#' @return \code{numeric} temperature (C) at a specified hour. 
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   diurnal_temp_variation_sinesqrt(t      = 8, 
#'                                   t_r    = 6, 
#'                                   t_s    = 18, 
#'                                   T_max  = 30, 
#'                                   T_min  = 10, 
#'                                   T_minp = 12)
#' 
diurnal_temp_variation_sinesqrt <- function (t, 
                                             t_r, 
                                             t_s, 
                                             T_max, 
                                             T_min, 
                                             T_minp) {
 
  stopifnot(t     >= 0, 
            t     <= 24, 
            t_r   >= 0, 
            t_r   <= 24, 
            t_s   >= 0, 
            t_s   <= 24, 
            T_max >= T_min)
  
  # Time estimates
  # Assume time of maximum temperature 4h before sunset

    t_p <- t_r + 24 
    t_x <- t_s - 4 
  
  # Temperature at sunset

    c  <- 0.39 
    To <- T_max - c * (T_max - T_minp)
  
  alpha <- T_max - T_min
  R <- T_max - To
  b <- (T_minp - To) / sqrt(t_p - t_s)
  
  Temp <- rep(NA, length(t))
  
  inds <- which(t <= t_r) 
  if(length(inds > 0))  {
    
    Temp[inds] <- To + b * sqrt(t[inds] - (t_s - 24))
    
  }

  inds <- which(t > t_r & t <= t_x) 
  if(length(inds > 0)) {
    
    Temp[inds] <- T_min + alpha * sin(pi / 2 * (t[inds] - t_r) / (t_x - t_r))
    
  }
  
  inds <- which(t > t_x & t < t_s) 
  if(length(inds > 0)) {
    
    Temp[inds] <- To + R * sin(pi / 2 * (1 + (t[inds] - t_x) / 4))
    
  }
  
  inds <- which(t >= t_s) 
  if(length(inds > 0))  {
    
    Temp[inds] <- To + b * sqrt(t[inds] - t_s)
    
  }

  Temp
  
}
