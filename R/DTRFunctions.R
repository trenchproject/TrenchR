#' @title Estimate Temperature Across Hours Using a Diurnal Temperature Variation Function Incorporating Sine and Exponential Components 
#'
#' @description Estimate temperature across hours using a diurnal temperature variation function incorporating sine and exponential components \insertCite{Parton1981}{TrenchR}. Default alpha, beta, gamma values are the average of 5 North Carolina sites  \insertCite{Wann1985}{TrenchR}. Other alpha, beta, gamma parameterizations include values for Denver, Colorado from \insertCite{Parton1981}{TrenchR}: 150cm air temperature: alpha = 1.86, beta = 2.20, gamma = -0.17; 10cm air temperature: alpha = 1.52, beta = 2.00, gamma = -0.18; soil surface temperature: alpha = 0.50, beta = 1.81, gamma = 0.49; 10cm soil temperature: alpha = 0.45, beta = 2.28, gamma = 1.83.
#' 
#' @param T_max \code{numeric} maximum daily temperature (C).
#' 
#' @param T_min \code{numeric} minimum daily temperature (C).
#' 
#' @param t_s \code{numeric} time of sunrise (hour).
#' 
#' @param t_r \code{numeric} time of sunset (hour).
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

  stopifnot(T_max >= T_min, t_s >= 0, t_s <= 24,t_r >= 0, t_r <= 24,t >= 0, t <= 24, alpha >= 0, alpha <= 24,gamma >= 0, gamma <= 24,beta >= 0, beta <= 24)
   
  l <- t_s - t_r #daylength
  
  T_x <- 0.5 * (t_r + t_s) + alpha #time of maximum temperature
  T_n <- t_r +  beta #time of minimum temperature
  
  # calculate temperature for nighttime hour
  if(!(t > (t_r + beta) & t < t_s)) {
    T_sn <- T_min + (T_max - T_min) * sin((pi * (t_s - t_r - beta)) / (l + 2 * (alpha -beta)))
    
    if (t <= (t_r + beta)) {
      
      Tas <- t + 24 - t_s
      
    }
      
    if (t >= t_s) {
      
      Tas <- t - t_s  #time after sunset
      
    }
    
    T <- T_min + (T_sn - T_min) * exp(-(gamma * Tas) / (24 - l + beta))
    
  }
  
  # calculate temperature for daytime hour
  if(t > (t_r + beta) & t < t_s) {
    
    T <- T_min + (T_max - T_min) * sin((pi * (t - t_r - beta)) / (l + 2 * (alpha - beta)))
    
  }
  
  T
  
}


#' @title Diurnal Temperature Across Hours
#'
#' @description estimate temperature for a specified hour using the sine interpolation in \insertCite{Campbell1998}{TrenchR}.
#' 
#' @param T_max \code{numeric} maximum daily temperature (C). 
#' 
#' @param T_min \code{numeric} minimum daily temperature (C).
#' 
#' @param t \code{numeric} time for temperature estimate (hour).
#' 
#' @return \code{numeric} temperature (C) at a specified hour. 
#' 
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
  
  stopifnot(t >= 0, t <= 24, T_max >= T_min)
  
  W <- pi / 12
  gamma <- 0.44 - 0.46 * sin(0.9 + W * t) + 0.11 * sin(0.9 + 2 * W * t)   # (2.2) diurnal temperature function
    
  T_max * gamma + T_min * (1 - gamma)
  
}


#' @title Estimate Temperature Across Hours Using Sine and Square Root Functions
#'
#' @details  Estimates temperature across hours using sine and square root functions
#' 
#' @description  estimate temperature for a specified hour using sine and square root functions. Source: \insertCite{Cesaraccio2001}{TrenchR}
#' 
#' @param t \code{numeric} hour or hours for temperature estimate.
#' 
#' @param tr \code{numeric} sunrise hour (0-23).
#' 
#' @param ts \code{numeric} sunset hour (0-23).
#' 
#' @param T_max \code{numeric} maximum temperature of current day (C).
#' 
#' @param T_min \code{numeric} minimum temperature of current day (C).
#' 
#' @param T_minp \code{numeric} minimum temperature of following day (C).
#' 
#' @return \code{numeric} temperature (C) at a specified hour. 
#' 
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
#'                                   tr     = 6, 
#'                                   ts     = 18, 
#'                                   T_max  = 30, 
#'                                   T_min  = 10, 
#'                                   T_minp = 12)
#' 
diurnal_temp_variation_sinesqrt <- function (t, 
                                             tr, 
                                             ts, 
                                             T_max, 
                                             T_min, 
                                             T_minp) {
 
  stopifnot(t >= 0, t <= 24, tr >= 0, tr <= 24, ts >= 0, ts <= 24, T_max >= T_min)
  
  # Time estimates
  tp <- tr + 24 # sunrise time following day
  tx <- ts - 4 # Assume time of maximum temperature 4h before sunset
  
  # Temperature at sunset
  c <- 0.39 # empircally fitted parameter
  To <- T_max - c * (T_max - T_minp)
  
  alpha <- T_max -T_min
  R <- T_max - To
  b <- (T_minp - To) / sqrt(tp - ts)
  
  T <- rep(NA, length(t))
  
  inds <- which(t <= tr) 

  if(length(inds>0))  {
    
    T[inds] <- To + b * sqrt(t[inds] - (ts - 24))
    
  }

  inds <- which(t > tr & t <= tx) 
  if(length(inds > 0)) {
    
    T[inds] <- T_min + alpha * sin(pi / 2 * (t[inds] - tr) / (tx - tr))
    
  }
  
  inds <- which(t > tx & t < ts) 
  if(length(inds > 0)) {
    
    T[inds] <- To + R * sin(pi / 2 * (1 + (t[inds] - tx) / 4))
    
  }
  
  inds <- which(t >= ts) 
  if(length(inds > 0))  {
    
    T[inds] <- To + b * sqrt(t[inds] - ts)
    
  }

  T
  
}
