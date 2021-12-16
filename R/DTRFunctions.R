#' Estimate temperature across hours using a diurnal temperature variation function incorporating sine and exponential components 
#'
#' @details Estimate temperature across hours using a diurnal temperature variation function incorporating sine and exponential components.
#' @description This function allows you to estimate temperature across hours using a diurnal temperature variation function incorporating sine and exponential components. Source: Parton WJ and Logan JA. 1981. A model for diurnal variation in soil and air temperature. Agricultural Meteorology 23: 205-216.
#' 
#' @param T_max maximum daily temperature (C)
#' 
#' @param T_min minimum daily temperature (C)
#' 
#' @param t_s time of sunrise (hour)
#' 
#' @param t_r time of sunset (hour)
#' 
#' @param t time for temperature estimate (hour)
#' 
#' @param alpha  time difference between t_x (time of maximum temperature) and noon (hour)
#' 
#' @param gamma decay parameter for rate of t change from sunset to t_n (time of minimum temp)
#' 
#' @param beta time difference between t_x and sunrise (hour)
#' 
#' @return temperature (C) at a specified hour 
#' 
#' @keywords Temperature
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @examples
#'   diurnal_temp_variation_sineexp(
#'   T_max=30, 
#'   T_min=10, 
#'   t=11, 
#'   t_r=6, 
#'   t_s=18, 
#'   alpha=2.59, 
#'   beta= 1.55, 
#'   gamma=2.2)
#' 
diurnal_temp_variation_sineexp=function(T_max, T_min, t, t_r, t_s, alpha=2.59, beta= 1.55, gamma=2.2){

  stopifnot(T_max>=T_min, t_s>=0, t_s<=24,t_r>=0, t_r<=24,t>=0, t<=24,alpha>=0, alpha<=24,gamma>=0, gamma<=24,beta>=0, beta<=24)
  
  #default values are the average of 5 North Carolina sites. Source: Wann M et al. 1985. Evaluation and calibration of three models for daily cycle of air temperature. Agricultural and Forest Meteorology 34: 121-128.
  
  #Other parameterizations include values for Denver, Colorado from Parton and Logan (1981): 
  # 150cm air temeprature: alpha=1.86, beta= 2.20, gamma=-0.17
  # 10cm air temperature: alpha=1.52, beta= 2.00, gamma=-0.18
  # soil surface temperature: alpha=0.50, beta= 1.81, gamma=0.49
  # 10cm soil temperature: alpha=0.45, beta= 2.28, gamma=1.83
 
l= t_s-t_r #daylength

T_x= 0.5*(t_r+t_s)+alpha #time of maximum temperature
T_n= t_r+ beta #time of minimum temperature

#calculate temperature for nighttime hour
if(!(t > (t_r + beta) & t < t_s)) {
  T_sn = T_min + (T_max - T_min) * sin((pi * (t_s - t_r - beta)) / (l + 2 * (alpha -
                                                                      beta)))
  if (t <= (t_r + beta))
    Tas = t + 24 - t_s
  if (t >= t_s)
    Tas = t - t_s  #time after sunset
  T = T_min + (T_sn - T_min) * exp(-(gamma * Tas) / (24 - l + beta))
}

#calculate temperature for daytime hour
if(t>(t_r + beta) &
   t<t_s) {
  T = T_min + (T_max - T_min) * sin((pi * (t - t_r - beta)) / (l + 2 * (alpha -
                                                                    beta)))
}
return(T)
}

#' Diurnal temperature across hours
#'
#' @details Uses a sine interpolation to estimate temperature across hours.
#' 
#' @description This function allows you to estimate temperature for a specified hour using the sine interpolation in Campbell and Norman (1998). Source: Campbell and Norman. 1998. An Introduction to Environmental Biophysics.
#' 
#' @param T_max maximum daily temperature (C) 
#' 
#' @param T_min minimum daily temperature (C)
#' 
#' @param t time for temperature estimate (hour)
#' 
#' @return temperature (C) at a specified hour 
#' 
#' @keywords Temperature
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @examples
#' 
#' diurnal_temp_variation_sine(T_max=30, T_min=10, t=11)
#' 
diurnal_temp_variation_sine=function(T_max, T_min, t){
  
  stopifnot(t>=0, t<=24, T_max>=T_min)
  
  W=pi/12;
  gamma= 0.44 - 0.46* sin(0.9 + W * t)+ 0.11 * sin(0.9 + 2 * W * t);   # (2.2) diurnal temperature function
  T = T_max*gamma + T_min * (1 - gamma)
  
  return(T)
}


#' Estimates temperature across hours using sine and square root functions
#'
#' @details  Estimates temperature across hours using sine and square root functions
#' 
#' @description This function allows you to estimate temperature for a specified hour using sine and square root functions. Source: Cesaraccio C et al. 2001. An improved model for determining degree-day values from daily temperature data. International Journal of Biometeorology 45:161-169.
#' 
#' @param t hour or hours for temperature estimate
#' 
#' @param tr sunrise hour (0-23)
#' 
#' @param ts sunset hour (0-23)
#' 
#' @param T_max maximum temperature of current day (C) 
#' 
#' @param T_min minimum temperature of current day (C)
#' 
#' @param T_minp minimum temperature of following day (C)
#' 
#' @return temperature (C) at a specified hour 
#' 
#' @keywords Temperature
#' 
#' @family microclimate functions
#' 
#' @export
#' 
#' @examples
#' diurnal_temp_variation_sinesqrt(t=8, tr=6, ts=18, T_max=30, T_min=10, T_minp=12)
#' 
diurnal_temp_variation_sinesqrt=function(t, tr, ts, T_max, T_min, T_minp){
 
  stopifnot(t>=0, t<=24, tr>=0, tr<=24, ts>=0, ts<=24, T_max>=T_min)
  
  #Time estimates
  tp = tr + 24 #sunrise time following day
  tx= ts - 4 #Assume time of maximum temperature 4h before sunset
  
  #Temperature at sunset
  c=0.39 #empircally fitted parameter
  To= T_max - c*(T_max-T_minp)
  
  alpha= T_max -T_min
  R = T_max - To
  b= (T_minp - To)/sqrt(tp -ts)
  
  T= rep(NA, length(t))
  
  inds=which(t<= tr) 
  ##t > ts & t <= tp ?
  if(length(inds>0))  T[inds]= To+b*sqrt(t[inds]-(ts-24) )
  ##t-ts? instead of t-(ts-24)?
  inds=which(t>tr & t<=tx) 
  if(length(inds>0)) T[inds]= T_min+ alpha*sin(pi/2*(t[inds]-tr)/(tx-tr))
  ##delete sin
  inds=which(t>tx & t<ts) 
  if(length(inds>0)) T[inds]= To+ R*sin(pi/2*(1+ (t[inds]-tx)/4) )
  
  inds=which(t>=ts) 
  if(length(inds>0))  T[inds]= To+b*sqrt(t[inds]-ts)
  ##delete
  return(T)
}
