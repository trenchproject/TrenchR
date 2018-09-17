#' Estimate temperature across hours using a diurnal temperature variation function incorporating sine and exponential componenT_s 
#'
#' @details This function allows you to estimate temperature across hours using a diurnal temperature variation function incorporating sine and exponential componenT_s as in Parton and Logan (1981).
#' @param T_max maximum daily temperature (C)
#' @param T_min minimum daily temperature (C)
#' @param t_s time of sunrise (hour)
#' @param t_r time of sunset (hour)
#' @param t hour for temperature estimate
#' @param alpha  time difference between t_x(time of maximum temperature) and noon
#' @param gamma decay parameter for rate of t change from sunset to t_n(time of minimum temp)
#' @param beta time difference between t_x and sunrise
#' @keywords Temperature
#' @export
#' @examples
#' \donT_run{
#' Thour.sineexp()
#' }

#Function to calculate Parton and Logan 1981 diurnal variation
#Parameters for Colorado
#alpha=1.86
#gamma=2.20
#beta= -0.17

#Wann 1985
#alpha= 2.59 #time difference between T_x and noon
#beta= 1.55 #time difference between T_x and sunrise
#gamma= 2.2 #decay parameter for rate of t change from sunset to T_n

#PAtterson 1981 function from Wann 1985
diurnal_temp_variation_sineexp=function(T_max, T_min, t, t_r, t_s, alpha=2.59, beta= 1.55, gamma=2.2){
#T_max= max temperature
#T_min= min temperature
#t= hour of measurement (0-24)

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

#----------------------------------------

#' Diurnal temperature across hours
#' From Campbell and Norman 1998 - Uses sine interpolation
#'
#' @details This function allows you to estimate temperature for a specified hour using the sine interpolation in Campbell and Norman (1998).
#' @param T_max maximum daily temperature in degree celsius 
#' @param T_min minimum daily temperature in degree celsius
#' @param t hour for temperature estimate
#' @keywords Temperature
#' @export
#' @examples
#' \donT_run{
#' diurnal_temp_variation_sine()
#' }


diurnal_temp_variation_sine=function(T_max, T_min, t){
  #T_max= max temperature
  #T_min= min temperature
  
  W=pi/12;
  gamma= 0.44 - 0.46* sin(0.9 + W * t)+ 0.11 * sin(0.9 + 2 * W * t);   # (2.2) diurnal temperature function
  T = T_max-T_min * (1-gamma);
  
  return(T)
}

