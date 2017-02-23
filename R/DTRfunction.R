#' Estimate temperature across hours using a diurnal temperature variation function incorporating sine and exponential components 
#'
#' @details This function allows you to calculate temperature across hours using a diurnal temperature variation function incorporating sine and exponential components.
#' @param Tmx maximum daily temperature (C)
#' @param Tmn minimum daily temperature (C)
#' @param ts time of sunrise (hour)
#' @param tr time of sunset (hour)
#' @param Hr hour for temperature estimate
#' @param alpha  time difference between tx(time of maximum temperature) and noon
#' @param gamma decay parameter for rate of t change from sunset to tn(time of minimum temp)
#' @param beta time difference between tx and sunrise
#' @keywords Temperature
#' @export
#' @examples
#' Thour.sineexp()

#Function to calculate Parton and Logan 1981 diurnal variation
#Parameters for Colorado
#alpha=1.86
#gamma=2.20
#beta= -0.17

#Wann 1985
#alpha= 2.59 #time difference between tx and noon
#beta= 1.55 #time difference between tx and sunrise
#gamma= 2.2 #decay parameter for rate of t change from sunset to tn

#PAtterson 1981 function from Wann 1985
Thour.sineexp=function(Tmx, Tmn, Hr, tr, ts, alpha=2.59, beta= 1.55, gamma=2.2){
#Tmx= max temperature
#Tmn= min temperature
#Hr= hour of measurement (0-24)

l= ts-tr #daylength

tx= 0.5*(tr+ts)+alpha #time of maximum temperature
tn= tr+ beta #time of minimum temperature

#calculate temperature for nighttime hour
if(!(Hr > (tr + beta) & Hr < ts)) {
  Tsn = Tmn + (Tmx - Tmn) * sin((pi * (ts - tr - beta)) / (l + 2 * (alpha -
                                                                      beta)))
  if (Hr <= (tr + beta))
    Tas = Hr + 24 - ts
  if (Hr >= ts)
    Tas = Hr - ts  #time after sunset
  T = Tmn + (Tsn - Tmn) * exp(-(gamma * Tas) / (24 - l + beta))
}

#calculate temperature for daytime hour
if(Hr>(tr + beta) &
   Hr<ts) {
  T = Tmn + (Tmx - Tmn) * sin((pi * (Hr - tr - beta)) / (l + 2 * (alpha -
                                                                    beta)))
}
return(T)
}

