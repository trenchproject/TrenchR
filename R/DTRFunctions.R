#' Estimate temperature across hours using a diurnal temperature variation function incorporating sine and exponential components 
#'
#' @details This function allows you to estimate temperature across hours using a diurnal temperature variation function incorporating sine and exponential components as in Parton and Logan (1981). We provide default values for alpha (2.59), beta (1.55), and gamma (2.2) for North Carolina from Wann 1985. See function for other parameterizations.
#' @param Tmx maximum daily temperature (C)
#' @param Tmn minimum daily temperature (C)
#' @param ts time of sunrise (hour)
#' @param tr time of sunset (hour)
#' @param Hr hour for temperature estimate
#' @param alpha  time difference between tx(time of maximum temperature) and noon
#' @param beta time difference between tx and sunrise
#' @param gamma decay parameter for rate of t change from sunset to tn(time of minimum temp)
#' @keywords Temperature
#' @export
#' @examples
#' \dontrun{
#' diurnal_temp_variation_sineexp(Tmx=40, Tmn=10, Hr=11, tr=18, ts=6, alpha=2.59, beta= 1.55, gamma=2.2)
#' }

diurnal_temp_variation_sineexp=function(Tmx, Tmn, Hr, tr, ts, alpha=2.59, beta= 1.55, gamma=2.2){
#Tmx= max temperature
#Tmn= min temperature
#Hr= hour of measurement (0-24)

#alpha, beta, gamma parameterizations
#Wann 1985
# Average of 5 North Carolina sites: alpha=2.59, beta= 1.55, gamma=2.2
#Parton and Logan 1981, parameterized for Denver, CO  
# 150cm air temeprature: alpha=1.86, beta= 2.20, gamma=-0.17
# 10cm air temperature: alpha=1.52, beta= 2.00, gamma=-0.18
# soil surface temperature: alpha=0.50, beta= 1.81, gamma=0.49
# 10cm soil temperature: alpha=0.45, beta= 2.28, gamma=1.83
  
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

#----------------------------------------

#' Diurnal temperature across hours
#' From Campbell and Norman 1998 - Uses sine interpolation
#'
#' @details This function allows you to estimate temperature for a specified hour using the sine interpolation in Campbell and Norman (1998).
#' @param Tmx maximum daily temperature in degree celsius 
#' @param Tmn minimum daily temperature in degree celsius
#' @param Hr hour for temperature estimate
#' @keywords Temperature
#' @export
#' @examples
#' \dontrun{
#' diurnal_temp_variation_sine(Tmx=30, Tmn=10, Hr=11)
#' }


diurnal_temp_variation_sine=function(Tmx, Tmn, Hr){
  #Tmx= max temperature
  #Tmn= min temperature
  
  W=pi/12;
  gamma= 0.44 - 0.46* sin(0.9 + W * Hr)+ 0.11 * sin(0.9 + 2 * W * Hr);   # (2.2) diurnal temperature function
  T = Tmx-Tmn * (1-gamma);
  
  return(T)
}

#----------------------------------------

#' Estimates temperature across hours
#' From Cesaraccio et al 2001 
#'
#' @details This function allows you to estimate temperature for a specified hour using sine and square root functions (Cesaraccio et al 2001).
#' 
#' @param Hr hour or hours for temperature estimate
#' @param tr sunrise hour (0-23)
#' @param ts sunset hour (0-23)
#' @param Tmx maximum temperature of current day (C) 
#' @param Tmn minimum temperature of current day (C)
#' @param Tmn_p minimum temperature of following day (C)
#' @keywords Temperature
#' @export
#' @examples
#' \dontrun{
#' diurnal_temp_variation_sinesqrt( Hr=8, tr=6, ts=18, Tmx=30, Tmn=20, Tmn_p=22)
#' }

diurnal_temp_variation_sinesqrt=function(Hr, tr, ts, Tmx, Tmn, Tmn_p){
 
  #Time estimates
  tp = tr + 24 #sunrise time following day
  tx= ts - 4 #Assume time of maximum temperature 4h before sunset
  
  #Temperature at sunset
  c=0.39 #empircally fitted parameter
  To= Tmx - c*(Tmx-Tmn_p)
  
  alpha= Tmx -Tmn
  R = Tmx - To
  b= (Tmn_p - To)/sqrt(tp -ts)
  
  T= rep(NA, length(Hr))
  
  inds=which(Hr<= tr) 
  if(length(inds>0))  T[inds]= To+b*sqrt(Hr[inds]-(ts-24) )
  
  inds=which(Hr>tr & Hr<=tx) 
  if(length(inds>0)) T[inds]= Tmn+ alpha*sin(pi/2*(Hr[inds]-tr)/(tx-tr))
  
  inds=which(Hr>tx & Hr<ts) 
  if(length(inds>0)) T[inds]= To+ R*sin(pi/2*(1+ (Hr[inds]-tx)/4) )
  
  inds=which(Hr>=ts) 
  if(length(inds>0))  T[inds]= To+b*sqrt(Hr[inds]-ts)
  
  return(T)
}
