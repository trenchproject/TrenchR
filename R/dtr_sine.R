#' Temperature across hours
#'
#' This function allows you to calculate temperature across hours.
#' @param Tmx maximum daily temperature (C)
#' @param Tmn minimum daily temperature (C)
#' @param Hr hour for temperature estimate
#' @keywords Temperature
#' @export
#' @examples
#' Thour.sine()

#From Campbell and Norman 1998 uses sine interpolation

Thour.sine=function(Tmx, Tmn, Hr){
  #Tmx= max temperature
  #Tmn= min temperature
  
  W=pi/12;
  gamma= 0.44 - 0.46* sin(0.9 + W * Hr)+ 0.11 * sin(0.9 + 2 * W * Hr);   # (2.2) diurnal temperature function
  T = Tmx-Tmn * (1-gamma);
  
  return(T)
}
