#' Diurnal temperature across hours
#' From Campbell and Norman 1998 - Uses sine interpolation
#'
#' @details This function allows you to calculate temperature across hours.
#' @param Tmx maximum daily temperature in degree celsius 
#' @param Tmn minimum daily temperature in degree celsius
#' @param Hr hour for temperature estimate
#' @keywords Temperature
#' @export
#' @examples
#' \dontrun{
#' Thour.sine()
#' }


Thour.sine=function(Tmx, Tmn, Hr){
  #Tmx= max temperature
  #Tmn= min temperature
  
  W=pi/12;
  gamma= 0.44 - 0.46* sin(0.9 + W * Hr)+ 0.11 * sin(0.9 + 2 * W * Hr);   # (2.2) diurnal temperature function
  T = Tmx-Tmn * (1-gamma);
  
  return(T)
}
