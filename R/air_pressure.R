#' Estimate air pressure in kPa (Kilo Pascal)
#' Credit - #http://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html
#'
#' @author 
#' @details 
#' @description  This function allows you to calculate estimated air pressure (kPa) as afunction of elevation
#' @param h height in meters.
#' @keywords air pressure
#' @export
#' @examples
#' airpressure_elev()
airpressure_elev<- function(h){  #H is height in meters 
  p= 101325* (1 - 2.25577*10^(-5)*h)^5.25588       
  p= p/1000 #convert to kPa
  return(p)
}
