library(ks)
#' Radiation with cloudiness
#' 
#' USE ERBS TO REPARTITION RADIATION (Olyphant 1984)
#' Separate Total radiation into components
#' kt is clearness index
#' Models presented in Wong and Chow. 2001. Applied Energy 69(2001):1991-224
#' Use Erbs et al model
#'
#' This function allows you to calculate direct and diffused radtation.
#' @param rad angle in radians
#' @param tau clearnex index #!specify
#' 
#' @keywords radiation
#' @export
#' @examples
#' radiation.erbs()
radiation.erbs=function(rad, tau){

  #USE ERBS TO REPARTITION RADIATION (Olyphant 1984)
  #Separate Total radiation into components
  #kt is clearness index
  # Models presented in Wong and Chow. 2001. Applied Energy 69(2001):1991-224
  #Use Erbs et al model
  
  #kd- diffuse fraction
  if(tau<=0.22) kd= 1-0.09*tau
  if(tau>0.22 & tau<0.8) kd= 0.9511 -0.1604*tau +4.388*tau^2 -16.638*tau^3 +12.336*tau^4
  if(tau>=0.8) kd[inds]= 0.125 #Correction from 16.5 for Niwot from Olyphant 1984
  
  #return direct and diffuse
 return (c(rad*(1-kd),rad*(kd)) )
}  

