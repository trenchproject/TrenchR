# IMPLEMENT OTHER MODELS IN Wong and Chow (2001, Applied Energy 69:1991-224)?

#' @details Partition solar radiation (W m^-2) into direct and diffuse components
#' @description This function allows you to partition solar radiation (W m^-2) into direct and diffuse components using the Erbs et al. model presented in Wong and Chow (2001, Applied Energy 69:1991-224). Correction for CO from Olyphant 1984. Returns the direct and diffuse components of radiation in W m^-2.
#' 
#' @param S is solar radiation in W m^-2
#' @param K_t is the clearnex index (dimensionless), which is the ratio of the global solar radiation measured at the surface to the total solar radiation at the top of the atmosphere.
#' 
#' @keywords solar radiation
#' @export
#' @examples
#' \dontrun{
#' partition.solar.radiation(500, 0.5)
#'}

partition.solar.radiation=function(S, K_t){

  #kd- diffuse fraction
  if(K_t<=0.22) kd= 1-0.09*K_t
  if(K_t>0.22 & K_t<0.8) kd= 0.9511 -0.1604*K_t +4.388*K_t^2 -16.638*K_t^3 +12.336*K_t^4
  if(K_t>=0.8) kd = 0.125 #Correction from 16.5 for CO from Olyphant 1984
  
  #return direct and diffuse
 return (c(S*(1-kd),S*(kd)) )
}  
