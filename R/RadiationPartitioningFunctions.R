#' Partition solar radiation (W m^-2) into direct and diffuse components
#' IMPLEMENT OTHER MODELS IN Wong and Chow (2001, Applied Energy 69:1991-224)?
#' @details This function allows you to partition solar radiation (W m^-2) into direct and diffuse components using the Erbs et al. model presented in Wong and Chow (2001, Applied Energy 69:1991-224). Correction for CO from Olyphant 1984. Returns the direct and diffuse components of radiation in W m^-2.
#' 
#' @param rad is solar radiation in W m^-2
#' @param kt is the clearnex index (dimensionless), which is the ratio of the global solar radiation measured at the surface to the total solar radiation at the top of the atmosphere.
#' 
#' @keywords solar radiation
#' @export
#' @examples
#' \dontrun{
#' partition_solar_radiation(500, 0.5)
#'}

partition_solar_radiation=function(rad, kt){

  #kd- diffuse fraction
  if(kt<=0.22) kd= 1-0.09*kt
  if(kt>0.22 & kt<0.8) kd= 0.9511 -0.1604*kt +4.388*kt^2 -16.638*kt^3 +12.336*kt^4
  if(kt>=0.8) kd = 0.125 #Correction from 16.5 for CO from Olyphant 1984
  
  #return direct and diffuse
 return (c(rad*(1-kd),rad*(kd)) )
}  
