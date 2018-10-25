#' Estimate the diffuse fraction (k_d) for partitioning solar radiation
#' 
#' @details Estimate the diffuse fraction (k_d) for partitioning solar radiation (W m^-2) into direct and diffuse components
#' @description This function allows you to partition solar radiation (W m^-2) into direct and diffuse components by estimating the diffuse fraction (k_d) using the models presented in Wong and Chow (2001, Applied Energy 69:1991-224).
#' @param method to use for estimating the diffuse fraction, currently available: "Liu_Jordan", "Orgill_Hollands", "Erbs", "Olyphant", "Spencer", "Reindl-1", "Reindl-2", "Lam_Li"
#' @param kt is the clearness index (dimensionless), which is the ratio of the global solar radiation measured at the surface to the total solar radiation at the top of the atmosphere.
#' @param lat is latitude in degrees, needed if method ism Spencer
#' @param sol.elev is the solar elevation angles in degrees, needed if method is Reindl-2 
#' @return diffuse fraction
#' @keywords solar radiation
#' @export
#' @examples
#' \dontrun{
#' partition_solar_radiation(method="Erbs", kt=0.5, lat=40, sol.elev=60)
#'}
partition_solar_radiation=function(method, kt, lat=NA, sol.elev=NA){  
 
  # Methods from Wong and Chow (2001, Applied Energy 69:1991-224)
  
  #based on the correlations between the clearness index kt (dimensionless) and the diffuse fraction kd (dimensionless), diffuse coefficient kD (dimensionless) or the direct transmittance kb (dimensionless) where
  #k_t= I_t/I_o, k_d=I_d/I_t, k_D=I_d/I_o, k_b=I_b/I_o,
  #where I_t, I_b, I_d, and I_o are the global, direct, diffuse, and extraterrestial irradiances, respectively
  
  #kd- diffuse fraction
  
  #6.1 Liu and Jordan 
  if(method=="Liu_Jordon") kd= 0.384 -0.416*kt
  
  #6.2 Orgill and Hollands
  if(method=="Orgill_Hollands"){
    if(kt<0.35) kd= 1-0.249*kt
    if(kt>=0.35 & kt<=0.75) kd= 1.577-1.84*kt
    if(kt>=0.75) kd = 0.177 
  }
  
  #6.3 Erbs et al.
  if(method=="Erbs"){
  if(kt<=0.22) kd= 1-0.09*kt
  if(kt>0.22 & kt<0.8) kd= 0.9511 -0.1604*kt +4.388*kt^2 -16.638*kt^3 +12.336*kt^4
  if(kt>=0.8) kd = 0.165 #Correction from 0.125 for CO from Olyphant 1984
  }
  
  if(method=="Olyphant"){ #Correction for Colorado from Olyphant 1984
    if(kt<=0.22) kd= 1-0.09*kt
    if(kt>0.22 & kt<0.8) kd= 0.9511 -0.1604*kt +4.388*kt^2 -16.638*kt^3 +12.336*kt^4
    if(kt>=0.8) kd = 0.125 
  }
  
  #6.4 Spencer
  if(method=="Spencer"){
    a3= 0.94+0.0118*abs(lat)
    b3= 1.185+0.0135*abs(lat)
    
    if(kt>=0.35 & kt<=0.75) kd= a3-b3*kt
    ## CHECK OUTSIDE RANGE
  }
  
  #6.5 Reindl et al.
  if(method=="Reindl-1"){
    if(kt<=0.3) kd= 1.02-0.248*kt
    if(kt>0.3 & kt<0.78) kd= 1.45-1.67*kt
    if(kt>=0.78) kd = 0.147
  }
  
  if(method=="Reindl-2"){
    if(kt<=0.3) kd= 1.02-0.254*kt
    if(kt>0.3 & kt<0.78) kd= 1.4-1.749*kt+0.177*sin(sol.elev*180/pi)
    if(kt>=0.78) kd = 0.486*kt -0.182*sin(sol.elev*180/pi)
  }
  
  #6.6 Lam and Li
  if(method=="Lam_Li"){
    if(kt<=0.15) kd= 0.977
    if(kt>0.15 & kt<=0.7) kd= 1.237-1.361*kt
    if(kt>0.7) kd = 0.273
  }
  
  #direct and diffuse is c(rad*(1-kd),rad*(kd))
  
  return (kd)

  }  

