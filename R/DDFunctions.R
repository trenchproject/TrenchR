#' Calculate degree days
#' 
#' Single sine wave approximation from  
#' Double Sine wave approximation of degree days from Allen 1976 
#' (see http://www.ipm.ucdavis.edu/WEATHER/ddss_tbl.html)
#' 
#' This function allows you to calculate degree days using single or double sine wave approximation.
#' @param Tmin Minimum temperature of the day.
#' @param Tmax Maximum temperature of the day.
#' @param LDT lower developmental threshold.
#' @param UDT upper developmental threshold.
#' @param method type of method being used
#' @keywords degree days
#' @export
#' @examples
#' degree.days()
#' 

#NEEDS CHECKING, PARTICULARLY degrees / radians

degree.days=function(Tmin,Tmax,LDT=NA,UDT=NA, method="single.sine"){

  alpha=(Tmax-Tmin)/2 
  theta1=asin(((LDT-(Tmax+Tmin)/2)/alpha)*pi/180)
  theta2=asin(((UDT-(Tmax+Tmin)/2)/alpha)*pi/180)
  
#Single sine calculation
if(method=="single.sine"){
  
# entirely above both thresholds
if(Tmin>=LDT) {dd=(UDT-LDT)/2}

#Intercepted by upper threshold
if(Tmax>UDT && Tmin>LDT){
  dd=1/pi*(((Tmax+Tmin)/2-LDT)*(theta2+ pi/2)+(UDT-LDT)*(pi/2-theta2)-alpha*cos(theta2))
}
    
#Intercepted by both thresholds
  if(Tmax>UDT && Tmin<LDT){
    dd=1/pi*(((Tmax+Tmin)/2-LDT)*(theta2+ theta1)+alpha*(cos(theta1)-cos(theta2))+ (UDT-LDT)*(pi/2-theta2))
  }
  
#Entirely between both thresholds
  if(Tmax<UDT && Tmin>LDT) {dd=(Tmax-Tmin)/2-LDT}
  
# intercepted by LDT
  ## CHECK 1/pi to 1/2pi?
if(Tmin<LDT && Tmax>LDT){
dd=1/pi*(((Tmax+Tmin)/2-LDT)*(pi/2-theta1)+alpha*cos(theta1))
if(!is.na(dd))if(dd<0){dd=0}
}

# entirely below both thresholds
if(Tmax<=LDT){ dd=0}
} #end single sine method

#---------------------------  
#double sine calculation
  if(method=="double.sine"){
    
    # entirely above both thresholds
    if(Tmin>=LDT) {dd=(UDT-LDT)/2}
    
    #Intercepted by upper threshold
    if(Tmax>UDT && Tmin>LDT){
      dd=1/(2*pi)*(((Tmax+Tmin)/2-LDT)*(theta2+ pi/2)+(UDT-LDT)*(pi/2-theta2)-alpha*cos(theta2))
    }
    
    #Intercepted by both thresholds
    if(Tmax>UDT && Tmin<LDT){
      dd=1/(2*pi)*(((Tmax+Tmin)/2-LDT)*(theta2+ theta1)+alpha*(cos(theta1)-cos(theta2))+ (UDT-LDT)*(pi/2-theta2))
    }
    
    #Entirely between both thresholds
    if(Tmax<UDT && Tmin>LDT) {dd=0.5*((Tmax-Tmin)/2-LDT)}
    
    # intercepted by LDT
    if(Tmin<LDT && Tmax>LDT){
      dd=1/(2*pi)*(((Tmax+Tmin)/2-LDT)*(pi/2-theta1)+alpha*cos(theta1))
      if(!is.na(dd))if(dd<0){dd=0}
    }
    
    # entirely below both thresholds
    if(Tmax<=LDT){ dd=0}
  } #end double sine method
  
return(dd)
}