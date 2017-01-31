#' Calculate degree days(using single sine wave approximation)
#' 
#' Single sine wave approximation from Baskerville & Emin 1969
#' Double Sine wave approximation of degree days from Allen 1976 
#' (see http://www.ipm.ucdavis.edu/WEATHER/ddss_tbl.html)
#' 
#' This function allows you to calculate degree days using single sine wave approximation.
#' @param Tmin Minimum temperature of the day.
#' @param Tmax Maximum temperature of the day.
#' @param LDT lower developmental threshold.
#' @keywords degree days
#' @export
#' @examples
#' degree.days()
#' 
degree.days=function(Tmin,Tmax,LDT){

# entirely above LDT
if(Tmin>=LDT) {dd=(Tmax+Tmin)/2-LDT}

# intercepted by LDT
## for single sine wave approximation
if(Tmin<LDT && Tmax>LDT){
alpha=(Tmax-Tmin)/2
theta1=asin(((LDT-(Tmax+Tmin))/alpha)*pi/180)
dd=1/pi*(((Tmax+Tmin)/2-LDT)*(pi/2-theta1)+alpha*cos(theta1))
if(!is.na(dd))if(dd<0){dd=0}
} #matches online calculation

# entirely below LDT
if(Tmax<=LDT){ dd=0}

return(dd)
}

#' Calculate degree days(using single sine wave approximation)
#' 
#' Single sine wave approximation from Baskerville & Emin 1969
#' Double Sine wave approximation of degree days from Allen 1976 
#' (see http://www.ipm.ucdavis.edu/WEATHER/ddss_tbl.html)
#' 
#' This function allows you to calculate degree days using single sine wave approximation.
#' @param Tdat - Vector containing Tmin and Tmax.
#' @param LDT lower developmental threshold.
#' @keywords degree days alternate
#' @export
#' @examples
#' degree.days.mat()
#' 
degree.days.mat=function(Tdat,LDT){
  Tmin=Tdat[1]
  Tmax=Tdat[2]
  
  # entirely above LDT
  if(Tmin>=LDT) {dd=(Tmax+Tmin)/2-LDT}
  
  # intercepted by LDT
  ## for single sine wave approximation
  if(Tmin<LDT && Tmax>LDT){
    alpha=(Tmax-Tmin)/2
    theta1=asin(((LDT-(Tmax+Tmin))/alpha)*pi/180)
    dd=1/pi*(((Tmax+Tmin)/2-LDT)*(pi/2-theta1)+alpha*cos(theta1))
    if(!is.na(dd))if(dd<0){dd=0}
  } #matches online calculation
  
  # entirely below LDT
  if(Tmax<=LDT){ dd=0}
  
  return(dd)
}

#' Calculate degree days using single sine wave approximation with thermoregulation
#'  
#' This function allows you to calculate degree days using single sine wave approximation assuming thermoregulation to Topt.
#' @param Tmin Minimum temperature of the day.
#' @param Tmax Maximum temperature of the day.
#' @param LDT lower developmental threshold.
#' @param Topt .
#' @param Trep .
#' @keywords degree days thermoregulation
#' @export
#' @examples
#' degree.days.thermoreg()
#' 
degree.days.thermoreg=function(Tmin,Tmax,LDT, Topt, Trep){

# entirely above LDT
if(Tmin>=LDT) {dd=(Tmax+Tmin)/2-LDT}

# intercepted by LDT
## for single sine wave approximation
if(Tmin<LDT && Tmax>LDT){
alpha=(Tmax-Tmin)/2
theta1=asin(((LDT-(Tmax+Tmin))/alpha)*pi/180)
dd=1/pi*(((Tmax+Tmin)/2-LDT)*(pi/2-theta1)+alpha*cos(theta1))
if(!is.na(dd))if(dd<0){dd=0}
}

# entirely below LDT
if(Tmax<=LDT){ dd=0}

dd.topt=NA
dd.s=NA
#area of topt box
dd.topt=sunh/24*(Topt-Trep)

#substract off duplicated area
alpha=(Tmax-Tmin)/2
theta1=asin(((Trep-(Tmax+Tmin))/alpha)*pi/180)
if(!is.na(theta1)) dd.s=1/pi*(((Tmax+Tmin)/2-Trep)*(pi/2-theta1)+alpha*cos(theta1))
if(!is.na(dd.s))  if(dd.s<0)dd.s=0

dd=dd+dd.topt-dd.s

return(dd)
}
