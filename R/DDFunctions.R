#' Calculate degree days
#' 
#' Single sine wave approximation from  
#' Double Sine wave approximation of degree days from Allen 1976 
#' (see http://www.ipm.ucdavis.edu/WEATHER/ddss_tbl.html)
#' 
#' @details This function allows you to calculate degree days using single/double sine wave and single/double triangulation approximation.
#' @param Tmin Minimum temperature of the day.
#' @param Tmax Maximum temperature of the day.
#' @param LDT lower developmental threshold.
#' @param UDT upper developmental threshold.
#' @param method type of method being used
#' @keywords degree days
#' @export
#' @examples
#' \dontrun{
#' degree_days(7,14,12,33,"single.sine")
#' degree_days(7,14,12,33,"single.triangulation")
#' }
#' 

degree_days=function(Tmin,Tmax,LDT=NA,UDT=NA, method="single.sine"){
  #amplitude
  alpha=(Tmax-Tmin)/2 
  dd = 0
  #Single sine calculation
  if (method == "single.sine") {
    
    
    if (Tmin >= UDT && Tmax > UDT) { # entirely above both thresholds
      dd = (Tmax - Tmin)
     } else  if ( Tmin > LDT  && Tmax > UDT) { #Intercepted by upper threshold 
      theta2=asin((UDT-(Tmax+Tmin)/2)/alpha)
      dd = 1 / pi * (((Tmax + Tmin) / 2 - LDT) * (theta2 + pi / 2) + (UDT - LDT) *
                       (pi / 2 - theta2) - alpha * cos(theta2))
      } else  if (Tmin < LDT &&  Tmax > UDT ) {  #Intercepted by both thresholds
      theta2=asin((UDT-(Tmax+Tmin)/2)/alpha)
      theta1=asin((LDT-(Tmax+Tmin)/2)/alpha)
      dd = 1 / pi * ((  ((Tmax + Tmin) / 2) - LDT) * (theta2 - theta1) + alpha * (cos(theta1) -
             cos(theta2)) + (UDT - LDT) * (pi / 2 - theta2))
    } else if (Tmin > LDT &&  Tmax < UDT ) { #Entirely between both thresholds
      dd = ((Tmax + Tmin) / 2) - LDT
     } else if (Tmin < LDT && Tmax > LDT) {  # intercepted by LDT  
      #theta1=asin((LDT-(Tmax+Tmin)/2)/alpha)
      # credit - http://stackoverflow.com/questions/20998460/unexpected-behavior-in-asin
      #It's a floating point issue. The way floating point numbers work is that all 
      #numbers need to be mapped to the nearest one which can be expressed as a finite 
      #sum of powers of two and this may lead to small inaccuracies in the expected output 
      #and can be dependent upon how the numbers are calculated
      theta1= asin(pmax(-1,pmin(1,(LDT-(Tmax+Tmin)/2)/alpha)))
      dd = 1 / pi * (( ((Tmax + Tmin) / 2) - LDT) * ( (pi / 2) - theta1) + alpha * cos(theta1))
    } else if (Tmin < LDT && Tmax <= LDT) { # entirely below both thresholds
      dd = 0
       }

      
  } #end single sine method
  
  #double sine calculation
  if (method == "double.sine") {
    
    if (Tmin >= LDT && Tmax > UDT) { # entirely above both thresholds
      dd = (UDT - LDT) / 2
    } else if (Tmin > LDT  && Tmax > UDT) { #Intercepted by upper threshold
      theta2=asin((UDT-(Tmax+Tmin)/2)/alpha)
      dd = 1 / (2 * pi) * (((Tmax + Tmin) / 2 - LDT) * (theta2 + pi / 2) + (UDT -
          LDT) * (pi / 2 - theta2) - alpha * cos(theta2))
    } else if (Tmin < LDT &&  Tmax > UDT) { #Intercepted by both thresholds
      theta2=asin((UDT-(Tmax+Tmin)/2)/alpha)
      theta1=asin((LDT-(Tmax+Tmin)/2)/alpha)
      dd = 1 / (2 * pi) * (((Tmax + Tmin) / 2 - LDT) * (theta2 - theta1) + alpha *
                             (cos(theta1) - cos(theta2)) + (UDT - LDT) * (pi / 2 - theta2))
    } else if (Tmin > LDT &&  Tmax < UDT) { #Entirely between both thresholds
      dd = 0.5 * ((Tmax + Tmin) / 2 - LDT)
    } else if (Tmin < LDT && Tmax > LDT) { # intercepted by LDT
      theta1= asin(pmax(-1,pmin(1,(LDT-(Tmax+Tmin)/2)/alpha)))
      dd = 1 / (2 * pi) * (((Tmax + Tmin) / 2 - LDT) * (pi / 2 - theta1) + alpha *
                             cos(theta1))
    } else if (Tmin < LDT && Tmax <= LDT) { # entirely below both thresholds
      dd = 0
    }
  } #end double sine method
  
  

  #Single triangulation - with simplified formula
  if (method == "single.triangulation") {
    
    MT = (Tmax+Tmin)/2
    if (Tmin >= UDT && Tmax > UDT) { # entirely above both thresholds
      dd = (UDT - LDT)
    } else  if ( Tmin > LDT  && Tmax > UDT) { #Intercepted by upper threshold 
      dd = (MT-LDT)-((Tmax-UDT)^2/((Tmax-Tmin)*2))
    } else  if (Tmin < LDT &&  Tmax > UDT ) {  #Intercepted by both thresholds
      dd = ((Tmax-LDT)^2-(Tmax-UDT)^2)/((Tmax-Tmin)*2)
    } else if (Tmin > LDT &&  Tmax < UDT ) { #Entirely between both thresholds
      dd = MT-LDT
    } else if (Tmin < LDT && Tmax > LDT) {  # intercepted by LDT  
      dd = (Tmax-LDT)^2/((Tmax-Tmin)*2)
    } else if (Tmin < LDT && Tmax <= LDT) { # entirely below both thresholds
      dd = 0
    }
    
    
  } #end single triangulation method
  
  #Double triangulation - with simplified formula
  if (method == "double.triangulation") {
    
    MT = (Tmax+Tmin)/2
    if (Tmin >= UDT && Tmax > UDT) { # entirely above both thresholds
      dd = (UDT - LDT)/2
    } else  if ( Tmin > LDT  && Tmax > UDT) { #Intercepted by upper threshold 
      dd = (MT-LDT)-((Tmax-UDT)^2/((Tmax-Tmin)*4))
    } else  if (Tmin < LDT &&  Tmax > UDT ) {  #Intercepted by both thresholds
      dd = ((Tmax-LDT)^2-(Tmax-UDT)^2)/((Tmax-Tmin)*4)
    } else if (Tmin > LDT &&  Tmax < UDT ) { #Entirely between both thresholds
      dd = (MT/4)-(LDT/2)
    } else if (Tmin < LDT && Tmax > LDT) {  # intercepted by LDT  
      dd = (Tmax-LDT)^2/((Tmax-Tmin)*4)
    } else if (Tmin < LDT && Tmax <= LDT) { # entirely below both thresholds
      dd = 0
    }
    
    
  } #end double triangulation method
  

  
  return(round(dd,2))
}
