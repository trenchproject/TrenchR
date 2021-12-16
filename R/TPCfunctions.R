#' Construct a Gaussian-quadratic thermal performance curve. 
#'     
#' @details Construct a Gaussian-quadratic thermal performance curve.
#' @description Constructs a thermal performance curve by combining as a gaussian function to describe the rise in performance up to the optimal temperature and a quadratic decline to zero performance at critical thermal maxima and higher temperatures. Reference: Deutsch CA, Tewksbury JJ, Huey RB, Sheldon KS, Ghalambor CK, Haak DC, Martin PR (2008) Impacts of climate warming on terrestrial ectotherms across latitude. Proceedings of the National Academy of Science of the United States of America, 105, 6668– 6672.
#' @param T temperature range (C)
#' @param Topt thermal optima (C), the temperature at which peak performance occurs
#' @param CTmin critical thermal minima (C), the lower temperature limit for performance
#' @param CTmax critical thermal maxima (C), the upper temperature limit for performance
#' @return performance
#' @keywords thermal performance curve
#' @export
#' @examples
#' \dontrun{
#' TPC(T=0:60, Topt=30, CTmin=10, CTmax=40)
#' }

TPC<- function(T,Topt,CTmin, CTmax){
  F=T
  F[]=NA
  sigma= (Topt-CTmin)/4
  F[T<=Topt & !is.na(T)]= exp(-((T[T<=Topt & !is.na(T)]-Topt)/(2*sigma))^2) 
  F[T>Topt & !is.na(T)]= 1- ((T[T>Topt & !is.na(T)]-Topt)/(Topt-CTmax))^2
  #set negetative to zero
  F[F<0]<-0
  
  return(F)
}

#' Construct a thermal performance curve based on a beta function. 
#'  
#' @details Construct a thermal performance curve based on a beta function.
#' @description Construct a thermal performance curve based on a beta function. Reference: Asbury, D.A. & Angilletta, M.J. (2010) Thermodynamic effects on the evolution of performance curves. American Naturalist, 176, E40–E49.
#' @param T temperature (C)
#' @param shift mode of the thermal performance curve
#' @param breadth breadth of the thermal performance curve
#' @param aran scale performance value? if aran=0, no scaling; if aran=1, include a thermodynamic effect on mean performance.
#' @param tolerance maximal breath (C) of the thermal performance curve
#' @param skew skewness of the thermal performance curve (0-1)
#' @return performance
#' @keywords thermal performance curve
#' @export
#' @examples
#' \dontrun{
#' TPC.beta(T=0:60, shift=-1, breadth=0.1, aran=0, tolerance= 43, skew=0.7)
#' }

TPC.beta<- function(T, shift=-1, breadth=0.1, aran=0, tolerance= 43, skew=0.7){ 

  stopifnot(breadth>0, aran %in% c(0,1), tolerance>0, skew>=0, skew<=1)
  
  T = T + 273.15 #Convert temperature in degrees Celsius to Kelvin
  shift= shift + 273.15 #Convert temperature in degrees Celsius to Kelvin         
  z=rep(0.01, length(T))
  z[which(is.na(T))]=NA  #account for NAs
  sel= which(T-shift>=0 & T-shift<=tolerance)
  z[sel]= ((T[sel]-shift)/tolerance)^(skew/breadth-1)*(1-(T[sel]-shift)/tolerance)^((1-skew)/breadth-1) / beta(skew/breadth,(1-skew)/breadth) 
  if(aran==1) z[sel]=z[sel]*exp(-0.6/(T[sel]*8.61734*10^(-5)))*10^10 #add scaling factor
  return(z)
}
