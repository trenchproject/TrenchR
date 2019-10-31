
#=======================
#Functions for TPCs add?
TPC.beta= function(T, shift=-1, breadth=0.1, aran=0, tolerance= 43, skew=0.7){ 
  T = T + 273.15 #Convert temperature in degrees Celsius to Kelvin
  shift= shift + 273.15 #Convert temperature in degrees Celsius to Kelvin         
  z=rep(0.01, length(T))
  z[which(is.na(T))]=NA  #account for NAs
  sel= which(T-shift>=0 & T-shift<=tolerance)
  z[sel]= ((T[sel]-shift)/tolerance)^(skew/breadth-1)*(1-(T[sel]-shift)/tolerance)^((1-skew)/breadth-1) / beta(skew/breadth,(1-skew)/breadth) 
  if(aran==1) z[sel]=z[sel]*exp(-0.6/(T[sel]*8.61734*10^(-5)))*10^10 #add scaling factor
  return(z)
}

#Performance Curve Function from Deutsch et al. 2008
TPC= function(T,Topt,CTmin, CTmax){
  F=T
  F[]=NA
  sigma= (Topt-CTmin)/4
  F[T<=Topt & !is.na(T)]= exp(-((T[T<=Topt & !is.na(T)]-Topt)/(2*sigma))^2) 
  F[T>Topt & !is.na(T)]= 1- ((T[T>Topt & !is.na(T)]-Topt)/(Topt-CTmax))^2
  #set negetative to zero
  F[F<0]<-0
  
  return(F)
}

