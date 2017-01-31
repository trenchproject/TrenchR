#' Temperature across hours
#'
#' This function allows you to calculate temperature across hours.
#' @param Tmat Vector containing Tmin,Tmax, Start Time, and End Time
#' @keywords Temperature
#' @export
#' @examples
#' Thours.mat()
Thours.mat=function(Tmat, alpha=1.86, beta= -0.17, gamma=2.20){
  #Tmx= max temperature
  #Tmn= min temperature
  
  T=NA
  if( sum(is.na(Tmat))==0){
    
    Tmx= as.numeric(Tmat[1])
    Tmn= as.numeric(Tmat[2])
    tr= as.numeric(Tmat[3])
    ts= as.numeric(Tmat[4])
    
    l= ts-tr #daylength
    
    tx= 0.5*(tr+ts)+alpha #time of maximum temperature
    tn= tr+ beta #time of minimum temperature
    
    hrs= 1:24
    
    Tsn= Tmn+(Tmx-Tmn)*sin((pi*(ts-tr-beta))/(l+2*(alpha-beta)))
    
    #daytime
    T= Tmn+(Tmx-Tmn)*sin((pi*(hrs-tr-beta))/(l+2*(alpha-beta)))
    #am
    inds= which(hrs<= (tr+beta))
    T[inds]= Tmn+(Tsn-Tmn)*exp(-(gamma*(hrs[inds]+24-ts))/(24-l+beta))
    #pm
    inds= which(hrs>ts)
    T[inds]= Tmn+(Tsn-Tmn)*exp(-(gamma*(hrs[inds]-ts))/(24-l+beta))
    
  } #end check NA
  return(T)
}
