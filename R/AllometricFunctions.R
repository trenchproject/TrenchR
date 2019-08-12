#ALLOMETRIC RELATIONSHIPS

#' Calculate surface area from mass 
#' 
#' @details This function allows you to estimate surface area (m^2) from mass (g) for a variety of taxa
#' @param m Mass in g
#' @param taxa Which class of organism, current choices: lizard, frog, insect
#' @return sa (m^2)
#' @keywords surface area
#' @export
#' @examples
#'  \dontrun{
#'  sa_from_mass(2,"insect")
#' }
#'

sa_from_mass<-function(m, taxa){

  
  #lizard, O'Connor 1999 in Fei et al 2011
  #initial mass in kg
  if(taxa=="lizard") sa= 0.0314*pi*(m/1000)^(2./3.)
  
  #Lizard,  Roughgarden 1981 from Norris (1965) and Porter and James (1979)
  #initial sa in mm^2
  #if(taxa=="lizard") sa=0.121*mass^0.688*(0.001)^2   
  
  #Frog, McClanahan and Baldwin 1969
  #initial sa in cm^2
  if(taxa=="frog") sa=9.9*m^0.56*(0.01)^2 
  
  #Insects, mostly grasshoppers
  #Lactin and Johnson 1997
  if(taxa=="insect") sa=0.0013*m^0.8 
  
  return(sa)
}

#' Calculate mass from length 
#' 
#' @details This function allows you to estimate mass (g) from length (m) for a variety of taxa
#' @param l Length in m, length is snout-vent length for amphibians and reptiles (excepting turtles where length is carapace length).
#' @param taxa Which class of organism, current choices: insect, lizard, salamander, frog, snake, turtle 
#' @return mass (g)
#' @keywords mass length
#' @export
#' @examples
#'  \dontrun{
#'  mass_from_length(0.04,"insect")
#' }
#'

mass_from_length<-function(l, taxa){
  
  #convert m to mm
  lengthmm= l*1000
  
    #Insect, Sample et al. 1993
    #also by orders and families
    #also has allometry with length * width
    #length in mm?
  if(taxa=="insect") mass= exp(-3.628)*lengthmm^2.494/1000
  ### TODO CHECK, INCLUDING UNITS
  
  #Lizards
  #Meiri 2010
  #also by clades and families
  #initial length in mm
  if(taxa=="lizard") mass= -4.852+3.088*log(lengthmm)
  
  #Below from Pough(1980)
  #inital length in cm
  
  #convert length 
  lengthcm=l*100
  
  if(taxa=="salamander") mass= 0.018*lengthcm^2.94
  if(taxa=="frog") mass= 0.06*lengthcm^3.24
  if(taxa=="snake") mass= 0.00066*lengthcm^3.02
  if(taxa=="turtle") mass= 0.39*lengthcm^2.69
  
  return(mass)
}

#' Calculate surface area from volume (Based on Mitchell 1976) 
#' 
#' @details This function allows you to estimate surface area (m^2) from volume (m^3) for a variety of taxa by approximating animal shape as a sphere. The function is intended for use in estimating convection as in Mitchell (1976).
#' @param V volume (m^3)
#' @param taxa Which class of organism, current choices: lizard, frog, sphere.
#' @return surface area (m^2)
#' @keywords surface area
#' @export
#' @examples
#'  \dontrun{
#'  sa_from_volume(0.001,"lizard")
#' }
#'

sa_from_volume<-function(V, taxa){

    #Kl and Ka are Empirical Constants(Mitchell 1976)
  
  # Case when taxa is Lizard (Norris 1965)
  Ka = dplyr::case_when(
    taxa == "lizard" ~ 11.0,
    TRUE ~ 1
  )
  
  # Case when taxa is Frog (Tracy 1972)
  Ka = dplyr::case_when(
    taxa == "frog" ~ 11.0,
    TRUE ~ 1
  )
  
  # Case when taxa is approximated as Sphere(Mitchell 1976)
  Ka = dplyr::case_when(
    taxa == "sphere" ~ 4.83,
    TRUE ~ 1
  )
  
  # Mitchell 1976
  # Calculate surface area
  sa= Ka * V^(2/3) * .01 #surface area m^2
  
  return(sa)
}

#' Calculate volume from length (Based on Mitchell 1976) 
#' 
#' @details This function allows you to estimate volume (m^3) from length (m) for a variety of taxa by approximating animal shape as a sphere. The function is intended for use in estimating convection as in Mitchell (1976).
#' @param l Length in m.
#' @param taxa Which class of organism, current choices: lizard,frog, or sphere
#' @return volume (m^3)
#' @keywords volume length
#' @export
#' @examples
#'  \dontrun{
#'   volume_from_length(.05,"lizard")
#' }
#'

volume_from_length<-function(l, taxa){
  
  #Kl and Ka are Empirical Constants (Mitchell 1976)
  # Case when taxa is Lizard (Norris 1965)
  Kl = dplyr::case_when(
    taxa == "lizard" ~ 3.3,
    TRUE ~ 1
  )
  
  # Case when taxa is Frog (Tracy 1972)
  Kl = dplyr::case_when(
    taxa == "frog" ~ 2.27,
    TRUE ~ 1
  )
  
  # Case when taxa is approximated as Sphere(Mitchell 1976)
  Kl = dplyr::case_when(
    taxa == "sphere" ~ 1.24,
    TRUE ~ 1
  )
  
  # Mitchell 1976
  # Calculate volume 
  V= (l/Kl)^(1/3) 
  
  return(V)
}

#' Calculate surface area from length by approximating the animal's body as a rotational ellipsoid 
#' 
#' @details Estimate surface area (m^2) from length (m) by approximating the animal's body as a rotational ellipsoid with half the body length as the semi-major axis. 
#' @param l Length in m.
#' @return sa (m^2)
#' @keywords surface area
#' @export
#' @examples
#'  \dontrun{
#'   sa_from_length(length=0.04)
#' }
#'

sa_from_length<-function(l){
  
  #to mm
  l=l*1000
  
  #Samietz et al. (2005)
  #inital units: mm
  #Area from Wolfram math world
  c<- l/2 #c- semi-major axis, a- semi-minor axis
  a<- (0.365+0.241*l*1000)/1000  #regression in Lactin and Johnson (1988)
  e=sqrt(1-a^2/c^2)
  sa=2*pi*a^2+2*pi*a*c/e*asin(e)
  
  #to m^2
  sa= sa/(1000^2)
  
  return(sa)
}

#' Calculate silhouette area
#' 
#' 
#' @details This function allows you to estimate the projected (silhouette) area as a portion of the surface area of the organism. Estimates the projected area as a function of zenith angle.
#' @param z zenith angle in degrees
#' @param taxa Which class of organism, current choices: frog, lizard, grasshopper
#' @param raz if lizard, relative solar azimuth angle in degrees, the horizontal angle of the sun (0-180 degrees) relative to the head and frontal plane of the lizard 
#' @param posture if lizard, indicate posture as "prostrate" or "elevated"
#' @return silhouette area as a proportion
#' @keywords silhouette area
#' @export
#' @examples
#' \dontrun{
  #' prop_silhouette_area(z=60, taxa= "frog")
#' }
#' 

prop_silhouette_area<-function(z, taxa, raz=0, posture="prostrate"){
  
  #frog, Tracy 1976
  if(taxa=="frog") psa=(1.38171*10^(-6)*z^4-1.93335*10^(-4)*z^3+4.75761*10^(-3)*z^2-0.167912*z+45.8228)/100
  #check area notation
  
  #lizards, Muth 1977
  if(taxa=="lizard"){
    if(posture=="prostrate" && raz==0){A=-2.3148*10^(-6); B=-2.1024*10^(-3); C=-4.6162*10^(-2); D=30.7316}
  
  ##TODO OTHER OPTIONS  == Done
    if(posture=="prostrate" && raz==90){A=-1.0185*10^(-5); B=1.3574*10^(-3); C=-9.5589*10^(-3); D=30.87255}
    
    if(posture=="prostrate" && raz==180){A=0; B=-2.7105*10^(-3); C=-6.3915*10^(-2); D=29.8534}
    
    if(posture=="elevated" && raz==0){A=3.6979*10^(-5); B=-4.7752*10^(-3); C=-6.4026*10^(-2); D=26.2831}
    
    if(posture=="elevated" && raz==90){A=0; B=-1.1756*10^(-4); C=-9.2594*10^(-2); D=26.2409}
    
    if(posture=="elevated" && raz==180){A=0; B=-1.5662*10^(-3); C=-5.6423*10^(-2); D=26.6833}
    
    
    psa= A*z^3+B*z^2+C*z+D                                       }                                                                                                                                
  #Grasshopper, Anderson et al. 1979
  if(taxa=="grasshopper") psa<-0.19-0.00173*z 
  
  return(psa/100)
}

#' Calculate silhouette area using the shape approximations
#' 
#' 
#' @details This function allows you to estimate the projected (silhouette) area as a portion of the surface area of the organism. Estimates the projected area as a function of the dimensions and the angle between the solar beam and the longitudinal axis of the solid. From Figure 11.6 in Campbell and Norman (1998).
#' @param shape Which shape to approximate an organism. Shapes are assumed to be prolate or have the longest axis parallel with the ground. Current choices: spheroid, cylinder flat ends, or cylinder hemisphere ends.
#' @param theta is the angle between the solar beam and the longitudinal axis in degrees
#' @param h is the height (long axis in m), cross section length for spheroid 
#' @param d is the diameter (short axis in m), cross section length for spheroid 
#' @return silhouette area as a proportion
#' @keywords silhouette area
#' @export
#' @examples
#' \dontrun{
#' prop_silhouette_area_shapes(shape="spheroid", theta=60, h=0.01, d=0.001)
#' prop_silhouette_area_shapes(shape="cylinder flat ends", theta=60, h=0.01, d=0.001)
#' prop_silhouette_area_shapes(shape="cylinder hemisphere ends", theta=60, h=0.01, d=0.001)
#' }

prop_silhouette_area_shapes<-function(shape, theta, h, d){
  
  #convert degree to radian
  theta_r= theta*(2*pi)/360
  
  #prolate spheroid
  if(shape=="spheroid") {
   x= d/h
   psa= sqrt(1+(x^2-1)*cos(theta_r)^2)/(2*x+ (2*sin(sqrt(1-x^2))^(-1)/sqrt(1-x^2)) ) #sin not converted to radians, check
  }
  
  if(shape=="cylinder flat ends") {
    psa= (cos(theta_r)+4*h*sin(theta_r)/(pi*d))/(2+4*h/d)
  }
  
  if(shape=="cylinder hemisphere ends") {
    psa= (1+4*h*sin(theta_r)/(pi*d))/(4+4*h/d)
  }
  
  return(psa)
}

