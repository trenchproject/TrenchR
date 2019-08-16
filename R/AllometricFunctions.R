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
  
    #Insects
    #Sample BE, Cooper RJ, Greer RD, and Whitmore RC. 1993. Estimation of insect biomass by length and width. The American Midland Naturalist 129:234-240.
    #also by orders and families
    #also has allometry with length * width
    #length in mm?
  if(taxa=="insect") mass= exp(-3.628)*lengthmm^2.494/1000
  
  #predicts mass in mg so divide by 1000
  ### TODO CHECK, INCLUDING UNITS
  
  #Lizards
  #Meiri S. 2010. Length-weight allometries in lizards. Journal of Zoology 281: 218-226. 
  #also by clades and families
  #initial length in mm
  if(taxa=="lizard") mass= 10^(-4.852+3.022*log10(lengthmm))
  
  #Below from Pough. 1980. The Advantages of Ectothermy for Tetrapods. The American Naturalist 115: 92-112.
  #inital length in cm
  
  #convert length 
  lengthcm=l*100
  
  if(taxa=="salamander") mass= 0.018*lengthcm^2.94
  if(taxa=="frog") mass= 0.06*lengthcm^3.24
  if(taxa=="snake") mass= 0.00066*lengthcm^3.02
  if(taxa=="turtle") mass= 0.39*lengthcm^2.69
  
  return(mass)
}

#' Calculate surface area from volume. 
#' 
#' @details This function allows you to estimate surface area (m^2) from volume (m^3) for a variety of taxa by approximating animal shape as a sphere. The function is intended for use in estimating convection as in Mitchell (1976). Source: Mitchell JW. 1976. Heat transfer from spheres and other animal forms. Biophysical Journal 16: 561-569.
#' @param V volume (m^3)
#' @param taxa Which class of organism, current choices: lizard, frog, sphere.
#' @return surface area (m^2)
#' @keywords surface area
#' @export
#' @examples
#'  \dontrun{
#'  sa_from_volume(V=0.001,"lizard")
#' }
#'

sa_from_volume<-function(V, taxa){

    #Ka is an empirical Constants (Mitchell 1976)
  
  # Case when taxa is Lizard (Norris 1965)
  if(taxa == "lizard") Ka = 11.0
  # Case when taxa is Frog (Tracy 1972)
  if(taxa == "frog") Ka = 11.0
  # Case when taxa is approximated as Sphere(Mitchell 1976)
  if(taxa == "sphere") Ka = 4.83
  
  # Mitchell 1976
  # Calculate surface area
  sa= Ka * V^(2/3) 
  
  return(sa)
}

#' Calculate volume from length (Based on Mitchell 1976) 
#' 
#' @details This function allows you to estimate volume (m^3) from length (m) for a variety of taxa by approximating animal shape as a sphere. The function is intended for use in estimating convection as in Mitchell (1976). Source: Mitchell JW. 1976. Heat transfer from spheres and other animal forms. Biophysical Journal 16: 561-569.
#' @param l Length in m.
#' @param taxa Which class of organism, current choices: lizard,frog, or sphere
#' @return volume (m^3)
#' @keywords volume length
#' @export
#' @examples
#'  \dontrun{
#'   volume_from_length(l=0.05,"lizard")
#' }
#'

volume_from_length<-function(l, taxa){
  
  #Kl is an empirical constant (Mitchell 1976)
  # Case when taxa is Lizard (Norris 1965)
  if(taxa == "lizard") Kl = 3.3
  # Case when taxa is Frog (Tracy 1972)
  if(taxa == "frog") Kl = 2.27
  # Case when taxa is approximated as Sphere (Mitchell 1976)
  if(taxa == "sphere") Kl = 1.24
  
  # Mitchell 1976
  # Calculate volume 
  V= (l/Kl)^3
  
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
#'   sa_from_length(l=0.04)
#' }
#'

sa_from_length<-function(l){
  
  #to mm
  l=l*1000
  
  #Source: Samietz J, Salser MA & Dingle H. 2005. Altitudinal variation in behavioural thermoregulation: local adaptation vs. plasticity in California grasshoppers. Journal of Evolutionary Biology 18: 1087–1096.
  #inital units: mm
  #c- semi-major axis (half of grasshopper length), a- semi-minor axis (half of grasshopper width)
  c<- l/2
  a<- (0.365+0.241*l*1000)/1000  #regression in Lactin DJ & Johnson DL. 1998. Convective heat loss and change in body temperature of grasshopper and locust nymphs: relative importance of wind speed, insect size and insect orientation. Journal of Thermal Biology 23: 5–13.
  e=sqrt(1-a^2/c^2)
  sa=2*pi*a^2+2*pi*a*c/(e*asin(e))
  
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
#' @param raz if lizard, relative solar azimuth angle (degrees), the horizontal angle of the sun relative to the head and frontal plane of the lizard. Options are 0 (in front), 90 (to side), and 180 (behind) degrees. 
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
  
  #frog
  #Source: Tracy CR. 1976. A model of the dynamic exchanges of water and energy between a terrestrial amphibian and its environment. Ecological Monographs 46: 293-326.
  if(taxa=="frog") psa=(1.38171*10^(-6)*z^4-1.93335*10^(-4)*z^3+4.75761*10^(-3)*z^2-0.167912*z+45.8228)/100
  
  #lizards
  #Source: Muth A. 1977. 
  if(taxa=="lizard"){
    if(!raz %in% c(0,90,180)) stop("raz should be 0,90,or 180")  
    
    if(posture=="prostrate" && raz==0){A=-2.3148*10^(-6); B=-2.1024*10^(-3); C=-4.6162*10^(-2); D=30.7316}
  
    if(posture=="prostrate" && raz==90){A=-1.0185*10^(-5); B=1.3574*10^(-3); C=-9.5589*10^(-3); D=30.87255}
    
    if(posture=="prostrate" && raz==180){A=0; B=-2.7105*10^(-3); C=-6.3915*10^(-2); D=29.8534}
    
    if(posture=="elevated" && raz==0){A=3.6979*10^(-5); B=-4.7752*10^(-3); C=-6.4026*10^(-2); D=26.2831}
    
    if(posture=="elevated" && raz==90){A=0; B=-1.1756*10^(-4); C=-9.2594*10^(-2); D=26.2409}
    
    if(posture=="elevated" && raz==180){A=0; B=-1.5662*10^(-3); C=-5.6423*10^(-2); D=26.6833}
    
    
    psa= (A*z^3+B*z^2+C*z+D)/100}                                                                                                                                
  #Grasshopper
  #Source: Anderson, R.V., Tracy, C.R. & Abramsky, Z. (1979) Habitat selection in two species of short‐horned grasshoppers. Oecologia, 38, 359–374.
  if(taxa=="grasshopper") psa<-0.19-0.00173*z 
  
  return(psa)
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
   psa= sqrt(1+(x^2-1)*cos(theta_r)^2)/(2*x+ (2*asin(sqrt(1-x^2))/sqrt(1-x^2)) ) #sin not converted to radians, check
  }
  
  if(shape=="cylinder flat ends") {
    psa= (cos(theta_r)+4*h*sin(theta_r)/(pi*d))/(2+4*h/d)
  }
  
  if(shape=="cylinder hemisphere ends") {
    psa= (1+4*h*sin(theta_r)/(pi*d))/(4+4*h/d)
  }
  
  return(psa)
}

