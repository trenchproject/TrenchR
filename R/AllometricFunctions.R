#' @title Calculate organism surface area from mass 
#' 
#' @description Estimate surface area (m^2) from mass (g) for a variety of taxa. 
#' 
#' @param m Mass in grams (g). \insertCite{Simonis2021}{TrenchR}
#'
#' @param taxa Class of organism, current choices: \code{"lizard"}, \code{"salamander"}, \code{"frog"}, \code{"insect"}.
#' 
#' @return Surface area in meters squared (m^2).
#' 
#' @keywords surface area
#'
#' @family allometric functions
#' 
#' @details Relationships come from 
#'  \itemize{
#'    \item{lizard: \insertCite{Porter1979;textual}{TrenchR} and \insertCite{Porter1979;textual}{TrenchR} in \insertCite{Roughgarden1981;textual}{TrenchR}}
#'    \item{salamander: whit}
#'    \item{frog: \insertCite{McClanahan1969;textual}{TrenchR}}
#'    \item{insect: \insertCite{Lactin1997;textual}{TrenchR}}
#'  }
#'    
#' @references
#'   \insertAllCited{}
#'
#'
#' @examples
#'  sa_from_mass(m = 2, taxa = "lizard")
#'  sa_from_mass(m = 2, taxa = "salamander")
#'  sa_from_mass(m = 2, taxa = "frog")
#'  sa_from_mass(m = 2, taxa = "insect")
#'
#'
#' @export
#'
sa_from_mass <- function(m, taxa){

  stopifnot(taxa %in% c("lizard", "salamander", "frog", "insect"), m > 0)
  

  if (taxa == "lizard") {

    # initial mass in kg

    0.0314 * pi * (m / 1000) ^ (2 / 3)
  
  } else if (taxa == "salamander") {

    # convert cm^2 to m^2  

    8.42 * m ^ 0.694 / (100 * 100) 

  } else if (taxa == "frog") {

    9.9 * m ^ 0.56 * (0.01) ^ 2 

  } else if (taxa == "insect" ) {

    0.0013 * m ^ 0.8 

  } 
  
}

#' @title Calculate mass from length 
#' 
#' @description Estimate mass (g) from length (m) for a variety of taxa.
#'
#' @param l Length in meters (m), length is snout-vent length for amphibians and reptiles (excepting turtles where length is carapace length).
#'
#' @param taxa Class of organism, current choices: \code{"insect"}, \code{"lizard"}, \code{"salamander"}, \code{"frog"}, \code{"snake"}, \code{"turtle"}. 
#'
#' @return Mass in grams (g)
#'
#' @keywords mass length
#'
#' @family allometric functions
#' 
#' @details Relationships come from 
#'  \itemize{
#'    \item{insect: Sample et al. (1993)}
#'    \item{lizard: Meiri (2010)}
#'    \item{salamander: Pough (1980)}
#'    \item{frog: Pough (1980)}
#'    \item{snake: Pough (1980)}
#'    \item{turtle: Pough (1980)} 
#'  }
#'    
#' @references
#'  
#'  Meiri S. 2010. Length-weight allometries in lizards. Journal of Zoology 281: 218-226. 
#'  
#'  Pough. 1980. The Advantages of Ectothermy for Tetrapods. The American Naturalist 115: 92-112.
#'  
#'  Sample BE, Cooper RJ, Greer RD, and Whitmore RC. 1993. Estimation of insect biomass by length and width. The American Midland Naturalist 129:234-240.
#'
#'
#' @examples
#'  mass_from_length(l = 0.04, taxa = "insect")
#'  mass_from_length(l = 0.04, taxa = "lizard")
#'  mass_from_length(l = 0.04, taxa = "salamander")
#'  mass_from_length(l = 0.04, taxa = "frog")
#'  mass_from_length(l = 0.04, taxa = "snake")
#'  mass_from_length(l = 0.04, taxa = "turtle")
#'
#' @export
#'
mass_from_length <- function(l, taxa) {
  
  stopifnot(taxa %in% c("insect", "lizard", "salamander", "frog", "snake", "turtle"), l > 0)
  
  # convert m to mm and cm

  lengthmm <- l * 1000
  lengthcm <- l * 100  
    
  if (taxa == "insect") {
 
    # predicts mass in mg so divide by 1000

    exp(-3.628) * lengthmm ^ 2.494/1000
  
  } else if (taxa == "lizard") {

    10 ^ (-4.852 + 3.022 * log10(lengthmm))
  
  } else if (taxa == "salamander"){

    0.018 * lengthcm ^ 2.94

  } else if (taxa == "frog") {    

    0.06 * lengthcm ^ 3.24

  } else if (taxa == "snake") {

    0.00066 * lengthcm ^ 3.02

  } else if (taxa == "turtle") {

    0.39 * lengthcm ^ 2.69

  }
}


#
#  working here
#


#' Calculate surface area from volume. 
#' 
#' @details This function allows you to estimate surface area (m^2) from volume (m^3) for a variety of taxa by approximating animal shape as a sphere. The function is intended for use in estimating convection as in Mitchell (1976). Source: Mitchell JW. 1976. Heat transfer from spheres and other animal forms. Biophysical Journal 16: 561-569.
#' @param V volume (m^3)
#' @param taxa Which class of organism, current choices: lizard, frog, sphere.
#' @return surface area (m^2)
#' @keywords surface area
#' @family allometric functions
#' @export
#' @examples
#'  \dontrun{
#'  sa_from_volume(V=0.001,"lizard")
#' }
#'

sa_from_volume<-function(V, taxa){

  stopifnot(taxa %in% c("lizard", "frog", "sphere"), V>0)
  
    #Ka is an empirical Constants (Mitchell 1976)
  # Case when taxa is Lizard (Norris 1965)
  if(taxa == "lizard") Ka = 11.0
  # Case when taxa is Frog (Tracy 1972)
  if(taxa == "frog") Ka = 11.0
  # Case when taxa is approximated as Sphere (Mitchell 1976)
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
#' @param taxa Which class of organism, current choices: lizard, frog, or sphere
#' @return volume (m^3)
#' @keywords volume length
#' @family allometric functions
#' @export
#' @examples
#'  \dontrun{
#'   volume_from_length(l=0.05,"lizard")
#' }
#'

volume_from_length<-function(l, taxa){
  
  stopifnot(taxa %in% c("lizard", "frog", "sphere"), l>0)
  
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
#' @param l Length in m
#' @return sa (m^2)
#' @keywords surface area
#' @family allometric functions
#' @export
#' @examples
#'  \dontrun{
#'   sa_from_length(l=0.04)
#' }
#'

sa_from_length<-function(l){
  
  stopifnot(l>0)
  
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
#' @param posture if lizard, indicate posture as "prostrate" or "elevated" (default: prostrate)
#' @return silhouette area as a proportion
#' @keywords silhouette area
#' @family allometric functions
#' @export
#' @examples
#' \dontrun{
#' prop_silhouette_area(z=60, taxa= "frog")
#' }
#' 

prop_silhouette_area<-function(z, taxa, raz=0, posture="prostrate"){
  
  stopifnot(taxa %in% c("frog", "lizard", "grasshopper"), z>=0,z<360)
  
  #frog
  #Source: Tracy CR. 1976. A model of the dynamic exchanges of water and energy between a terrestrial amphibian and its environment. Ecological Monographs 46: 293-326.
  if(taxa=="frog") psa=(1.38171*10^(-6)*z^4-1.93335*10^(-4)*z^3+4.75761*10^(-3)*z^2-0.167912*z+45.8228)/100
  
  #lizards
  #Source: Muth A. 1977. Thermoregulatory Postures and Orientation to the Sun: A Mechanistic Evaluation for the Zebra-Tailed Lizard, Callisaurus draconoides. Copeia 4:710-720.
  if(taxa=="lizard"){
    if(!raz %in% c(0,90,180)) stop("raz should be 0,90,or 180") 
    if(!posture %in% c("prostrate","elevated")) stop("posture should be prostrate or elevated")
    
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
#' @details This function allows you to estimate the projected (silhouette) area as a portion of the surface area of the organism. Estimates the projected area as a function of the dimensions and the angle between the solar beam and the longitudinal axis of the solid. From Figure 11.6 in Campbell and Norman. 1998. An Introduction to Environmental Biophysics.
#' @param shape Which shape to approximate an organism. Shapes are assumed to be prolate or have the longest axis parallel with the ground. Current choices: spheroid, cylinder flat ends, or cylinder hemisphere ends.
#' @param theta is the angle between the solar beam and the longitudinal axis in degrees
#' @param h is the height (long axis in m), cross section length for spheroid 
#' @param d is the diameter (short axis in m), cross section length for spheroid 
#' @return silhouette area as a proportion
#' @keywords silhouette area
#' @family allometric functions
#' @export
#' @examples
#' \dontrun{
#' prop_silhouette_area_shapes(shape="spheroid", theta=60, h=0.01, d=0.001)
#' prop_silhouette_area_shapes(shape="cylinder flat ends", theta=60, h=0.01, d=0.001)
#' prop_silhouette_area_shapes(shape="cylinder hemisphere ends", theta=60, h=0.01, d=0.001)
#' }

prop_silhouette_area_shapes<-function(shape, theta, h, d){
  
  stopifnot(shape %in% c("spheroid", "cylinder flat ends", "cylinder hemisphere ends"), theta>=0,theta<360, h>=0, d>=0)
  
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

