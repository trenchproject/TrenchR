#ALLOMETRIC RELATIONSHIPS

#' Calculate surface area from mass 
#' 
#' @details This function allows you to calculate surface area (m^2) from mass (g) for a variety of taxa
#' @param mass Mass in grams.
#' @param taxa Which class of organism, current choices: lizard, frog, insect
#' @return sa (m^2)
#' @keywords surface area
#' @export
#' @examples
#'  \dontrun{
#'  calculate_surface_area(2,"insect")
#' }
#'

calculate_surface_area<-function(mass, taxa="lizard"){

  
  #lizard, O'Connor 1999 in Fei et al 2011
  #initial mass in kg
  if(taxa=="lizard") sa= 0.0314*pi*(mass/1000)^(2./3.)
  
  #Lizard,  Roughgarden 1981 from Norris (1965) and Porter and James (1979)
  #initial sa in mm^2
  #if(taxa=="lizard") sa=0.121*mass^0.688*(0.001)^2   
  
  #Frog, McClanahan and Baldwin 1969
  #initial sa in cm^2
  if(taxa=="frog") sa=9.9*mass^0.56*(0.01)^2 
  
  #Insects, mostly grasshoppers
  #Lactin and Johnson 1997
  if(taxa=="insect") sa=0.0013*mass^0.8 
  
  return(sa)
}

#' Calculate mass from length 
#' 
#' @details This function allows you to estimate mass (g) from length (m) for a variety of taxa
#' @param length length in m, length is snout-vent length for amphibians and reptiles (excepting turtles where length is carapace length).
#' @param taxa Which class of organism, current choices: insect, lizard, salamander, frog, snake, turtle 
#' @return mass (g)
#' @keywords mass length
#' @export
#' @examples
#'  \dontrun{
#'  calculate_mass(0.04,"insect")
#' }
#'

calculate_mass<-function(length, taxa){
  
  #convert m to mm
  lengthmm= length*1000
  
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
  lengthcm=length*100
  
  if(taxa=="salamander") mass= 0.018*lengthcm^2.94
  if(taxa=="frog") mass= 0.06*lengthcm^3.24
  if(taxa=="snake") mass= 0.00066*lengthcm^3.02
  if(taxa=="turtle") mass= 0.39*lengthcm^2.69
  
  return(mass)
}

#' Calculate surface area from volume (Based on Mitchell 1976) 
#' 
#' @details This function allows you to calculate surface area (m^2) from volume (m^3) for a variety of taxa
#' @param volume Density in m^3.
#' @param taxa Which class of organism, current choices: lizard, frog, sphere.
#' @return sa (m^2)
#' @keywords surface area
#' @export
#' @examples
#'  \dontrun{
#'  calculate_sa_volume(volume=0.001,"lizard")
#' }
#'

calculate_sa_volume<-function(volume, taxa){
  ##TODO CHECK, FIX UNITS
  
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
  sa= Ka * volume^(2/3) * .01 #surface area m2
  
  return(sa)
}

#' Calculate surface area from length (Based on Mitchell 1976) 
#' 
#' @details This function allows you to calculate surface area (m^2) from length(m) for a variety of taxa
#' @param length in meters.
#' @param taxa Which class of organism, current choices: lizard,frog, or sphere
#' @return sa (m^2)
#' @keywords surface area length
#' @export
#' @examples
#'  \dontrun{
#'  calculate_sa_length(.05,"lizard")
#' }
#'

calculate_sa_length<-function(length, taxa){
  
  #Kl and Ka are Empirical Constants(Mitchell 1976)
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
  # Calculate volume first
  V= (length/Kl)^(1/3)
  sa= Ka * (V^(2/3)) #surface area m2
  
  return(sa)
}

#' Calculate surface area from length approximating the animal's body as a rotational ellipsoid 
#' 
#' @details Calculate surface area (m^2) from length (m) approximating the animal's body as a rotational ellipsoid with half the body length as the semi-major axis q. 
#' @param length length in m.
#' @return sa (m^2)
#' @keywords surface area
#' @export
#' @examples
#'  \dontrun{
#'  calculate_surface_area_length_ellipsoid(length=0.04)
#' }
#'

calculate_surface_area_length_ellipsoid<-function(length){
  
  #to mm
  length=length*1000
  
  #Samietz et al. (2005)
  #inital units: mm
  #Area from Wolfram math world
  c<- length/2 #c- semi-major axis, a- semi-minor axis
  a<- (0.365+0.241*length*1000)/1000  #regression in Lactin and Johnson (1988)
  e=sqrt(1-a^2/c^2)
  sa=2*pi*a^2+2*pi*a*c/e*asin(e)
  
  #to m^2
  sa= sa/(1000^2)
  
  return(sa)
}

