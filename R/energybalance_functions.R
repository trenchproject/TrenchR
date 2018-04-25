#' Calculate conductance
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate conductance of an ectotherm(Reptiles)
#' @param Ts Surface Temperature in Kelvin.
#' @param Ta Air Temperature in Kelvin.
#' @param Tb Body Temperature in Kelvin.
#' @param lambda Mean thickness(or diameter of the body) in meters.
#' @param K Thermal conductivity (W K^-1 m^-1 )
#' @param sa Surface area  in m^2
#' @param proportion In contact to the surface
#' @return conductance (W m^2)
#' @keywords conductance
#' @export
#' @examples
#' \dontrun{
#' conductance(246,227,227,0.05,0.5,.0025, 0.45)
#' }
#' 

conductance<-function(Ts,Ta,Tb=Ta,lambda,K=0.5, sa,proportion){
 
  ##TODO Make To / Tb a parameter with default Ta
  # Assume Initial body temperature to air temperature
  #To = Ta
  #Tb = Ta
  ##TODO add units fal all paramters above
  ##TODO specify K units and look for options / values to generalize to ther species 
  ## Set the conductivity
  #K = case_when(
  #  taxa == "lizard" ~ 0.5,
  #  TRUE ~ 0.1
  #)
  
  ##TODO Might be beter as a parameter with default value
  # Calculate the area of contact
  #A_contact = case_when(
  #  position == "flat" ~ 0.35*sa,
  #  position == "standing" ~ 0.05*sa
  #)
  
  # Calculate the area of contact
  A_contact  = sa * proportion
  
  # Default K(0.5) being used is for Lizard(Porter et al. (1973).)
  # Conduction
  # Calculating the heat loss (Difference between animal temperature and its environment)
  # m^2 * W K^-1 m^-1 * K / m
  eb_conductance = A_contact*K*(Ts - Tb)/(lambda/2)

  
  return(eb_conductance)
}

#' Calculate surface area from mass 
#' 
#' @details This function allows you to calculate surface area (m^2) from mass (g) for a variety of taxa
#' @param mass Mass in grams.
#' @param taxa Which class of organism, current choice: lizard.
#' @return sa
#' @keywords surface area
#' @export
#' @examples
#'

calculate_surface_area<-function(mass, taxa="lizard"){

  ##TODO GENERALIZE TO MORE SPECIES BY FINDING DATA
  ##TODO MAKE FUNCTION TO CALCULATE SURFACE AREA FROM BODY LENGTH, BOTH FIND DATA FOR TAXA AND MAKE GENERAL FUNCTIONS FOR CYLINDERS, ETC.
  
  # Mass in kg
  mass_kg=mass/1000. #in kg
  
  # If Mass is not given, approximate it by l/d = 10 relation.
  # and further, SA can it be approximated by  pi * l^2/10 ? 
  # Calculate surface area
  sa= 0.0314*3.14159*mass_kg^(2./3.) #surface area m2
  
  return(sa)
}

#' Calculate surface area from mass/density (Based on Mitchell 1976) 
#' 
#' @details This function allows you to calculate surface area (m^2) from mass (g) and density(g/cm^3) for a variety of taxa
#' @param mass Mass in grams.
#' @param density Density in grams/cm^3.
#' @param taxa Which class of organism, current choice: lizard.
#' @return sa (m^2)
#' @keywords surface area
#' @export
#' @examples
#'  \dontrun{
#'  calculate_sa(100,0.45,"lizard")
#' }
#'

calculate_sa<-function(mass, density, taxa="lizard"){
  
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
  sa= Ka * mass/(density^(2/3)) * .01 #surface area m2
  
  return(sa)
}

#' Calculate surface area from length (Based on Mitchell 1976) 
#' 
#' @details This function allows you to calculate surface area (m^2) from length(m) for a variety of taxa
#' @param length Characteristic dimension (trunk diameter or SVL) in meter.
#' @param taxa Which class of organism, current choice: lizard,frog or general 'sphere'.
#' @return sa (m^2)
#' @keywords surface area length
#' @export
#' @examples
#'  \dontrun{
#'  calculate_sa_length(.05,"lizard")
#' }
#'

calculate_sa_length<-function(length, taxa="lizard"){
  
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


#' Calculate convection
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate convection of an ectotherm(Reptiles)
#' @param Ta Air Temperature in Kelvin.
#' @param To Initial Body Temperature in Kelvin.
#' @param h_L Convective heat transfer ceofficient (W m^-2 K^-1)
#' @param sa Surface area  in m^2
#' @param proportion exposed to air
#' @return convection (W)
#' @keywords convection
#' @export
#' @examples
#' \dontrun{
#' convection(224,222,10.45,.0025, 0.85)
#' }
#' 

convection<-function(Ta,To,h_L=10.45,sa,proportion ){
  
  ##TODO Add units to all parameters above
  
  ## TODO seems strange to set body temperature to zero, better to make To or Tb parameter
  # Assume Initial body temperature to 0
  #To = 0
  
  ##TODO MAKE PARAMETER AND LOOK INTO GENERALIZING
  # Convective heat transfer ceofficient (W m^-2 K^-1) 
  # (Porter et al. 1973)
  # TODO Case statement for the heat transfer coefficient
  # h_L=10.45 
  
  ## TODO make parameter with default value
  # Calculate skin area exposed to air
  # Aair = 0.9*sa # skin area that is exposed to air
  
  # Calculate skin area exposed to air
  A_air = sa*proportion
  
  
  ##TODO Will eventually want to expand to other forms of convection
  ##TODO see Mitchell. 1976. Heat transfer from spheres and other animal forms, 
  ## http://www.sciencedirect.com/science/article/pii/S0006349576857116
  #convection, assuming no wind
  # W m^-2 K^-1 * m^2 * K
  eb_convection =   h_L*A_air*(Ta-To)
  
  
  return(eb_convection)
}

#' Calculate heat transfer coefficient for lizard
#' 
#' @details This function allows you get heat transfer coefficient for Lizard(Based on Porter et al. 1973)
#' @param A_v Air velocity m/s.
#' @param taxa Which class of organism, current choice: lizard
#' @return heat transfer coefficient(W m^-2 K^-1)
#' @keywords heat transfer coefficient 
#' @export
#' @examples
#' \dontrun{
#' heat_transfer_coefficient_lizard(3,"lizard")
#' }
#' 

heat_transfer_coefficient_lizard<-function(A_v,taxa="lizard"){
  
  #Convert air velocity from m/s to cm/sec
  A_v_cm = A_v/100
  
  # Case when orientation is parallel
  eb_hl_lizard = dplyr::case_when(
    orientation == "parallel" ~ 0.0038927 + (0.0001169 *A_v_cm ),
    TRUE ~ 1
  )
  
  # Case when orientation is transverse
  eb_hl_lizard = dplyr::case_when(
    orientation == "transverse" ~ 0.012132 + (0.000116 *A_v_cm ),
    TRUE ~ 1
  )
  
  #TODO - Need to verify the conversion
  # Convert from cal/minute / cm^2 /Celsius to W m^-2 K^-1
  # Used 1 calorie per minute ( cal/min ) = 0.070 watts ( W )
  eb_hl_lizard_SI <- (eb_hl_lizard * 0.070 * 100 ) 
    
  return(eb_hl_lizard_SI)
}


#' Calculate heat transfer coefficient (based on Mitchell 1976)
#' (Uses Table 1 which is Convective Heat Trasfer Relations to Animal Shapes)
#' 
#' @details This function allows you get heat transfer coefficient for various taxa(based Mitchell 1976)
#' @param A_v Air velocity m/s.
#' @param length Characteristic dimension (trunk diameter or SVL) in meter.
#' @param taxa Which class of organism, current choice: lizard,frog or general 'sphere'
#' @return heat transfer coefficient(W m^-2 K^-1)
#' @keywords heat transfer coefficient lizard
#' @export
#' @examples
#' \dontrun{
#' heat_transfer_coefficient(3,.05, "lizard")
#' }
#' 

heat_transfer_coefficient<-function(A_v,length, taxa="lizard"){
  
  # k(Thermal conductivity) and nu(Kinematic Viscocity) is 
  # based on air temperature at 20 deg C(293K) (Biophysical Ecology Gates)
  k= 25.7 * 10^(-3) # W m^-1 C^-1
  nu= 15.3 * 10^(-6) # m^2 s^-1
  
  #Dimensionless constant (Cl)
 
  # Case when taxa(animal shape) is sphere
  Cl= dplyr::case_when(
    taxa == "sphere" ~ 0.37,
    TRUE ~ 1
  )
  
  # Case when taxa(animal shape) is sphere
  Cl = dplyr::case_when(
    taxa == "lizard" ~ 0.35,
    TRUE ~ 1
  )
  
  #n
  # Case when taxa(animal shape) is sphere
  n= dplyr::case_when(
    taxa == "sphere" ~ 0.6,
    TRUE ~ 1
  )
  
  # Case when taxa(animal shape) is sphere
  n = dplyr::case_when(
    taxa == "lizard" ~ 0.6,
    TRUE ~ 1
  )
  
  #
  eb_hl_SI <- Cl *k * ((A_v * length/ nu )^n) /length
  
  return(eb_hl_SI)
}



#' Calculate metabolic expenditure
#' 
#' Caters to heat generated because of metabolism.
#' 
#' 
#' 
#' @details This function allows you to calculate basal or field metabolic rate (j/s) of various taxa (BMR or FMR depends on taxa)
#' @param Tb body temperature in degrees C, currently only used for lizard.
#' @param mass Mass in grams.
#' @param taxa Which taxa. options: lizard, reptile, bird, mammal
#' @return metabolic expense
#' @keywords metabolism
#' @export
#' @examples
#' \dontrun{
#' metabolic_rate(24,10.5,"reptile")
#' }
#' 

metabolic_rate<-function(Tb=NA, mass, taxa="reptile"){
  

  # Metabolism - Buckley 2008, based on data for Sceloporus undulatus from Mike Angilletta
  #Check x3 is for activity
  #eb_meta = case_when(
  #  species == "lizard" ~ exp(-10.0+0.51*log(mass)+0.115*Tb ) *3/3600,
  #  TRUE ~ 1
  #)
  
  #Nagy 2005, JEB
  #FMR in j/s, M is mass in grams
  #Convert 1 kJ/day=0.0115741
  ##TODO check conversion
  
  #reptiles
  eb_meta = case_when(
    taxa == "reptile" ~ 0.196*mass^0.889 * 0.0115741,
    TRUE ~ 1
  )
  
  #mammals
  eb_meta = case_when(
    taxa == "mammal" ~ 4.82*mass^0.734 * 0.0115741,
    TRUE ~ 1
  )
  
  #birds
  eb_meta = case_when(
    taxa == "bird" ~ 10.5 * mass^0.681 * 0.0115741,
    TRUE ~ 1
  )
  
  ##TODO Peferable to use Gilloolly et al. 2001 equation including temperature, http://science.sciencemag.org/content/293/5538/2248
  ##TODO See whether can find coefficients to include temperature from Gillooly, Savage, etc.
  ##TODO ?? Can further divide equations above using Nagy et al. 1999 ENERGETICS OF FREE-RANGING MAMMALS, REPTILES, AND BIRDS. Needed?
  
  return(eb_meta)
}

#' Calculate absorbed solar radiation
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate absorbed solar radiation of an ectotherm(Reptiles)

#' @param sa surface area in m^2
#' @param solar Downward solar radiation(W/m^2).
#' @param thermal_abs thermal absorptivity as a percent, default value is for lizards, Bartlett & Gates 1967.
#' @return solar radiation absorbed
#' @keywords Solar radiation absorbed
#' @export
#' @examples
#' \dontrun{
#' solar_radiation_absorbed(10.5,1000,"lizard")
#' }
#' 

solar_radiation_absorbed<-function( sa, solar,thermal_abs=0.965){
  
  # Absorbance of Lizard skin - thermal absoptivity, Bartlett & Gates 1967
  # Ranges between .95 and 1.0, Gates 1980
  ## TODO: See whether we should include values for other taxa 
  ## TODO: Look in Porter and Gates 1967 for these and other parameter values. Can't remember what's there. (http://onlinelibrary.wiley.com/doi/10.2307/1948545/full)
  
  ##TODO Look into generalizing
  #projected lizard area for direct and scattered solar radiation
  A_p = 0.4*A_L 
  
  #solar radiation
  eb_solar_radiation = thermal_abs*A_p*solar
  
  
  return(eb_solar_radiation)
}

# Heat loss because of evaporation ?
