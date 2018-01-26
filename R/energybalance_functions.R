#' Calculate conductance
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate conductance of an ectotherm(Reptiles)
#' @param Ts Surface Temperature.
#' @param Ta Air Temperature.
#' @param mass Mass in grams.
#' @param lambda Mean thickness.
#' @param position Whether the organism is lying flat or standing.
#' @param K thermal conductivity
#' @param sa surface area in m^2
#' @return conductance
#' @keywords conductance
#' @export
#' @examples
#' \dontrun{
#' conductance(24,22,10.5,0.02,"flat","lizard")
#' }
#' 

conductance<-function(Ts,Ta, sa,lambda,K=0.5, position="flat"){
 
  ##TODO Make To / Tb a parameter with default Ta
  # Assume Initial body temperature to air temperature
  To = Ta
  
  ##TODO add units fal all paramters above
  ##TODO specify K units and look for options / values to generalize to ther species 
  ## Set the conductivity
  #K = case_when(
  #  taxa == "lizard" ~ 0.5,
  #  TRUE ~ 0.1
  #)
  
  ##TODO Might be beter as a parameter with default value
  # Calculate the area of contact
  A_contact = case_when(
    position == "flat" ~ 0.35*sa,
    position == "standing" ~ 0.05*sa
  )
  
  #conduction
  eb_conductance = A_contact*K*(Ts - To)/(lambda/2)

  
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
  
  # Calculate surface area
  sa= 0.0314*3.14159*mass_kg^(2./3.) #surface area m2
  
  return(sa)
}


#' Calculate convection
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate convection of an ectotherm(Reptiles)
#' @param Ts Surface Temperature.
#' @param Ta Air Temperature.
#' @param sa surface area in m^2
#' @param K thermal conductivity
#' @return convection
#' @keywords convection
#' @export
#' @examples
#' \dontrun{
#' convection(24,22,10.5,"lizard")
#' }
#' 

convection<-function(Ts,Ta, sa,K=0.5){
  
  ##TODO Add units to all parameters above
  
  ## TODO seems strange to set body temperature to zero, better to make To or Tb parameter
  # Assume Initial body temperature to 0
  To = 0
  
  ##TODO MAKE PARAMETER AND LOOK INTO GENERALIZING
  # Convective heat transfetr ceofficient (W m-2 K-1) 
  # (Porter et al. 1973)
  # TODO Case statement for the heat transfer coefficient
  h_L=10.45 
  
  ## TODO make parameter with default value
  #Calculate skin area exposed to air
  Aair = 0.9*sa # skin area that is exposed to air
  
  ##TODO Will eventually want to expand to other forms of convection
  ##TODO see Mitchell. 1976. Heat transfer from spheres and other animal forms, http://www.sciencedirect.com/science/article/pii/S0006349576857116
  #convection, assuming no wind
  eb_convection =   h_L*Aair*(Ta-To)
  
  
  return(eb_convection)
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
#' metabolic(24,10.5,"lizard")
#' }
#' 

metabolic_rate<-function(Tb=NA, mass, taxa="lizard"){
  

  # Metabolism - Buckley 2008, based on data for Sceloporus undulatus from Mike Angilletta
  #Check x3 is for activity
  ew = case_when(
    species == "lizard" ~ exp(-10.0+0.51*log(mass)+0.115*Tb ) *3/3600,
    TRUE ~ 1
  )
  
  #Nagy 2005, JEB
  #FMR in j/s, M is mass in grams
  #Convert 1 kJ/day=0.0115741
  ##TODO check conversion
  
  #reptiles
  ew = case_when(
    taxa == "reptile" ~ 0.196*mass^0.889 * 0.0115741,
    TRUE ~ 1
  )
  
  #mammals
  ew = case_when(
    taxa == "mammal" ~ 4.82*mass^0.734 * 0.0115741,
    TRUE ~ 1
  )
  
  #birds
  ew = case_when(
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
