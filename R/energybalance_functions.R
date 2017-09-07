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
#' @param species Which species.
#' @return conductance
#' @keywords conductance
#' @export
#' @examples
#' \dontrun{
#' conductance(24,22,10.5,0.02,"flat","lizard")
#' }
#' 

conductance<-function(Ts,Ta, mass,lambda,position="flat", species="lizard"){
 
  # Assume Initial body temperature to air temperature
  To = Ta
  
  # Set the conductivity
  K = case_when(
    species == "lizard" ~ 0.5,
    TRUE ~ 0.1
  )
  
  # Mass in kg
  mass_kg=mass/1000. #in kg
  
  # Calculate surface area
  A_L= 0.0314*3.14159*mass_kg^(2./3.) #surface area m2
  
  # Calculate the area of contact
  A_contact = case_when(
    position == "flat" ~ 0.35*A_L,
    position == "standing" ~ 0.05*A_L
  )
  
  #conduction
  eb_conductance = A_contact*K*(Ts - To)/(lambda/2)

  
  return(eb_conductance)
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
#' @param mass Mass in grams.
#' @param species Which species.
#' @return convection
#' @keywords convection
#' @export
#' @examples
#' \dontrun{
#' convection(24,22,10.5,"lizard")
#' }
#' 

convection<-function(Ts,Ta, mass, species="lizard"){
  
  # Assume Initial body temperature to 0
  To = 0
  
  #convective heat transfetr ceofficient (W m-2 K-1) 
  #(Fei et al. 2012, J Ther Biol, 37: 56-64, Porter et al. 1973)
  # TODO Case statement for the heat transfer cooefficient 
  h_L=10.45 
  
  # Set the conductivity
  K = case_when(
    species == "lizard" ~ 0.5,
    TRUE ~ 0.1
  )
  
  # Mass in kg
  mass_kg=mass/1000. #in kg
  
  # Calculate surface area
  A_L= 0.0314*3.14159*mass_kg^(2./3.) #surface area m2
  
  #Calculate skin area exposed to air
  Aair = 0.9*A_L # skin area that is exposed to air
  
  #convection, assuming no wind
  eb_convection =   h_L*Aair*(Ta-To)
  
  
  return(eb_convection)
}

#' Calculate metabolic expenditure
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate metabolic expense of an ectotherm(Reptiles)
#' @param Ta Air Temperature.
#' @param mass Mass in grams.
#' @param species Which species.
#' @return metabolic expense
#' @keywords metabolism
#' @export
#' @examples
#' \dontrun{
#' metabolic(24,10.5,"lizard")
#' }
#' 

metabolic<-function(Ta, mass, species="lizard"){
  
  # Assume Initial body temperature to air temperature
  # Already in equilibrium 
  To = Ta
  
  # Metabolism - Buckley 2008
  ew = case_when(
    species == "lizard" ~ exp(-10.0+0.51*log(mass)+0.115*(To-273)) *3,
    TRUE ~ 1
  )
  
  #metabolic rate (j/s)
  eb_meta = ew/3600.
  
  return(eb_meta)
}

