#' Calculate conductance assuming animal thermal conductivity is rate limiting
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate conductance (W) of an ectothermic animal to its substate. Method assumes the major resistance to conduction is within surface layers of the animal and that the interior of the animal is equal in temperature to its surface (thermally well mixed). Reference: Spotila et al. 1992. Biophysicas of Heat and Mass Transfer.
#' @param Ts Surface Temperature in Kelvin.
#' @param Tb Body Temperature in Kelvin.
#' @param lambda Mean thickness of the animal skin (surface) in meters, assumes well mixed interior
#' @param K Thermal conductivity (W K^-1 m^-1 ), K=0.5 W K^-1 m^-1 for naked skin, K=0.15 for insect cuticle ( Galushko et al 2005); conductivity of ground is generally greater than that of animal tissues, so animal thermal conductivity is generally rate limiting step. 
#' @param sa Surface area  in m^2
#' @param proportion In contact to the surface
#' @return conductance (W)
#' @keywords conductance
#' @export
#' @examples
#' \dontrun{
#' conductance_animal(Ts= 293,Tb=303,lambda=10^-6,K=0.5,sa=10^-3, proportion=0.2)
#' }
#' 

conductance_animal<-function(Ts,Tb,lambda,K=0.5, sa,proportion){
 
  # Calculate the area of contact
  A_contact  = sa * proportion
  
  # Conduction
  # Calculating the heat loss (Difference between animal temperature and its environment)
  # m^2 * W K^-1 m^-1 * K / m
  eb_conductance = A_contact*K*(Ts - Tb)/(lambda)

  return(eb_conductance)
}

#' Calculate conductance assuming substrate thermal conductivity is rate limiting
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate conductance (W) of an ectothermic animal to its substate.
#'          Method assumes the major resistance to conduction is the substrate and that the interior of the 
#'          animal is equal in temperature to its surface (thermally well mixed). 
#'          Reference: Spotila et al. 1992. Biophysicas of Heat and Mass Transfer.
#' @param Ts Surface Temperature in Kelvin.
#' @param Tb Body Temperature in Kelvin.
#' @param D Characteristic dimension of the animal in meters
#' @param K_g Thermal conductivity of substrate (W K^-1 m^-1 )
#' @param sa Surface area  in m^2
#' @param proportion In contact to the surface
#' @return conductance (W)
#' @keywords conductance
#' @export
#' @examples
#' \dontrun{
#' conductance_substrate(Ts= 293,Tb=303,D=0.01,K=0.3,sa=10^-2, proportion=0.2)
#' }
#' 

conductance_substrate<-function(Ts,Tb,D,K_g=0.5, sa,proportion){
  
  # Calculate the area of contact
  A_contact  = sa * proportion
  
  #Thermal conductance between animal and substrate
  H_g= 2.0*K_g/D
  
  # Conduction,  m^2 * W K^-1 m^-1 * K / m
  eb_conductance = A_contact*H_g*(Tb - Ts)
  
  return(eb_conductance)
}



#' Calculate convection
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate convection. Includes enhncement factor associated for outdoor environments in Mitchell (1976).
#' @param Ta Air Temperature in Kelvin.
#' @param Tb Initial Body Temperature in Kelvin.
#' @param h_L Convective heat transfer coefficient (W m^-2 K^-1)
#' @param sa Surface area  in m^2
#' @param proportion of surface area exposed to air
#' @param ef is the enhancement factor, used to adjuct h_L to field condictions (using h_L approximation from Mitchell 1976).  Approximated as 1.3 by default, but see Mitchell 1976 for more relationship.
#' @return convection (W)
#' @keywords convection
#' @export
#' @examples
#' \dontrun{
#' convection(224,222,10.45,.0025, 0.85)
#' }
#' 

convection<-function(Ta,Tb,h_L=10.45,sa,proportion, ef=1.3 ){
  
  # Calculate skin area exposed to air
  A_air = sa*proportion
  
  
  ##TODO Will eventually want to expand to other forms of convection
  ##TODO see Mitchell. 1976. Heat transfer from spheres and other animal forms, 
  ## http://www.sciencedirect.com/science/article/pii/S0006349576857116
  #convection, assuming no wind
  # W m^-2 K^-1 * m^2 * K
  eb_convection =   ef*h_L*A_air*(Ta-Tb)
  
  return(eb_convection)
}

#' Calculate heat transfer coefficient for lizard
#' 
#' @details This function allows you estimate the heat transfer coefficient for a lizard(Based on Porter et al. 1973)
#' @param A_v Air velocity m/s.
#' @param orientation parallel or transverse
#' @return heat transfer coefficient(W m^-2 K^-1)
#' @keywords heat transfer coefficient 
#' @export
#' @examples
#' \dontrun{
#' heat_transfer_coefficient_lizard(3,"lizard")
#' }
#' 

heat_transfer_coefficient_lizard<-function(A_v,orientation="parallel"){
  
  #Convert air velocity from m/s to cm/sec
  A_v_cm = A_v*100
  
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
#' @details This function allows you estiamte the heat transfer coefficient for various taxa (based Mitchell 1976).  Approximates forces convective heat transfer for animal shapes using convective relationship for a sphere.
#' @param A_v Air velocity m/s.
#' @param D Characteristic dimension (e.g., diameter or snout-vent length) in meters.
#' @param k Thermal conductivity of air, W m^-1 K^-1, can calculate using DRYAIR or WETAIR in NicheMapR
#' @param nu Kinematic Viscocity of air, m^2 s^-1, can calculate using DRYAIR or WETAIR in NicheMapR
#' @param taxa Which class of organism, current choices: sphere,cylinder,frog,lizard_surface,lizard_elevated,flyinginsect,spider
#' @return heat transfer coefficient(W m^-2 K^-1)
#' @keywords heat transfer coefficient
#' @export
#' @examples
#' \dontrun{
#' heat_transfer_coefficient(3,.05,k= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "cylinder")
#' }
#' 

heat_transfer_coefficient<-function(A_v,D,k,nu, taxa="cylinder"){
  
  ##TODO Check unit: K or C
  
  taxas= c("sphere","cylinder","frog","lizard_surface","lizard_elevated","flyinginsect","spider")
  #cylinder assumes 40<Re<4000
  #lizard assumes prostrate on or elevated above surface, average for parallel and perpendicular to air flow
  
  # Dimensionless constant (Cl)
  Cls= c(0.37,0.615,0.258,1.36,1.91,0.0749,0.47)
  ns= c(0.6,0.466,0.667,0.39,0.45,0.78,0.5) 
  
  #find index  
  ind= match(taxa, taxas)
  
  eb_hl_SI <- Cls[ind] *k * ((A_v * D/ nu )^ns[ind]) /D
  
  return(eb_hl_SI)
}

#' Calculate absorbed solar and thermal radiation
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate solar and thermal radiation (W) absorbed by the surface of an animal. 
#'          Follows Gates Biophysical Ecology and Spotila et al. 1992.
#' @param abs solar absorptivity of animal surface (proportion), default value is for reptiles
#' @param As surface area  in m^2
#' @param psa_dir proportion surface area exposed to solar radiation
#' @param psa_ref proportion surface area exposed to reflected solar radiation
#' @param R_dir direct solar radiation (W/m^2)
#' @param R_dif diffuse solar radiation (W/m^2)
#' @param R_ref reflected solar radiation (W/m^2), either provided or estimated if surface albedo is provided instead
#' @param rho_S is surface albedo (proportion), optional (not used) if reflected radiation is provided
#' @return solar radiation absorbed (W)
#' @keywords Solar radiation absorbed
#' @export
#' @examples
#' \dontrun{
#' radiation_absorbed(abs=0.9, As=1, psa_dir=0.4, psa_ref=0.4, R_dir=1000, R_dif=200, rho_s=0.5)
#' }
#' 

radiation_absorbed<-function(abs=0.9, As, psa_dir, psa_ref, R_dir, R_dif, R_ref=NA, rho_s=NA){
  
 ## TODO: Add function with absorbances for other taxa, Many values in Gates Biophysical ecology Table 8.2
 
#Calculate A_ref if not provided
  if( is.na(R_ref)) R_ref= rho_s*R_dir
  
 #Areas
  A_dir = As*psa_dir
  A_dif= A_dir
  A_ref = As*psa_ref
  
  #solar radiation
  R_abs= abs*A_dir*R_dir + abs*A_dif*R_dif + abs*A_ref*R_ref
  
  return(R_abs)
}

#' Calculate emitted thermal radiation
#' 
#' 
#' @details This function allows you to estimate thermal radiation (W) emitted by the surface of an animal. Follows Gates Biophysical Ecology and Spotila et al. 1992.
#' @param emissivity longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param As surface area  in m^2
#' @param psa_dir proportion surface area exposed to sky (or enclosure)
#' @param psa_ref proportion surface area exposed to ground
#' @param Tb body surface temperatue in K
#' @param Ts ground surface temperatue in K
#' @param Ta ambient air temperature in K, only required if animal is in enclosed environment
#' @param enclosed TRUE or FALSE
#' @return emitted thermal radiation (W)
#' @keywords emitted thermal radiation
#' @export
#' @examples
#' \dontrun{
#' thermal_radiation_emitted(emissivity=0.96, As=1, psa_dir=0.4, psa_ref=0.6, Tb=303, Ts=293, Ta=298, enclosed=FALSE)
#' }
#' 

thermal_radiation_emitted<-function(emissivity=0.96, As, psa_dir, psa_ref, Tb, Ts, Ta, enclosed=FALSE){
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #Areas
  A_s = As*psa_dir
  A_r = As*psa_ref
  
  #estimate effective radiant temperature of sky
  #Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
  Tsky= (1.22*(Ta-273.15) -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation
  
  
  if(enclosed) 
         R= A_r*emissivity*sigma*(Tb^4 - Ta^4)
  else 
          R= emissivity*sigma*(A_s*(Tb^4 - Tsky^4)+A_r*(Tb^4 - Ts^4))

  return(R)
}


#' Calculate heat loss associated with evaporative water loss
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to estimate heat loss associated with evaporative water loss by an ampbian (Spotila et al. 1992) or lizard (Empirical measurements in Porter et a. 1973).
#' @param emissivity longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param As surface area  in m^2
#' @param Tb body temperatue in K
#' @param taxa taxa current choices: lizard, amphibian_wetskin (fully wet skin), amphibian (not fully wet skin)
#' @param rho_s saturation water vapor density at skin surface (kg/m^3) (needed if amphibian)
#' @param rho_a saturation water vapor density in ambient air (kg/m^3) (needed if amphibian)
#' @param RH relative humidity (0-1) (needed if amphibian)
#' @param h_C Convective heat transfer coefficient (W m^-2 C^-1) (needed if amphibian)
#' @param r_i internal (cutaneous) resistance to vapor transport (s/m) (needed if amphibian)
#' @return evaporative heat loss (W)
#' @keywords evaporative heat loss
#' @export
#' @examples
#' \dontrun{
#' evaporative_heat_loss(As=0.1, Tb=293, taxa="amphibian", rho_s=1.2, rho_a=1.2, RH=0.5, h_c=20, r_i=5000)
#' evaporative_heat_loss(As=0.1, Tb=293, taxa="lizard")
#' }
#' 

evaporative_heat_loss<-function(As, Tb, taxa, rho_s=NA, rho_a=NA, RH=NA, h_c=NA, r_i=NA){
  #TODO LOOK INTO PARAMETERIZATIONS? FIX UNITS.
  
  #Porter et al. 1973 in Gates Biophysical ecology
  if(taxa=="lizard"){ 
    if(Tb<293) E_kg= 0.27
    if(Tb>=293 & Tb<=309) E_kg= 0.08*exp(0.586)*(Tb-273.5)
    if(Tb>309) E_kg= 2.97*10^(-3)*exp(0.1516)*(Tb-273.5) 
    
    #convert from W/kg to W/m2
    E= E_kg*0.067/0.018 #for 0.067kg lizard with 0.018m^2 surface area
    
    #multiply by surface area
    E= E*As
    }

  #Spotila et al. 1992
  evap_heat= 2.44*10^(-6) #J/kg at most temperatures
  
  rhocp= 1200 #J*m^(-3)*C^(-1)  
  #  external (convective) resistance to water vapor transport (s/m)
  r_e= 0.93*rhocp/h_c
  
  if(taxa=="amphibian_wetskin"){ 
  #Ec= rate of water transport (kg/s)
  Ec= As *(1/r_e)*(rho_s-RH*rho_a)
  #to W
  E= Ec*evap_heat
  }
  
  if(taxa=="amphibian"){ 
  Ec= As * (rho_s-RH*rho_a)/(r_i+r_e)
  #to W
  E= Ec*evap_heat
  }  
    
  return(E)
}


#' Calculate metabolism as a function of mass
#' 
#' 
#' 
#' 
#' @details This function allows you to estiamte field metabolic rate (W) of various taxa as a function of mass(g). Does not account for temperature. Uses empirical relationships from Nagy et al. 1999.
#' @param mass Mass in grams.
#' @param taxa Which taxa. options: reptile, bird, mammal
#' @return metabolim (W)
#' @keywords metabolism
#' @export
#' @examples
#' \dontrun{
#' mr_from_mass(10.5,"reptile")
#' }
#' 

mr_from_mass<-function(mass, taxa="reptile"){
  
  #Nagy 2005, JEB
  #FMR in W, M is mass in grams
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
  
 ##TODO ?? Can further divide equations above using Nagy et al. 1999 ENERGETICS OF FREE-RANGING MAMMALS, REPTILES, AND BIRDS. Needed?
  
  return(eb_meta)
}

#' Calculate basal (or resting) metabolism as a function of mass and body temperature.
#' 
#' 
#' 
#' 
#' @details This function allows you to estimate basal (or resting) metabolic rate (W) as a function of mass (g) and temperature (K). Based on empirical data and the metabolic theory of ecology (3/4 scaling exponent). From Gilooly et al. 2001.
#' @param mass Mass in grams.
#' @param Tb body temperature in K
#' @param taxa Which taxa. options: bird, mammal, reptile, amphibian, invertebrate.
#' @return metabolim (W)
#' @keywords metabolism
#' @export
#' @examples
#' \dontrun{
#' mr_from_mass_temp(mass=100, Tb=303,"reptile")
#' }
#' 

mr_from_mass_temp<-function(mass,Tb, taxa){
  
  #From  Gilloolly et al. 2001 
  if(taxa=="bird" | taxa=="mammal") mr= exp(-9100/Tb+29.49)*mass^0.75/60
  if(taxa=="reptile") mr= exp(-8700/Tb+26.85)*mass^0.75/60
  if(taxa=="amphibian") mr= exp(-5760/Tb+16.68)*mass^0.75/60
  if(taxa=="invertebrate") mr= exp(-9150/Tb+27.62)*mass^0.75/60
  return(mr)
}

#==================================
#REPLACE WITH ABOVE?

solar_radiation_absorbed<-function(sa,psa, solar,thermal_abs=0.95){
  
  ## TODO: Add function with absorbances for other taxa, Many values in Gates Biophysical ecology
  
  #projected area for direct and scattered solar radiation
  A_p = psa*sa 
  
  #solar radiation
  eb_solar_radiation = thermal_abs*A_p*solar
  
  return(eb_solar_radiation)
}

# Heat loss because of evaporation ?


