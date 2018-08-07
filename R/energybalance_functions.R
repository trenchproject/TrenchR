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
#' @details This function allows you to calculate convection as in Mitchell (1976). Includes an enhancement factor associated with outdoor environments.
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
#' @details This function allows you estimate the heat transfer coefficient for a lizard (Based on Porter et al. 1973)
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
#' @details This function allows you estimate the heat transfer coefficient for various taxa (based on Mitchell 1976).  Approximates forced convective heat transfer for animal shapes using the convective relationship for a sphere.
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
#' @details This function allows you to estimate solar and thermal radiation (W) absorbed by the surface of an animal. 
#'          Follows Gates' Biophysical Ecology and Spotila et al. 1992.
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
#' @details This function allows you to estimate thermal radiation (W) emitted by the surface of an animal. Follows Gates' Biophysical Ecology and Spotila et al. 1992.
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
#' @details This function allows you to estimate heat loss associated with evaporative water loss by an amphibian (Spotila et al. 1992) or lizard (based on empirical measurements in Porter et a. 1973).
#' @param emissivity longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param As surface area  in m^2
#' @param Tb body temperatue in K
#' @param taxa taxa current choices: lizard, amphibian_wetskin (fully wet skin), amphibian (not fully wet skin)
#' @param rho_s saturation water vapor density at skin surface (kg/m^3) (needed if amphibian)
#' @param rho_a saturation water vapor density in ambient air (kg/m^3) (needed if amphibian)
#' @param RH relative humidity (0-1) (needed if amphibian)
#' @param h_C convective heat transfer coefficient (W m^-2 C^-1) (needed if amphibian)
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
  #TODO FIX UNITS.
  
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

#' Approximate saturation water vapor pressure
#'
#' 
#' @details Approximate saturation water vapor pressure as a function of ambient temperature for temperatures from 0 to 40C (Rosenberg 1974 in Spotila et al. 1992, see also NichMapR WETAIR and DRYAIR functions from Tracy et al. 1980).
#' @param Ta air temperature (C)
#' @return Saturation water vapor pressure (Pa)
#' @keywords Saturation water vapor pressure
#' @export
#' @examples
#' \dontrun{
#' saturation_water_vapor_pressure(Ta=20)
#' }
#' 

saturation_water_vapor_pressure<-function(Ta){
  e_s= 10^(0.02604*Ta+2.82488)
  
  return(e_s)
}

#' Calculate external resistance to water vapor transfer
#'
#' 
#' @details This function allows you to estimate external resistance to water vapor transfer using the Lewis rule relating heat and mass transport (Spotila et al. 1992).
#' @param h_c heat transfer (convection) coefficient (W m^-2 C^-1)
#' @param rhocp aggregate parameter (J m^-3 C^-1) that is the product of the density of air (kg m^-3)and the specific heat of air at constant pressure (J kg^-1 C^-1). Default of 12000 J m^-3 C^-1 is commonly assumed.
#' @return external resistance to water vapor transfer (s m^-1)
#' @keywords external resistance to water vapor transfer
#' @export
#' @examples
#' \dontrun{
#' external_resistance_to_water_vapor_transfer(h_c=20)
#' }
#' 

external_resistance_to_water_vapor_transfer<-function(h_c, rhocp=12000){
 
  r_e= rhocp / h_c

    return(r_e)
}

#' Calculate metabolism as a function of mass
#' 
#' 
#' 
#' 
#' @details This function allows you to estimate field metabolic rate (W) of various taxa as a function of mass(g). Does not account for temperature. Uses empirical relationships from Nagy et al. 1999.
#' @param mass Mass in grams.
#' @param taxa Which taxa. Current options: reptile, bird, mammal
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

#' Calculate actual vapor pressure from dewpoint temperature
#'
#' 
#' @details Calculate actual vapor pressure from dewpoint temperature based on Stull 2000
#' @param Tdewpoint dewpoint temperature (C)
#' @return actual vapor pressure (kPa)
#' @keywords actual vapor pressure
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' actual_vapor_pressure(Tdewpoint=20)
#' }
#' 

actual_vapor_pressure<-function(Tdewpoint){
  
  ea = ((2.71828182845904^(((1.0/273.0)-(1.0/(Tdewpoint + 273.15)))*5422.9939))*0.611)
  
  return(ea)
}

#' Calculate saturation vapor pressure
#'
#' 
#' @details Calculate saturation vapor pressure (kPa) based on the Clausius-Clapeyron equation (Stull 2000)
#' @param Ta air temperature (K)
#' @return saturation vapor pressure (kPa)
#' @keywords saturation vapor pressure
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' saturation_vapor_pressure(Ta=293)
#' }
#' 

saturation_vapor_pressure<-function(Ta){
  
  #constants
  Rv = 461.5 #J*K^-1*kg^-1, ideal gas constant for water vapor
  L = 2.5*10^6 #J per kg, latent heat of vaporization
  e_o= 0.611 #kPa
  
  es = e_o*exp((L/Rv)*((1./273.15)-(1./Ta))) 
  
  return(es)
}

#' Estimate the boundary layer resistance
#' 
#' @details This function allows you to estimate boundary layer resistance under free convection. Based on the function in Riddell et al. 2017 
#' @param Ta air temperature (K)
#' @param e_s saturation vapor pressure (kPa)
#' @param e_a actual vapor pressure (kPa)
#' @param elev elevation (m)
#' @param D characteristic dimension (e.g., body diameter) (m), ##diameter = 0.0016*log(mass) + 0.0061 for mass(g) #empirical formula for salamander diameter, Riddell et al. 2017
#' @param u is wind speed in m/s, if not provided assume free convection; if provided, use forced convection if appropriate 
#' @return boundary layer resistance (s cm^-1) 
#' @keywords boundary layer resistance
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' boundary_layer_resistance(Ta=293, e_s=2.4, e_a=2.5, elev=500, D=0.007, u=2)
#' }
#' 

boundary_layer_resistance<-function(Ta, e_s, e_a, elev, D, u=NA){
  
  #constant
  gravity = 9.8 #meters per second
  
  air_pressure = (101325.*(1.-(2.2569*10^-5)*elev)^5.2553)
  air_density = air_pressure/(287.04*Ta)
  dynamic_viscosity = (1.8325*10^-5)*((296.16+120.)/(Ta+120.))*((Ta/296.16)^1.5) #Tracy et al. 2010
  kinematic_viscosity = dynamic_viscosity/air_density
  T_surface = (Ta)*(1.+0.38*((e_s*1000.)/air_pressure)) #organism soil temperature in steady state heat balance
  T_air = (Ta)*(1.+0.38*((e_a*1000.)/air_pressure)) #air temperature in steady state heat balance
  coef_thermal_expansion = 1.0/Ta
  
  #Grashof and Nusselt numbers
  Grashof = (coef_thermal_expansion*gravity*(D^3)*(abs(T_surface-T_air)))/(kinematic_viscosity^2)
  Nusselt = 0.48*((Grashof)^0.25)
  
  thermal_conductivity = (2.4525*10^-2)+((7.038*10^-5)*(Ta-273.15))
  mixing_ratio = (0.6257*(e_a*1000))/(air_pressure-(1.006*(e_a*1000)))
 
  #free convection
  hc = (Nusselt*thermal_conductivity)/D #free convective heat transfer coefficient
  
  if(!is.na(u)){ #check if wind speed is provided
  #estimate Reynolds number- ratio of interval viscous forces
  Re= u*D / kinematic_viscosity
  
  #forced convection
  #use if Gr< 16 * Re^2
  if( Grashof <= 16*Re^2){
  hc= 0.923 * (u^0.333 * D^(-0.666))
  }
  } #end check if windspeed is provided
    
  #calculate boundary layer resistance 
  specific_heat = (1004.84+(1846.4*mixing_ratio))/(1+mixing_ratio)
  rb = 0.93*((specific_heat*air_density)/hc)/100
  
  return(rb)
}

#' Calculate humid operative temperature
#'
#' 
#' @details This function allows you to calculate humid operative temperature (adaptation of Campbell and Norman 1998).
#' @param r_i internal (skin) resistance (s cm^-1)
#' @param r_b boundary layer resistance (s cm^-1)
#' @param diameter body diameter (m), ##diameter = 0.0016*log(mass) + 0.0061 for mass(g) #empirical formula for diameter, Riddell et al. 2017
#' @param Ta ambient temperature (C)
#' @param elev elevation (m)
#' @param e_s saturation vapor pressure (kPa)
#' @param e_a actual vapor pressure (kPa)
#' @param Rabs Solar and thermal radiation absorbed (W)
#' @param emissivity of salamander skin, default emissivity=0.96 
#' @param radiative_conductance
#' 
#' @return humid operative temperature (C)
#' @keywords humid operative temperature
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' Tb_salamander_humid(r_i=4,r_b=1,diameter=0.007,T=20,elev=500,e_a=2.5,e_s=2.3,Rabs=400, emissivity=0.96)
#' }
#' 

Tb_salamander_humid<-function(r_i,r_b,diameter,Ta,elev,e_a, e_s,Rabs, emissivity=0.96){
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  vpd= e_a - e_s #vapor pressure deficit
  
  #radiative conductance function, Campbell and Norman 1998
  radiative_conductance= (4*(5.670373*10^-8)*(Ta+273.15)^3)/29.3
 
  gamma_naut = 0.000666
  a = (r_i*100.0)/41.4
  gva = (r_b*100)/41.4
  rad = (4*5670373*10^(-8)*(Ta+273.15)*3.)/29.3
  gamma = gamma_naut*((a+(1./gva))/((1./rad)+(1./gva)))
  s = ((((17.502*240.97))*0.611*exp((17.502*Ta)/(Ta+240.97)))/(240.97+Ta)^2)/(101.3*exp(-elev/8200))
  Tbh = Ta+(gamma/(gamma+s))*(((Rabs - (emissivity*sigma*((Ta+273.15)^4)))/(29.3*(radiative_conductance+gva)))-(vpd/(gamma*(101.3*exp(-elev/8200)))))
  
  return(Tbh)
}

### TODO check temperatures. Ta, Tg?
#' Estimate absorbed longwave (thermal) radiation
#'
#' 
#' @details This function allows you to estimate longwave (thermal) radiation (W) absorbed from the sky and the ground (adaptation of Campbell and Norman 1998).
#' @param Ta air temperature (C)
#' @param emissivity_ground emmisitivity (proportion) for more soil types (Campbell and Norman 1998), default value of 0.97
#' @param abs_longwave absorptance (proportion) of organism to longwave radiation (Bartlett and Gates 1967, Buckley 2008), default value of 0.965
#' 
#' @return thermal radiation absorbed
#' @keywords longwave (thermal) radiation absorbed
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' thermal_radiation_absorbed(Ta=20, emissivity_ground=0.97, abs_longwave=0.965)
#' }
#' 

thermal_radiation_absorbed<-function(Ta, emissivity_ground=0.97, abs_longwave=0.965){
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  'longwave radiation from sky function, Campbell and Norman 1998'
  Rlongwave_sky= 53.1*10^-14*(Ta+273.15)^6.
  
  'longwave radiation from ground function, Campbell and Norman 1998'
  Rlongwave_ground= emissivity_ground*sigma*(Ta+273.15)^4.
  
  'radiation absorbed function, adapted from Campbell and Norman 1998'
  Rlongwave= 0.5*abs_longwave*(Rlongwave_sky+Rlongwave_ground)
  
  return(Rlongwave)
}


#' Statistical approximation of soil temperature
#'
#' 
#' @details This function allows you to estimate soil temperature at a given depth and hour approximating diurnal variation as sinusoidal (adapted from Campbell and Norman 1998).
#' @param Ts_max daily maximum soil surface temperature (C)
#' @param Ts_min daily minimum soil surface temperature (C)
#' @param depth depth (cm) ???
#' 
#' @return soil temperature (C)
#' @keywords soil temperature
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' Tsoil(Ts_max=30, Ts_min=15, hour=12, depth=5)
#' }
#' 

Tsoil<-function(Ts_max, Ts_min, hour, depth){
 
  offset= ifelse(hour %in% c(0,1,2,3), -13, 11)
  Tsoil= ((Ts_max+Ts_min)/2.0)+((Ts_max-Ts_min)/2.0)*(2.71**(-depth/5.238))*sin((3.14/12.)*(hour-offset)-depth/5.238)
  
  return(Tsoil)
}

