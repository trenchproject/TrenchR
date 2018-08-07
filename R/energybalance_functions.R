#' Calculate conductance assuming animal thermal conductivity is rate limiting
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to calculate conductance (W) of an ectothermic animal to its substate. Method assumes the major resistance to conduction is within surface layers of the animal and that the interior of the animal is equal in temperature to its surface (thermally well mixed). Reference: Spotila et al. 1992. Biophysicas of Heat and Mass Transfer.
#' @param Tg Ground (Surface) Temperature in Kelvin.
#' @param Tb Body Temperature in Kelvin.
#' @param d Mean thickness of the animal skin (surface) in meters, assumes well mixed interior
#' @param K Thermal conductivity (W K^-1 m^-1 ), K=0.5 W K^-1 m^-1 for naked skin, K=0.15 for insect cuticle ( Galushko et al 2005); conductivity of ground is generally greater than that of animal tissues, so animal thermal conductivity is generally rate limiting step. 
#' @param A Surface area  in m^2
#' @param proportion In contact to the surface
#' @return conductance (W)
#' @keywords conductance
#' @export
#' @examples
#' \dontrun{
#' Qconduction_animal(Tg= 293,Tb=303,d=10^-6,K=0.5,A=10^-3, proportion=0.2)
#' }
#' 

Qconduction_animal<-function(Tg,Tb,d,K=0.5, A,proportion){
 
  # Calculate the area of contact
  A_contact  = A * proportion
  
  # Conduction
  # Calculating the heat loss (Difference between animal temperature and its environment)
  # m^2 * W K^-1 m^-1 * K / m
  Qcond = A_contact*K*(Tg - Tb)/(d)

  return(Qcond)
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
#' @param Tg Surface Temperature in Kelvin.
#' @param Tb Body Temperature in Kelvin.
#' @param D Characteristic dimension of the animal in meters
#' @param K_g Thermal conductivity of substrate (W K^-1 m^-1 )
#' @param A Surface area  in m^2
#' @param proportion In contact to the surface
#' @return conductance (W)
#' @keywords conductance
#' @export
#' @examples
#' \dontrun{
#' Qconduction_substrate(Tg= 293,Tb=303,D=0.01,K_g=0.3,A=10^-2, proportion=0.2)
#' }
#' 

Qconduction_substrate<-function(Tg,Tb,D,K_g=0.5, A,proportion){
  
  # Calculate the area of contact
  A_contact  = A * proportion
  
  #Thermal conductance between animal and substrate
  H_g= 2.0*K_g/D
  
  # Conduction,  m^2 * W K^-1 m^-1 * K / m
  Qcond = A_contact*H_g*(Tb - Tg)
  
  return(Qcond)
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
#' @param H_L Convective heat transfer coefficient (W m^-2 K^-1)
#' @param A Surface area  in m^2
#' @param proportion of surface area exposed to air
#' @param ef is the enhancement factor, used to adjust H_L to field condictions (using H_L approximation from Mitchell 1976).  Approximated as 1.3 by default, but see Mitchell 1976 for more relationship.
#' @return convection (W)
#' @keywords convection
#' @export
#' @examples
#' \dontrun{
#' Qconvection(Ta= 293,Tb= 303,H_L=10.45,A=0.0025, proportion=0.85)
#' }
#' 

Qconvection<-function(Ta,Tb,H_L=10.45,A,proportion, ef=1.3 ){
  
  # Calculate skin area exposed to air
  A_air = A*proportion
  
  ##TODO Will eventually want to expand to other forms of convection
  ##TODO see Mitchell. 1976. Heat transfer from spheres and other animal forms, 
  ## http://www.sciencedirect.com/science/article/pii/S0006349576857116
  #convection, assuming no wind
  # W m^-2 K^-1 * m^2 * K
  Qconv =   ef*H_L*A_air*(Ta-Tb)
  
  return(Qconv)
}

#' Calculate heat transfer coefficient for lizard
#' 
#' @details This function allows you estimate the heat transfer coefficient for a lizard (Based on Porter et al. 1973)
#' @param A_v Air velocity m/s.
#' @param orientation parallel or transverse
#' @return heat transfer coefficient, H (W m^-2 K^-1)
#' @keywords heat transfer coefficient 
#' @export
#' @examples
#' \dontrun{
#' heat_transfer_coefficient_lizard(V=3, orientation="parallel")
#' }
#' 

heat_transfer_coefficient_lizard<-function(V,orientation="parallel"){
  
  #Convert air velocity from m/s to cm/sec
  V_cm = V*100
  
  # Case when orientation is parallel
  H_L = dplyr::case_when(
    orientation == "parallel" ~ 0.0038927 + (0.0001169 *V_cm ),
    TRUE ~ 1
  )
  
  # Case when orientation is transverse
  H_L = dplyr::case_when(
    orientation == "transverse" ~ 0.012132 + (0.000116 *V_cm ),
    TRUE ~ 1
  )
  
  #TODO - Need to verify the conversion
  # Convert from cal/minute / cm^2 /Celsius to W m^-2 K^-1
  # Used 1 calorie per minute ( cal/min ) = 0.070 watts ( W )
  H_L_SI <- (H_L * 0.070 * 100 ) 
    
  return(H_L_SI)
}


#' Calculate heat transfer coefficient (based on Mitchell 1976)
#' (Uses Table 1 which is Convective Heat Trasfer Relations to Animal Shapes)
#' 
#' @details This function allows you estimate the heat transfer coefficient for various taxa (based on Mitchell 1976).  Approximates forced convective heat transfer for animal shapes using the convective relationship for a sphere.
#' @param V Air velocity m/s.
#' @param D Characteristic dimension (e.g., diameter or snout-vent length) in meters.
#' @param K Thermal conductivity of air, W m^-1 K^-1, can calculate using DRYAIR or WETAIR in NicheMapR
#' @param nu Kinematic Viscocity of air, m^2 s^-1, can calculate using DRYAIR or WETAIR in NicheMapR
#' @param taxa Which class of organism, current choices: sphere,cylinder,frog,lizard_surface,lizard_elevated,flyinginsect,spider
#' @return heat transfer coefficient, H_L (W m^-2 K^-1)
#' @keywords heat transfer coefficient
#' @export
#' @examples
#' \dontrun{
#' heat_transfer_coefficient(V=3,D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "cylinder")
#' }
#' 

heat_transfer_coefficient<-function(V,D,K,nu, taxa="cylinder"){
  
  ##TODO Check unit: K or C
  
  taxas= c("sphere","cylinder","frog","lizard_surface","lizard_elevated","flyinginsect","spider")
  #cylinder assumes 40<Re<4000
  #lizard assumes prostrate on or elevated above surface, average for parallel and perpendicular to air flow
  
  # Dimensionless constant (Cl)
  Cls= c(0.37,0.615,0.258,1.36,1.91,0.0749,0.47)
  ns= c(0.6,0.466,0.667,0.39,0.45,0.78,0.5) 
  
  #find index  
  ind= match(taxa, taxas)
  
  H_L_SI <- Cls[ind] *K * ((V * D/ nu )^ns[ind]) /D
  
  return(H_L_SI)
}

#' Calculate absorbed solar and thermal radiation
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to estimate solar and thermal radiation (W) absorbed by the surface of an animal. 
#'          Follows Gates' Biophysical Ecology and Spotila et al. 1992.
#' @param a solar absorptivity of animal surface (proportion), default value is for reptiles
#' @param A surface area  in m^2
#' @param psa_dir proportion surface area exposed to solar radiation
#' @param psa_ref proportion surface area exposed to reflected solar radiation
#' @param S_dir direct solar radiation (W/m^2)
#' @param S_dif diffuse solar radiation (W/m^2)
#' @param S_ref reflected solar radiation (W/m^2), either provided or estimated if surface albedo is provided instead
#' @param a_s is surface albedo (proportion), optional (not used) if reflected radiation is provided
#' @return solar radiation absorbed (W)
#' @keywords Solar radiation absorbed
#' @export
#' @examples
#' \dontrun{
#' Qradiation_absorbed(a=0.9, A=1, psa_dir=0.4, psa_ref=0.4, S_dir=1000, S_dif=200, a_s=0.5)
#' }
#' 

Qradiation_absorbed<-function(a=0.9, A, psa_dir, psa_ref, S_dir, S_dif, S_ref=NA, a_s=NA){
  
 ## TODO: Add function with absorbances for other taxa, Many values in Gates Biophysical ecology Table 8.2
 
#Calculate A_ref if not provided
  if( is.na(S_ref)) S_ref= a_s*S_dir
  
 #Areas
  A_dir = A*psa_dir
  A_dif= A_dir
  A_ref = A*psa_ref
  
  #solar radiation
  Qabs= a*A_dir*S_dir + a*A_dif*S_dif + a*A_ref*S_ref
  
  return(Qabs)
}

#' Calculate emitted thermal radiation
#' 
#' 
#' @details This function allows you to estimate thermal radiation (W) emitted by the surface of an animal. Follows Gates' Biophysical Ecology and Spotila et al. 1992.
#' @param epsilon longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param A surface area  in m^2
#' @param psa_dir proportion surface area exposed to sky (or enclosure)
#' @param psa_ref proportion surface area exposed to ground
#' @param Tb body surface temperatue in K
#' @param Tg ground surface temperatue in K
#' @param Ta ambient air temperature in K, only required if animal is in enclosed environment
#' @param enclosed TRUE or FALSE
#' @return emitted thermal radiation, Qemit (W)
#' @keywords emitted thermal radiation
#' @export
#' @examples
#' \dontrun{
#' Qemitted_thermal_radiation(epsilon=0.96, A=1, psa_dir=0.4, psa_ref=0.6, Tb=303, Tg=293, Ta=298, enclosed=FALSE)
#' }
#' 

Qemitted_thermal_radiation<-function(epsilon=0.96, A, psa_dir, psa_ref, Tb, Tg, Ta, enclosed=FALSE){
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #Areas
  A_s = A*psa_dir
  A_r = A*psa_ref
  
  #estimate effective radiant temperature of sky
  #Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
  Tsky= (1.22*(Ta-273.15) -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation
  
  if(enclosed) 
         Qemit= A_r*epsilon*sigma*(Tb^4 - Ta^4)
  else 
          Qemit= epsilon*sigma*(A_s*(Tb^4 - Tsky^4)+A_r*(Tb^4 - Tg^4))

  return(Qemit)
}


#' Calculate heat loss associated with evaporative water loss
#' 
#' 
#' 
#' 
#' 
#' @details This function allows you to estimate heat loss associated with evaporative water loss by an amphibian (Spotila et al. 1992) or lizard (based on empirical measurements in Porter et a. 1973).
#' @param epsilon longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param A surface area  in m^2
#' @param Tb body temperatue in K
#' @param taxa taxa current choices: lizard, amphibian_wetskin (fully wet skin), amphibian (not fully wet skin)
#' @param rho_s saturation water vapor density at skin surface (kg/m^3) (needed if amphibian)
#' @param rho_a saturation water vapor density in ambient air (kg/m^3) (needed if amphibian)
#' @param h relative humidity (0-1) (needed if amphibian)
#' @param H_L convective heat transfer coefficient (W m^-2 C^-1) (needed if amphibian)
#' @param r_i internal (cutaneous) resistance to vapor transport (s/m) (needed if amphibian)
#' @return evaporative heat loss (W)
#' @keywords evaporative heat loss
#' @export
#' @examples
#' \dontrun{
#' Qevaporation(A=0.1, Tb=293, taxa="amphibian", rho_s=1.2, rho_a=1.2, h=0.5, H_L=20, r_i=5000)
#' Qevaporation(A=0.1, Tb=293, taxa="lizard")
#' }
#' 

Qevaporation<-function(A, Tb, taxa, rho_s=NA, rho_a=NA, h=NA, H_L=NA, r_i=NA){
  #TODO FIX UNITS.
  
  #Porter et al. 1973 in Gates Biophysical ecology
  if(taxa=="lizard"){ 
    if(Tb<293) E_kg= 0.27
    if(Tb>=293 & Tb<=309) E_kg= 0.08*exp(0.586)*(Tb-273.5)
    if(Tb>309) E_kg= 2.97*10^(-3)*exp(0.1516)*(Tb-273.5) 
    
    #convert from W/kg to W/m2
    E= E_kg*0.067/0.018 #for 0.067kg lizard with 0.018m^2 surface area
    
    #multiply by surface area
    Qevap= E*A
    }

  #Spotila et al. 1992
  evap_heat= 2.44*10^(-6) #J/kg at most temperatures
  
  rhocp= 1200 #J*m^(-3)*C^(-1)  
  #  external (convective) resistance to water vapor transport (s/m)
  r_e= 0.93*rhocp/H_L
  
  if(taxa=="amphibian_wetskin"){ 
  #Ec= rate of water transport (kg/s)
  Ec= A *(1/r_e)*(rho_s-h*rho_a)
  #to W
  Qevap= Ec*evap_heat
  }
  
  if(taxa=="amphibian"){ 
  Ec= A * (rho_s-h*rho_a)/(r_i+r_e)
  #to W
  Qevap= Ec*evap_heat
  }  
    
  return(Qevap)
}

#' Approximate saturation water vapor pressure
#'
#' 
#' @details Approximate saturation water vapor pressure as a function of ambient temperature for temperatures from 0 to 40C (Rosenberg 1974 in Spotila et al. 1992, see also NichMapR WETAIR and DRYAIR functions from Tracy et al. 1980).
#' @param Ta air temperature (C)
#' @return Saturation water vapor pressure, e_s (Pa)
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
#' @param H_L heat transfer (convection) coefficient (W m^-2 C^-1)
#' @param rhocp aggregate parameter (J m^-3 C^-1) that is the product of the density of air (kg m^-3)and the specific heat of air at constant pressure (J kg^-1 C^-1). Default of 12000 J m^-3 C^-1 is commonly assumed.
#' @return external resistance to water vapor transfer (s m^-1)
#' @keywords external resistance to water vapor transfer
#' @export
#' @examples
#' \dontrun{
#' external_resistance_to_water_vapor_transfer(H_L=20)
#' }
#' 

external_resistance_to_water_vapor_transfer<-function(H_L, rhocp=12000){
 
  r_e= rhocp / H_L

    return(r_e)
}

#' Calculate metabolism as a function of mass
#' 
#' 
#' 
#' 
#' @details This function allows you to estimate field metabolic rate (W) of various taxa as a function of mass(g). Does not account for temperature. Uses empirical relationships from Nagy et al. 1999.
#' @param m Mass in grams.
#' @param taxa Which taxa. Current options: reptile, bird, mammal
#' @return metabolim (W)
#' @keywords metabolism
#' @export
#' @examples
#' \dontrun{
#' Qmetabolism_from_mass(m=12,"reptile")
#' }
#' 

Qmetabolism_from_mass<-function(m, taxa="reptile"){
  
  #Nagy 2005, JEB
  #FMR in W, M is mass in grams
  #Convert 1 kJ/day=0.0115741
  ##TODO check conversion
  
  #reptiles
  if(taxa == "reptile") Qmet = 0.196*m^0.889 * 0.0115741
  
  #mammals
  if(taxa == "mammal") Qmet = 4.82*m^0.734 * 0.0115741
  
  #birds
  if(taxa == "bird") Qmet = 10.5 * m^0.681 * 0.0115741
  
 ##TODO ?? Can further divide equations above using Nagy et al. 1999 ENERGETICS OF FREE-RANGING MAMMALS, REPTILES, AND BIRDS. Needed?
  
  return(Qmet)
}

#' Calculate basal (or resting) metabolism as a function of mass and body temperature.
#' 
#' 
#' 
#' 
#' @details This function allows you to estimate basal (or resting) metabolic rate (W) as a function of mass (g) and temperature (K). Based on empirical data and the metabolic theory of ecology (3/4 scaling exponent). From Gilooly et al. 2001.
#' @param m Mass in grams.
#' @param Tb body temperature in K
#' @param taxa Which taxa. options: bird, mammal, reptile, amphibian, invertebrate.
#' @return metabolim (W)
#' @keywords metabolism
#' @export
#' @examples
#' \dontrun{
#' Qmetabolism_from_mass_temp(m=100, Tb=303,"reptile")
#' }
#' 

Qmetabolism_from_mass_temp<-function(m,Tb, taxa){
  
  #From  Gilloolly et al. 2001 
  if(taxa=="bird" | taxa=="mammal") Qmet= exp(-9100/Tb+29.49)*m^0.75/60
  if(taxa=="reptile") Qmet= exp(-8700/Tb+26.85)*m^0.75/60
  if(taxa=="amphibian") Qmet= exp(-5760/Tb+16.68)*m^0.75/60
  if(taxa=="invertebrate") Qmet= exp(-9150/Tb+27.62)*m^0.75/60
  return(Qmet)
}

#' Calculate actual vapor pressure from dewpoint temperature
#'
#' 
#' @details Calculate actual vapor pressure from dewpoint temperature based on Stull 2000
#' @param Tdewpoint dewpoint temperature (C)
#' @return actual vapor pressure, e_a (kPa)
#' @keywords actual vapor pressure
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' actual_vapor_pressure(Tdewpoint=20)
#' }
#' 

actual_vapor_pressure<-function(Tdewpoint){
  
  e_a = ((2.71828182845904^(((1.0/273.0)-(1.0/(Tdewpoint + 273.15)))*5422.9939))*0.611)
  
  return(e_a)
}

#' Calculate saturation vapor pressure
#'
#' 
#' @details Calculate saturation vapor pressure (kPa) based on the Clausius-Clapeyron equation (Stull 2000)
#' @param Ta air temperature (K)
#' @return saturation vapor pressure, e_s (kPa)
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
  
  e_s = e_o*exp((L/Rv)*((1./273.15)-(1./Ta))) 
  
  return(e_s)
}

#' Estimate the boundary layer resistance
#' 
#' @details This function allows you to estimate boundary layer resistance under free convection. Based on the function in Riddell et al. 2017 
#' @param Ta air temperature (K)
#' @param e_s saturation vapor pressure (kPa)
#' @param e_a actual vapor pressure (kPa)
#' @param elev elevation (m)
#' @param D characteristic dimension (e.g., body diameter) (m), ##diameter = 0.0016*log(mass) + 0.0061 for mass(g) #empirical formula for salamander diameter, Riddell et al. 2017
#' @param V is wind speed in m/s, if not provided assume free convection; if provided, use forced convection if appropriate 
#' @return boundary layer resistance (s cm^-1) 
#' @keywords boundary layer resistance
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' boundary_layer_resistance(Ta=293, e_s=2.4, e_a=2.5, elev=500, D=0.007, V=2)
#' }
#' 

boundary_layer_resistance<-function(Ta, e_s, e_a, elev, D, V=NA){
  
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
  
  if(!is.na(V)){ #check if wind speed is provided
  #estimate Reynolds number- ratio of interval viscous forces
  Re= V*D / kinematic_viscosity
  
  #forced convection
  #use if Gr< 16 * Re^2
  if( Grashof <= 16*Re^2){
  hc= 0.923 * (V^0.333 * D^(-0.666))
  }
  } #end check if windspeed is provided
    
  #calculate boundary layer resistance 
  specific_heat = (1004.84+(1846.4*mixing_ratio))/(1+mixing_ratio)
  r_b = 0.93*((specific_heat*air_density)/hc)/100
  
  return(r_b)
}

#' Calculate humid operative temperature
#'
#' 
#' @details This function allows you to calculate humid operative temperature (adaptation of Campbell and Norman 1998).
#' @param r_i internal (skin) resistance (s cm^-1)
#' @param r_b boundary layer resistance (s cm^-1)
#' @param D body diameter (m), ##diameter = 0.0016*log(mass) + 0.0061 for mass(g) #empirical formula for diameter, Riddell et al. 2017
#' @param Ta ambient temperature (C)
#' @param elev elevation (m)
#' @param e_s saturation vapor pressure (kPa)
#' @param e_a actual vapor pressure (kPa)
#' @param Qabs Solar and thermal radiation absorbed (W)
#' @param epsilon emissivity of salamander skin, default epsilon=0.96 
#' @return humid operative temperature (C)
#' @keywords humid operative temperature
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' Tb_salamander_humid(r_i=4,r_b=1,D=0.007,Ta=20,elev=500,e_a=2.5,e_s=2.3,Qabs=400, epsilon=0.96)
#' }
#' 

Tb_salamander_humid<-function(r_i,r_b,D,Ta,elev,e_a, e_s,Qabs, epsilon=0.96){
  
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
  Tbh = Ta+(gamma/(gamma+s))*(((Qabs - (epsilon*sigma*((Ta+273.15)^4)))/(29.3*(radiative_conductance+gva)))-(vpd/(gamma*(101.3*exp(-elev/8200)))))
  
  return(Tbh)
}

### TODO check temperatures. Ta, Tg?
#' Estimate absorbed longwave (thermal) radiation
#'
#' 
#' @details This function allows you to estimate longwave (thermal) radiation (W) absorbed from the sky and the ground (adaptation of Campbell and Norman 1998).
#' @param Ta air temperature (C)
#' @param epsilon_ground emmisitivity (proportion) for more soil types (Campbell and Norman 1998), default value of 0.97
#' @param a_longwave absorptance (proportion) of organism to longwave radiation (Bartlett and Gates 1967, Buckley 2008), default value of 0.965
#' 
#' @return thermal radiation absorbed
#' @keywords longwave (thermal) radiation absorbed
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' Qthermal_radiation_absorbed(Ta=20, epsilon_ground=0.97, a_longwave=0.965)
#' }
#' 

Qthermal_radiation_absorbed<-function(Ta, epsilon_ground=0.97, a_longwave=0.965){
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  'longwave radiation from sky function, Campbell and Norman 1998'
  Slongwave_sky= 53.1*10^-14*(Ta+273.15)^6.
  
  'longwave radiation from ground function, Campbell and Norman 1998'
  Slongwave_ground= epsilon_ground*sigma*(Ta+273.15)^4.
  
  'radiation absorbed function, adapted from Campbell and Norman 1998'
  Slongwave= 0.5*a_longwave*(Slongwave_sky+Slongwave_ground)
  
  return(Slongwave)
}


#' Statistical approximation of soil temperature
#'
#' 
#' @details This function allows you to estimate soil temperature at a given depth and hour approximating diurnal variation as sinusoidal (adapted from Campbell and Norman 1998).
#' @param Tg_max daily maximum soil surface temperature (C)
#' @param Tg_min daily minimum soil surface temperature (C)
#' @param depth depth (cm) ???
#' 
#' @return soil temperature (C)
#' @keywords soil temperature
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' Tsoil(Tg_max=30, Tg_min=15, hour=12, depth=5)
#' }
#' 

Tsoil<-function(Tg_max, Tg_min, hour, depth){
 
  offset= ifelse(hour %in% c(0,1,2,3), -13, 11)
  Tsoil= ((Tg_max+Tg_min)/2.0)+((Tg_max-Tg_min)/2.0)*(2.71**(-depth/5.238))*sin((3.14/12.)*(hour-offset)-depth/5.238)
  
  return(Tsoil)
}

