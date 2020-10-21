#' Calculate conductance assuming animal thermal conductivity is rate limiting
#' 
#' @details This function allows you to calculate conductance (W) of an ectothermic animal to its substate. Method assumes the major resistance to conduction is within surface layers of the animal and that the interior of the animal is equal in temperature to its surface (thermally well mixed). Reference: Spotila et al. 1992. Biophysics of Heat and Mass Transfer. In Feder and Burggren. Environmental Physiology of the Amphibians.
#' @param T_g Ground (Surface) Temperature in Kelvin.
#' @param T_b Body Temperature in Kelvin.
#' @param d Mean thickness of the animal skin (surface) in (m), assumes well mixed interior
#' @param K Thermal conductivity (W K^-1 m^-1), K=0.5 W K^-1 m^-1 for naked skin, K=0.15 for insect cuticle (Galushko et al 2005); conductivity of ground is generally greater than that of animal tissues, so animal thermal conductivity is generally rate limiting step. 
#' @param A Surface area in m^2
#' @param proportion in contact with the surface
#' @return conductance (W)
#' @keywords conductance
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Qconduction_animal(T_g= 293,T_b=303,d=10^-6,K=0.5,A=10^-3, proportion=0.2)
#' }
#' 

Qconduction_animal<-function(T_g, T_b, d ,K=0.5, A, proportion){
 
  stopifnot(T_g>200, T_g<400, T_b>200, T_b<400, d>=0, K>0, A>0, proportion>=0, proportion<=1)
  
  # Calculate the area of contact
  A_contact  = A * proportion
  
  # Conduction
  # Calculating the heat loss (Difference between animal temperature and its environment)
  # m^2 * W K^-1 m^-1 * K / m
  Qcond = A_contact*K*(T_b - T_g)/(d)

  return(Qcond)
}

#' Calculate conductance assuming substrate thermal conductivity is rate limiting
#' 
#' @details This function allows you to calculate conductance (W) of an ectothermic animal to its substate.
#'          Method assumes the major resistance to conduction is the substrate and that the interior of the 
#'          animal is equal in temperature to its surface (thermally well mixed). 
#'          Reference: Spotila et al. 1992. Biophysics of Heat and Mass Transfer. In Feder and Burggren. Environmental Physiology of the Amphibians.
#' @param T_g Surface Temperature in Kelvin.
#' @param T_b Body Temperature in Kelvin.
#' @param D Characteristic dimension of the animal in meters
#' @param K_g Thermal conductivity of substrate (W K^-1 m^-1)
#' @param A Surface area in m^2
#' @param proportion In contact to the surface
#' @return conductance (W)
#' @keywords conductance
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Qconduction_substrate(T_g= 293, T_b=303, D=0.01, K_g=0.3, A=10^-2, proportion=0.2)
#' }
#' 

Qconduction_substrate<-function(T_g, T_b, D, K_g=0.5, A, proportion){
  
  stopifnot(T_g>200, T_g<400, T_b>200, T_b<400, D>=0, K_g>0, A>0, proportion>=0, proportion<=1)
  
  # Calculate the area of contact
  A_contact  = A * proportion
  
  #Thermal conductance between animal and substrate
  H_g= 2.0*K_g/D
  
  # Conduction,  m^2 * W K^-1 m^-1 * K / m
  Qcond = A_contact*H_g*(T_b - T_g)
  
  return(Qcond)
}

#' Calculate convection
#' 
#' @details This function allows you to calculate convection from an organism to its environment as in Mitchell (1976). Includes an enhancement factor associated with outdoor environments. Reference: Mitchell. 1976. Heat transfer from spheres and other animal forms. Biophysical Journal 16(6): 561–569.
#' @param T_a Air Temperature in Kelvin.
#' @param T_b Initial Body Temperature in Kelvin.
#' @param H_L Convective heat transfer coefficient (W m^-2 K^-1)
#' @param A Surface area in m^2
#' @param proportion of surface area exposed to air
#' @param ef is the enhancement factor, used to adjust H to field conditions.  Approximated as mean value of 1.23 by default, but see Mitchell 1976 for further information.
#' @return convection (W)
#' @keywords convection
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Qconvection(T_a= 293, T_b= 303, H_L=10.45, A=0.0025, proportion=0.85)
#' }
#' 

Qconvection<-function(T_a, T_b, H_L=10.45, A, proportion, ef=1.23){
  
  stopifnot(T_a>200, T_a<400, T_b>200, T_b<400, H_L>0, A>0, proportion>=0, proportion<=1, ef>=1)
  
  # Calculate skin area exposed to air
  A_air = A*proportion

  Qconv = ef*H_L*A_air*(T_b-T_a)
  
  return(Qconv)
}

#' Calculate heat transfer coefficient. 
#' 
#' @details This function allows you to estimate the heat transfer coefficient for various taxa based on empirical measurements. Reference: Mitchell. 1976. Heat transfer from spheres and other animal forms. Biophysical Journal 16(6): 561–569. (Uses Table I: Convective Heat Transfer Relations for Animal Shapes)
#' @param V Air velocity m/s.
#' @param D Characteristic dimension (e.g., diameter or snout-vent length) in meters.
#' @param K Thermal conductivity of air, W m^-1 K^-1, can calculate using DRYAIR or WETAIR in NicheMapR
#' @param nu Kinematic Viscocity of air, m^2 s^-1, can calculate using DRYAIR or WETAIR in NicheMapR
#' @param taxa Which class of organism, current choices: sphere, cylinder, frog, lizard_surface, lizard_elevated, flyinginsect, spider
#' @return heat transfer coefficient, H_L (W m^-2 K^-1)
#' @keywords heat transfer coefficient
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' heat_transfer_coefficient(V=0.5,D=0.05,K= 25.7 * 10^(-3), nu= 15.3 * 10^(-6), "cylinder")
#' }
#' 

heat_transfer_coefficient<-function(V, D, K, nu, taxa="cylinder"){
  
  stopifnot(V>=0, D>=0, K>=0, nu>=0, taxa %in% c("sphere","cylinder","frog","lizard_surface","lizard_elevated","flyinginsect","spider"))
  
  taxas= c("sphere","cylinder","frog","lizard_surface","lizard_elevated","flyinginsect","spider")
  #cylinder assumes 40<Re<4000
  #lizard assumes prostrate on or elevated above surface, average for parallel and perpendicular to air flow
  
  # Dimensionless constant (Cl)
  Cls= c(0.37,0.615,0.258,1.36,1.91,0.0749,0.47)
  ns= c(0.6,0.466,0.667,0.39,0.45,0.78,0.5) 
  
  #find index  
  ind= match(taxa, taxas)
  
  Re= V*D/nu #Reynolds number 
  Nu <- Cls[ind] * Re^ns[ind]  #Nusselt number
  H_L= Nu * K / D
   
  return(H_L)

}

#' Calculate heat transfer coefficient using a sphereical approximation (based on Mitchell 1976)
#' 
#' @details This function allows you to estimate the heat transfer coefficient for various taxa.  Approximates forced convective heat transfer for animal shapes using the convective relationship for a sphere. Reference: Mitchell. 1976. Heat transfer from spheres and other animal forms. Biophysical Journal 16(6): 561–569. (Uses Table III: Convective Heat Transfer Relations for Animal Shapes.)
#' @param V Air velocity m/s.
#' @param D Characteristic dimension (e.g., diameter or snout-vent length) in meters.
#' @param K Thermal conductivity of air, W m^-1 K^-1, can calculate using DRYAIR or WETAIR in NicheMapR
#' @param nu Kinematic Viscocity of air, m^2 s^-1, can calculate using DRYAIR or WETAIR in NicheMapR
#' @param taxa Which class of organism, current choices: sphere,frog,lizard,flyinginsect,spider
#' @return heat transfer coefficient, H_L (W m^-2 K^-1)
#' @keywords heat transfer coefficient
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' heat_transfer_coefficient_approximation(V=3,D=0.05,K= 25.7 * 10^(-3),nu= 15.3 * 10^(-6), "sphere")
#' }
#' 

heat_transfer_coefficient_approximation<-function(V, D, K, nu, taxa="sphere"){
  
  stopifnot(V>=0, D>=0, K>=0, nu>=0, taxa %in% c("sphere","frog","lizard","flyinginsect","spider"))
  
  taxas= c("sphere","frog","lizard","flyinginsect","spider")
 
  # Dimensionless constant (Cl)
  Cls= c(0.34,0.196,0.56,0.0714,0.52)
  ns= c(0.6,0.667,0.6, 0.78,0.5) 
  
  #find index  
  ind= match(taxa, taxas)
  
  Re= V*D/nu #Reynolds number 
  Nu <- Cls[ind] * Re^ns[ind]  #Nusselt number
  H_L= Nu * K / D
  
  return(H_L)
}

#' Calculate heat transfer coefficient (based on Mitchell 1976 in Spotila 1992)
#' 
#' @details This function allows you to estimate the heat transfer coefficient for various taxa (based on Mitchell 1976 and using relationship in Spotila et al 1992). References: Spotila et al. 1992. Biophysics of Heat and Mass Transfer. In Feder and Burggren. Environmental Physiology of the Amphibians; Mitchell. 1976. Heat transfer from spheres and other animal forms. Biophysical Journal 16(6): 561–569.
#' @param V Air velocity m/s.
#' @param D Characteristic dimension (e.g., diameter or snout-vent length) in meters.
#' @return heat transfer coefficient, H_L (W m^-2 K^-1)
#' @keywords heat transfer coefficient
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' heat_transfer_coefficient_simple(V=0.5,D=0.05)
#' }
#' 

heat_transfer_coefficient_simple<-function(V,D){
  
  stopifnot(V>=0, D>=0)
  
  H_L= 6.77 * V^0.6 * D^(-0.4)
  
  return(H_L)
}

#' Calculate absorbed solar and thermal radiation
#' 
#' @details This function allows you to estimate solar and thermal radiation (W) absorbed by the surface of an animal. 
#'          Follows Gates' Biophysical Ecology and Spotila et al. 1992. Reference: Spotila et al. 1992. Biophysics of Heat and Mass Transfer. In Feder and Burggren. Environmental Physiology of the Amphibians.
#' @param a solar absorptivity of animal surface (proportion), default value is for reptiles
#' @param A surface area in m^2
#' @param psa_dir proportion surface area exposed to solar radiation
#' @param psa_ref proportion surface area exposed to reflected solar radiation
#' @param S_dir direct solar radiation (W/m^2)
#' @param S_dif diffuse solar radiation (W/m^2)
#' @param S_ref reflected solar radiation (W/m^2), either provided or estimated if surface albedo is provided instead
#' @param a_s is surface albedo (proportion), optional (not used) if reflected radiation is provided, Values available in Gates Biophysical ecology Table 8.2.
#' @return solar radiation absorbed (W)
#' @keywords Solar radiation absorbed
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Qradiation_absorbed(a=0.9, A=1, psa_dir=0.4, psa_ref=0.4, S_dir=1000, S_dif=200, a_s=0.5)
#' }
#' 

Qradiation_absorbed<-function(a=0.9, A, psa_dir, psa_ref, S_dir, S_dif, S_ref=NA, a_s=NA){

stopifnot(a>=0, a<=1, A>0, psa_dir>=0, psa_dir<=1, psa_ref>=0, psa_ref<=1,S_dir>0, S_dif>0)  
  
#Calculate S_ref if not provided
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
#' @details This function allows you to estimate thermal radiation (W) emitted by the surface of an animal. Follows Gates' Biophysical Ecology and Spotila et al. 1992. Reference: Spotila et al. 1992. Biophysics of Heat and Mass Transfer. In Feder and Burggren. Environmental Physiology of the Amphibians; Porter et al. 1973. Behavioral implications of mechanistic ecology. Oecologia 13:1-54.
#' @param epsilon longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param A surface area  in m^2
#' @param psa_dir proportion surface area exposed to sky (or enclosure)
#' @param psa_ref proportion surface area exposed to ground
#' @param T_b body surface temperature in K
#' @param T_g ground surface temperature in K
#' @param T_a ambient air temperature in K, only required if animal is in enclosed environment
#' @param enclosed TRUE or FALSE
#' @return emitted thermal radiation, Qemit (W)
#' @keywords emitted thermal radiation
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Qemitted_thermal_radiation(
#'   epsilon=0.96,
#'   A=1, 
#'   psa_dir=0.4, 
#'   psa_ref=0.6, 
#'   T_b=303, 
#'   T_g=293, 
#'   T_a=298, 
#'   enclosed=FALSE)
#' }
#' 

Qemitted_thermal_radiation<-function(epsilon=0.96, A, psa_dir, psa_ref, T_b, T_g, T_a, enclosed=FALSE){
  
  stopifnot(epsilon>=0, epsilon<=1, A>0, psa_dir>=0, psa_dir<=1, psa_ref>=0, psa_ref<=1, T_b>200, T_b<400, T_g>200, T_g<400, T_a>200, T_a<400, enclosed %in% c(TRUE, FALSE))
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #Areas
  A_s = A*psa_dir
  A_r = A*psa_ref
  
  #estimate effective radiant temperature of sky
  #Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
  Tsky= (1.22*(T_a-273.15) -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation
  
  if(enclosed) 
         Qemit= A_r*epsilon*sigma*(T_b^4 - T_a^4)
  else 
          Qemit= epsilon*sigma*(A_s*(T_b^4 - Tsky^4)+A_r*(T_b^4 - T_g^4))

  return(Qemit)
}

#' Calculate heat loss associated with evaporative water loss
#' 
#' @details This function allows you to estimate heat loss associated with evaporative water loss by an amphibian (Spotila et al. 1992) or lizard (based on empirical measurements in Porter et al. 1973). Reference: Spotila et al. 1992. Biophysics of Heat and Mass Transfer. In Feder and Burggren. Environmental Physiology of the Amphibians.
#' @param A surface area  in m^2
#' @param T_b body temperatue in K
#' @param taxa taxa current choices: lizard, amphibian_wetskin (fully wet skin), amphibian (not fully wet skin)
#' @param rho_s saturation water vapor density at skin surface (kg/m^3) (needed if amphibian)
#' @param rho_a saturation water vapor density in ambient air (kg/m^3) (needed if amphibian)
#' @param h relative humidity (0-1) (needed if amphibian)
#' @param H convective heat transfer coefficient (W m^-2 K^-1) (needed if amphibian)
#' @param r_i internal (cutaneous) resistance to vapor transport (s/m) (needed if amphibian)
#' @return evaporative heat loss (W)
#' @keywords evaporative heat loss
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Qevaporation(A=0.1, T_b=293, taxa="amphibian", rho_s=0.003, rho_a=0.002, h=0.5, H=20, r_i=50)
#' Qevaporation(A=0.1, T_b=293, taxa="lizard")
#' }
#' 

Qevaporation<-function(A, T_b, taxa, rho_s=NA, rho_a=NA, h=NA, H=NA, r_i=NA){
  
  stopifnot(A>0, T_b>200, T_b<400, taxa %in% c("lizard", "amphibian_wetskin", "amphibian"))

  if(taxa %in% c("amphibian_wetskin", "amphibian")) stopifnot(rho_s>0, rho_a>0, h>=0, h<=1, H>0, r_i>0)                                                              
                                                              
  #Porter et al. 1973 in Gates Biophysical ecology
  if(taxa=="lizard"){ 
    if(T_b<293) E_kg= 0.27
    if(T_b>=293 & T_b<=309) E_kg= 0.08*exp(0.586)*(T_b-273.5)
    if(T_b>309) E_kg= 2.97*10^(-3)*exp(0.1516)*(T_b-273.5) 
    
    #convert from W/kg to W/m2
    E= E_kg*0.067/0.018 #for 0.067kg lizard with 0.018m^2 surface area
    
    #multiply by surface area
    Qevap= E*A
    }

  #Spotila et al. 1992
  evap_heat= 2.44*10^(6) #J/kg at most temperatures
  
  rhocp= 1200 #J*m^(-3)*K^(-1)  
  #  external (convective) resistance to water vapor transport (s/m), Lewis rule
  r_e= 0.93*rhocp/H
  
  if(taxa=="amphibian_wetskin"){ 
  #Ec= rate of water transport (kg/s)
  Ec= A *(rho_s-h*rho_a)/r_e
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
#' @details Approximate saturation water vapor pressure as a function of ambient temperature for temperatures from 0 to 40°C (Rosenberg 1974 in Spotila et al. 1992, see also NichMapR WETAIR and DRYAIR functions from Tracy et al. 1980).  Reference: Spotila et al. 1992. Biophysics of Heat and Mass Transfer. In Feder and Burggren. Environmental Physiology of the Amphibians.
#' @param T_a air temperature (°C)
#' @return Saturation water vapor pressure, e_s (Pa)
#' @keywords Saturation water vapor pressure
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' saturation_water_vapor_pressure(T_a=20)
#' }
#' 

saturation_water_vapor_pressure<-function(T_a){
  e_s= 10^(0.02604*T_a+2.82488)
  
  return(e_s)
}

#' Calculate external resistance to water vapor transfer
#'
#' 
#' @details This function allows you to estimate external resistance to water vapor transfer using the Lewis rule relating heat and mass transport. Reference: Spotila et al. 1992. Biophysics of Heat and Mass Transfer. In Feder and Burggren. Environmental Physiology of the Amphibians.
#' @param H heat transfer (convection) coefficient (W m^-2 °C^-1)
#' @param rhocp aggregate parameter (J m^-3 °C^-1) that is the product of the density of air (kg m^-3) and the specific heat of air at constant pressure (J kg^-1 °C^-1). Default of 12000 J m^-3 °C^-1 is commonly assumed.
#' @return external resistance to water vapor transfer (s m^-1)
#' @keywords external resistance to water vapor transfer
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' external_resistance_to_water_vapor_transfer(H=20)
#' }
#' 

external_resistance_to_water_vapor_transfer<-function(H, rhocp=12000){
 
  stopifnot(H>0)
  
  r_e= 0.93 * rhocp / H

    return(r_e)
}

#' Calculate metabolism as a function of mass
#' 
#' @details This function allows you to estimate field metabolic rate (W) of various taxa as a function of mass(g). Does not account for temperature. Uses empirical relationships from Nagy KA. 2005. Field metabolic rate and body size. Journal of Experimental Biology 208: 1621-1625.
#' @param m Mass in grams.
#' @param taxa Which taxa. Current options: reptile, bird, mammal
#' @return metabolism (W)
#' @keywords metabolism
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Qmetabolism_from_mass(m=12,"reptile")
#' }
#' 

Qmetabolism_from_mass<-function(m, taxa="reptile"){
  
  stopifnot(m>0, taxa %in% c("reptile", "bird", "mammal") )
  
  #FMR in W, M is mass in grams
  #Convert 1 kJ/day=0.0115741 W
  
  #reptiles
  if(taxa == "reptile") Qmet = 0.196*m^0.889 * 0.0115741
  
  #mammals
  if(taxa == "mammal") Qmet = 4.82*m^0.734 * 0.0115741
  
  #birds
  if(taxa == "bird") Qmet = 10.5 * m^0.681 * 0.0115741
  
 #Can further divide equations above using Nagy et al. 1999 ENERGETICS OF FREE-RANGING MAMMALS, REPTILES, AND BIRDS. 
  
  return(Qmet)
}

#' Calculate basal (or resting) metabolism as a function of mass and body temperature.
#' 
#' @details This function allows you to estimate basal (or resting) metabolic rate (W) as a function of mass (g) and temperature (K). Based on empirical data and the metabolic theory of ecology (3/4 scaling exponent). Reference:  Gillooly JF et al. 2001. Effects of size and temperature on metabolic rate. Science 293: 2248-2251. 
#' @param m Mass in grams.
#' @param T_b body temperature in K
#' @param taxa Which taxa. options: bird, mammal, reptile, amphibian, invertebrate.
#' @return basal metabolim (W)
#' @keywords metabolism
#' @family biophysical models
#' @export
#' @examples 
#' \dontrun{
#' Qmetabolism_from_mass_temp(m=100, T_b=303, "reptile")
#' }
#' 

Qmetabolism_from_mass_temp<-function(m, T_b, taxa){
  
  stopifnot(m>0, T_b>200, T_b<400, taxa %in% c("bird","mammal","reptile","amphibian","invertebrate") )
  
  #Source:  Gillooly JF et al. 2001. Effects of size and temperature on metabolic rate. Science 293: 2248-2251. 
  if(taxa=="bird" | taxa=="mammal") Qmet= exp(-9100/T_b+29.49)*m^0.75/60
  if(taxa=="reptile") Qmet= exp(-8780/T_b+26.85)*m^0.75/60
  if(taxa=="amphibian") Qmet= exp(-5760/T_b+16.68)*m^0.75/60
  if(taxa=="invertebrate") Qmet= exp(-9150/T_b+27.62)*m^0.75/60
  return(Qmet)
}

#' Calculate actual vapor pressure from dewpoint temperature
#'
#' 
#' @details Calculate actual vapor pressure from dewpoint temperature based on Stull 2000. Source: Riddell EA. 2017. Physical calculations of resistance to water loss improve predictions of species range models. Ecological Monographs 87: 21-23.
#' @param Tdewpoint dewpoint temperature (°C)
#' @return actual vapor pressure, e_a (kPa)
#' @keywords actual vapor pressure
#' @family biophysical models
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
#' @details Calculate saturation vapor pressure (kPa) based on the Clausius-Clapeyron equation (Stull 2000). Source: Riddell EA. 2017. Physical calculations of resistance to water loss improve predictions of species range models. Ecological Monographs 87: 21-23.
#' @param T_a air temperature (K)
#' @return saturation vapor pressure, e_s (kPa)
#' @keywords saturation vapor pressure
#' @family biophysical models
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' saturation_vapor_pressure(T_a=293)
#' }
#' 

saturation_vapor_pressure<-function(T_a){
  
  stopifnot(T_a>200, T_a<400)
  
  #constants
  Rv = 461.5 #J*K^-1*kg^-1, ideal gas constant for water vapor
  L = 2.5*10^6 #J per kg, latent heat of vaporization
  e_o= 0.611 #kPa
  
  e_s = e_o*exp((L/Rv)*((1./273.15)-(1./T_a))) 
  
  return(e_s)
}

#' Estimate the boundary layer resistance
#' 
#' @details This function allows you to estimate boundary layer resistance under free convection. Based on the function in Riddell et al. 2017. Source: Riddell EA. 2017. Physical calculations of resistance to water loss improve predictions of species range models. Ecological Monographs 87: 21-23. 
#' @param T_a air temperature (K)
#' @param e_s saturation vapor pressure (kPa)
#' @param e_a actual vapor pressure (kPa)
#' @param elev elevation (m)
#' @param D characteristic dimension (e.g., body diameter) (m)
#' @param u is wind speed in m/s, if not provided assume free convection; if provided, use forced convection if appropriate 
#' @return boundary layer resistance (s cm^-1) 
#' @keywords boundary layer resistance
#' @family biophysical models
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' boundary_layer_resistance(T_a=293, e_s=2.5, e_a=2.0, elev=500, D=0.007, u=2)
#' }
#' 

boundary_layer_resistance<-function(T_a, e_s, e_a, elev, D, u=NA){
  
  stopifnot(T_a>200, T_a<400, e_s>0, e_a>0, elev>0, D>0)
  
  if(e_s<e_a) stop("Actual vapor pressure, e_a, must be lower that saturation vapor pressure, e_s.")
  
  #constant
  gravity = 9.8 #meters per second
  
  air_pressure = (101325.*(1.-(2.2569*10^-5)*elev)^5.2553)
  air_density = air_pressure/(287.04*T_a)
  dynamic_viscosity = (1.8325*10^-5)*((296.16+120.)/(T_a+120.))*((T_a/296.16)^1.5) #Tracy et al. 2010
  kinematic_viscosity = dynamic_viscosity/air_density
  T_surface = (T_a)*(1.+0.38*((e_s*1000.)/air_pressure)) #organism soil temperature in steady state heat balance
  T_air = (T_a)*(1.+0.38*((e_a*1000.)/air_pressure)) #air temperature in steady state heat balance
  coef_thermal_expansion = 1.0/T_a
  
  #Grashof and Nusselt numbers
  Grashof = (coef_thermal_expansion*gravity*(D^3)*(abs(T_surface-T_air)))/(kinematic_viscosity^2)
  Nusselt = 0.48*((Grashof)^0.25)
  
  thermal_conductivity = (2.4525*10^-2)+((7.038*10^-5)*(T_a-273.15))
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
  r_b = 0.93*((specific_heat*air_density)/hc)/100
  
  return(r_b)
}

#' Calculate humid operative temperature
#'
#' 
#' @details This function allows you to calculate humid operative temperature (adaptation of Campbell and Norman 1998). Source: Riddell EA. 2017. Physical calculations of resistance to water loss improve predictions of species range models. Ecological Monographs 87: 21-23.
#' @param r_i internal (skin) resistance (s cm^-1)
#' @param r_b boundary layer resistance (s cm^-1)
#' @param D body diameter (m), (diameter = 0.0016*log(mass) + 0.0061 for mass(g)) #empirical formula for diameter, Riddell et al. 2017
#' @param T_a ambient temperature (°C)
#' @param elev elevation (m)
#' @param e_s saturation vapor pressure (kPa)
#' @param e_a actual vapor pressure (kPa)
#' @param Qabs Solar and thermal radiation absorbed (W)
#' @param epsilon emissivity of salamander skin, default epsilon=0.96 
#' @return humid operative temperature (°C)
#' @keywords humid operative temperature
#' @family biophysical models
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' Tb_salamander_humid(r_i=4,r_b=1,D=0.01,T_a=20,elev=500,e_a=2.0,e_s=2.5,Qabs=400, epsilon=0.96)
#' }
#' 

Tb_salamander_humid<-function(r_i,r_b,D,T_a,elev,e_a, e_s,Qabs, epsilon=0.96){
  
  stopifnot(r_i>0,r_b>0,D>0,elev>0,e_s>0,e_a>0,epsilon>0.5,epsilon<=1)
  
  if(e_s<e_a) stop("Actual vapor pressure, e_a, must be lower that saturation vapor pressure, e_s.")
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  vpd= e_s -e_a #vapor pressure deficit
  
  #radiative conductance function, Campbell and Norman 1998
  radiative_conductance= (4*(5.670373*10^-8)*(T_a+273.15)^3)/29.3
 
  gamma_naut = 0.000666
  a = (r_i*100.0)/41.4
  gva = (r_b*100)/41.4
  rad = (4*5670373*10^(-8)*(T_a+273.15)*3.)/29.3
  gamma = gamma_naut*((a+(1./gva))/((1./rad)+(1./gva)))
  s = ((((17.502*240.97))*0.611*exp((17.502*T_a)/(T_a+240.97)))/(240.97+T_a)^2)/(101.3*exp(-elev/8200))
  Tbh = T_a+(gamma/(gamma+s))*(((Qabs - (epsilon*sigma*((T_a+273.15)^4)))/(29.3*(radiative_conductance+gva)))-(vpd/(gamma*(101.3*exp(-elev/8200)))))
  
  return(Tbh)
}

#' Estimate absorbed longwave (thermal) radiation
#' 
#' @details This function allows you to estimate longwave (thermal) radiation (W) absorbed from the sky and the ground (adaptation of Campbell and Norman 1998). Source: Riddell EA. 2017. Physical calculations of resistance to water loss improve predictions of species range models. Ecological Monographs 87: 21-23.
#' @param T_a air temperature (°C)
#' @param T_g ground temperature (°C)
#' @param epsilon_ground emmisitivity (proportion) for more soil types (Campbell and Norman 1998), default value of 0.97
#' @param a_longwave absorptance (proportion) of organism to longwave radiation (Bartlett and Gates 1967, Buckley 2008), default value of 0.965
#' @return thermal radiation absorbed (W)
#' @keywords longwave (thermal) radiation absorbed
#' @family biophysical models
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' Qthermal_radiation_absorbed(T_a=20, T_g=25, epsilon_ground=0.97, a_longwave=0.965)
#' }
#' 

Qthermal_radiation_absorbed<-function(T_a,T_g, epsilon_ground=0.97, a_longwave=0.965){
  
  stopifnot(epsilon_ground>=0, epsilon_ground<=1, a_longwave>=0, a_longwave<=1)
  
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #longwave radiation from sky function, Campbell and Norman 1998
  Slongwave_sky= 53.1*10^-14*(T_a+273.15)^6.
  
  #longwave radiation from ground function, Campbell and Norman 1998
  Slongwave_ground= epsilon_ground*sigma*(T_g+273.15)^4.
  
  #radiation absorbed function, adapted from Campbell and Norman 1998
  Slongwave= 0.5*a_longwave*(Slongwave_sky+Slongwave_ground)
  
  return(Slongwave)
}

#' Statistical approximation of soil temperature
#'
#' 
#' @details This function allows you to estimate soil temperature at a given depth and hour approximating diurnal variation as sinusoidal (adapted from Campbell and Norman 1998). Source: Riddell EA. 2017. Physical calculations of resistance to water loss improve predictions of species range models. Ecological Monographs 87: 21-23.
#' @param Tg_max daily maximum soil surface temperature (°C)
#' @param Tg_min daily minimum soil surface temperature (°C)
#' @param hour hour of the day
#' @param depth depth (cm) ???
#' @return soil temperature (°C)
#' @keywords soil temperature
#' @family biophysical models
#' @export
#' @author Eric Riddell
#' @examples
#' \dontrun{
#' Tsoil(Tg_max=30, Tg_min=15, hour=12, depth=5)
#' }
#' 

Tsoil<-function(Tg_max, Tg_min, hour, depth){
 
  stopifnot(Tg_max>Tg_min, depth>=0)
  
  offset= ifelse(hour %in% c(0,1,2,3), -13, 11)
  Tsoil= ((Tg_max+Tg_min)/2.0)+((Tg_max-Tg_min)/2.0)*(2.71**(-depth/5.238))*sin((3.14/12.)*(hour-offset)-depth/5.238)

  return(Tsoil)
}


#' Calculate Nusselt Number
#'
#' @details This function allows you to estimate the Nusselt Number, which describes dimensionless conductance (Gates 1980 Biophysical Ecology).
#' @param H_L Convective heat transfer coefficient (W m^-2 K^-1)
#' @param D is characteristic dimension (e.g., body diameter) (m)
#' @param K Thermal conductivity (W K^-1 m^-1)
#' @return Nusselt number
#' @keywords Nusselt number
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Nusselt_number(H_L=20, D=0.01, K=0.5)
#' }
#' 

Nusselt_number<-function(H_L, D, K){
  
  stopifnot(H_L>0, D>0, K>0)
  
  Nu = H_L * D / K #eq 9.24
  
  return(Nu)
}

#' Calculate Prandtl Number
#'
#' @details This function allows you to estimate the Prandtl Number, which describes the ratio of kinematic viscosity to thermal diffusivity (Gates 1980 Biophysical Ecology).
#' @param c_p is specific heat at constant pressure (J mol^{-1} K^{-1})
#' @param mu is dynamic viscosity (mol s^{-1}m^{-1})
#' @param K Thermal conductivity (W K^-1 m^-1)
#' @return Prandtl number
#' @keywords Prandtl number
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Prandtl_number(c_p=29.3, mu=0.00001, K=0.5)
#' }
#' 

Prandtl_number<-function(c_p, mu, K){
  
  stopifnot(c_p>0, mu>0, K>0)
  
  Pr= c_p *mu /K #eq 9.26
  return(Pr)
}

#' Calculate Reynolds Number
#'
#' @details This function allows you to estimate the Reynolds Number, which describes the dynamic properties of the fluid surrounding the animal as the ratio of internal viscous forces (Gates 1980 Biophysical Ecology).
#' @param D is characteristic dimension (e.g., body diameter) (m)
#' @param u is wind speed in m/s
#' @param nu is the kinematic viscosity, ratio of dynamic viscosity to density of the fluid (m^2 s^(-1)), can calculate from DRYAIR or WETAIR
#' 
#' @return Reynolds number
#' @keywords Reynolds number
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Reynolds_number(u=1, D=0.001, nu=1.2)
#' }
#' 

Reynolds_number<-function(u, D, nu){
  
  stopifnot(D>=0, u>=0, nu>=0)
  
     Re= u*D / nu #eq 9.25
  return(Re)
}

#' Calculate Grashof Number
#'
#' @details This function allows you to estimate the Grashof Number, which describes the abilty of a parcel of fluid warmer or colder than the surrounding fluid to rise against or fall with the attractive force of gravity. Ratio of a buoyant force times an inertial force to the square of a viscous force. Reference: Campell and Norman. 1998. An Introduction to Environmental Biophysics
#' @param Ta Air temperature (°C).
#' @param Tg Ground (surface) temperature (°C).
#' @param D is characteristic dimension (e.g., body diameter) (m)
#' @param nu is the kinematic viscosity, ratio of dynamic viscosity to density of the fluid (m^2 s^-1), can calculate from DRYAIR() or WETAIR()
#' @return Grashof number
#' @keywords Grashof number
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Grashof_number(Ta=30, Tg=35, D=0.001, nu=1.2)
#' }
#' 

Grashof_number<-function(Ta, Tg, D, nu){
  
  stopifnot(D>=0, nu>=0)
  
  #constant
  gravity = 9.8 #meters per second
  
  Gr = gravity * D^3* abs(Tg-Ta) / (Ta * nu^2)
    
  return(Gr)
}

#------
#' Calculate Grashof Number in Gates
#'
#' @details This function allows you to estimate the Grashof Number, which describes the abilty of a parcel of fluid warmer or colder than the surrounding fluid to rise against or fall with the attractive force of gravity (Gates 1980 Biophysical Ecology). Ratio of a buoyant force times an inertial force to the square of a viscous force.
#' @param Ta Air temperature (°C).
#' @param Tg Ground (surface) temperature (°C).
#' @param beta coefficient of volumetric thermal expansion, beta= 3.67 x 10^-3 °C^-1  in air and 41.9 x 10^-4 °C^-1 in water.
#' @param D is characteristic dimension (e.g., body diameter) (m)
#' @param nu is the kinematic viscosity, ratio of dynamic viscosity to density of the fluid (m^2 s-1), can calculate from DRYAIR or WETAIR
#' 
#' @return Grashof number
#' @keywords Grashof number
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Grashof_number_Gates(Ta=30, Tg=35, beta=0.00367, D=0.001, nu=1.2)
#' }
#' 

Grashof_number_Gates<-function(Ta, Tg, beta, D, nu){

  stopifnot(beta>0, D>=0, nu>0)
  
  #constant
  gravity = 9.8 #meters per second
  
  Gr = gravity * beta * D^3 *abs(Tg-Ta) / nu^2
  
  return(Gr)
}
#---------

#' Estimate the Nusselt number from the Reynolds number (based on Mitchell 1976)
#' 
#' @details This function allows you to estimate the Nusselt number from the Reynolds number for various taxa.  Source: Mitchell JW. 1976. Heat transfer from spheres and other animal forms. Biophysical Journal 16: 561-569. See Table 1. Convective Heat Trasfer Relations for Animal Shapes.  
#' @param Re is the Reynolds Number (dimensionless)
#' @param taxa Which class of organism, current choices: sphere, cylinder, frog, lizard_traverse_to_air_flow, lizard_parallel_to_air_flow, lizard_surface, lizard_elevated, flyinginsect, spider
#' @return Nusselt number (dimensionless)
#' @keywords Nusselt number
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Nu_from_Re(Re=5, taxa="cylinder")
#' }
#' 

Nu_from_Re<-function(Re, taxa="cylinder"){
  
  taxas= c("sphere","cylinder","frog","lizard_traverse_to_air_flow", "lizard_parallel_to_air_flow","lizard_surface","lizard_elevated","flyinginsect","spider")
  stopifnot(taxa %in% taxas)
  
  # Dimensionless constant (Cl)
  Cls= c(0.37,0.615,0.258,0.35,0.1,1.36,1.91,0.0749,0.47)
  ns= c(0.6,0.466,0.667,0.6,0.74,0.39,0.45,0.78,0.5) 
  
  #find index  
  ind= match(taxa, taxas)
  
  Nu <- Cls[ind] *Re^(ns[ind])
  
  return(Nu)
}

#' Estimate the Nusselt number from the Grashof number (based on Gates 1980)
#' 
#' @details This function allows you to estimate the Nusselt number from the Grashof Number.  
#' @param Gr is the Grashof Number (dimensionless)
#' @return Nusselt number (dimensionless)
#' @keywords Nusselt number
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Nu_from_Gr(Gr=5)
#' }
#' 

Nu_from_Gr<-function(Gr){
  
  Nu <- 0.48 * Gr^0.25
  
  return(Nu)
}


#' Commpare Grashof and Reyolds numbers to determine whether convection is free or forced (Gates 1980)
#' 
#' @details This function allows you to commpare the Grashof and Reyolds numbers to determine whether convection is free or forced (Gates 1980).
#' @param Gr is the Grashof Number (dimensionless)
#' @param Re is the Reynolds Number (dimensionless)
#' @return "free", "forced" or "intermediate"
#' @keywords free or forced convection
#' @family biophysical models
#' @export
#' @examples
#' \dontrun{
#' Free_or_forced_convection(Gr=100, Re=5)
#' }
#' 

Free_or_forced_convection<-function(Gr, Re){
  
  conv= "intermediate condition, mixed convection based on Nusselt numbers is appropriate"
  if(Gr<0.1*Re^2) conv="forced convection" #P284
  if(Gr>16*Re^2) conv="free convection"
  
  return(conv)
}


