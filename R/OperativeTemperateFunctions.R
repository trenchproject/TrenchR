#' Predicts body temperatures (operative environmental temperature) of an ectotherm in K. 
#' 
#' 
#' @details Predicts body temperatures (operative environmental temperature) of an ectotherm in K. Uses approximation in Campbell and Norman (1998).
#' 
#' @param Ta is air temperature in K.
#' @param Rabs solar and thermal radiation absorbed (W m^-2)
#' @param emissivity longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param c_p specific heat of air (J mol^-1 K^-1)
#' @param d characteristic dimension of the animal (m)
#' @param u is wind speed in m/s
#' @return operative environmental temperature (K)
#' @keywords operative environmental temperature
#' @export
#' @examples 
#' \dontrun{
#' estimate_Te_CampbellNorman(Ta=303, Rabs=823, emissivity=0.96, c_p=29.3, d=0.17, u=1)
#'}
#' 
estimate_Te_CampbellNorman=function(Ta, Rabs, emissivity=0.96, c_p=29.3, d, u){
    
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #thermal radiation emitted
  Qemissivity= emissivity*sigma*Ta^4
  
  #conductance
  g_Ha=1.4*0.135*sqrt(u/d) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976)
  g_r= 4*sigma*Ta^3/c_p # (12.7) radiative conductance
  
  # operative environmental temperature
  Te=Ta+(Rabs-Qemissivity)/(c_p*(g_r+g_Ha))                       

  return(Te) 
}

#' Estimates net energy exchange between an animal and the environment in W.
#' 
#' 
#' @details Estimates net energy exchange between an animal and the environment in W. Follows Gates (1980, Biophysical ecology) and others.
#' 
#' @param Qabs Solar and thermal radiation absorbed (W)
#' @param Qemit Thermal radiation emitted (W)
#' @param Qconv Energy exchange between an animal and the air (W)
#' @param Qcond Energy exchange between an animal and the surface (W)
#' @param Qmet Energy emitted due to metabolism (W)
#' @param Qevap Energy emitted due to evaporative water loss (W)
#' @return net energy exchange (W)
#' @keywords operative environmental temperature
#' @export
#' @examples 
#' \dontrun{
#' estimate_Qnet_Gates(Qabs=500, Qconv=100, Qcond=100, Qmet=10, Qevap=5)
#'}
#' 
estimate_Qnet_Gates=function(Qabs, Qconv, Qcond, Qmet, Qevap){
  
  Qnet= Qabs -Qemit +Qconv +Qcond +Qmet -Qevap
 
  return(Qnet) 
}

#' Predicts body temperatures (operative environmental temperature) of an ectotherm in K. 
#' 
#' 
#' @details Predicts body temperatures (operative environmental temperature) of an ectotherm in K. Uses approximation in Gates (1980, Biophysical ecology). Omits evaporative and metabolic heat loss.

#' @param As surface area  in m^2
#' @param d characteristic dimension for conduction (m)
#' @param psa_dir proportion surface area exposed to sky (or enclosure)
#' @param psa_ref proportion surface area exposed to ground
#' @param psa_air of surface area exposed to air
#' @param psa_g of surface in contact with substrate
#' @param Ts ground surface temperatue in K
#' @param Ta ambient air temperature in K
#' @param Rabs Solar and thermal radiation absorbed (W)
#' @param emissivity longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param h_L Convective heat transfer coefficient (W m^-2 K^-1)
#' @param K Thermal conductivity (W K^-1 m^-1 ), K=0.5 W K^-1 m^-1 for naked skin, K=0.15 for insect cuticle ( Galushko et al 2005); conductivity of ground is generally greater than that of animal tissues, so animal thermal conductivity is generally rate limiting step. 
#' @return operative environmental temperature (K)
#' @keywords operative environmental temperature
#' @export
#' @examples 
#' \dontrun{
#' estimate_Te_Gates(As=1, d=0.001, psa_dir=0.6, psa_ref=0.4, psa_air=0.6, psa_g=0.2, Ts=303, Ta=310, Rabs=800, emissivity=0.95, h_L=10, K=0.5)
#'}
#' 
estimate_Te_Gates=function(As, d, psa_dir, psa_ref, psa_air, psa_g, Ts, Ta, Rabs, emissivity, h_L, K){
  
  #Solar and thermal radiation
  Qabs= Rabs
  #-------------
  #Thermal radiaiton emitted
  
    #Stefan-Boltzmann constant
    sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #Areas
  A_s = As*psa_dir
  A_r = As*psa_ref
  
  #estimate effective radiant temperature of sky
  #Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
  Tsky= (1.22*(Ta-273.15) -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation
  
 # Qemit= emissivity*sigma*(A_s*(Tb^4 - Tsky^4)+A_r*(Tb^4 - Ts^4))
  #-----------
  #Convection
    # Calculate skin area exposed to air
    A_air = As*psa_air
  
 # Qconv =   h_L*A_air*(Ta-Tb)
  
  #-----------
  #Conduction
  
    # Calculate the area of contact
    A_contact  = As * psa_g
  
#  Qcond = A_contact*K*(Ts - Tb)/d
  
  #-----------
  #solve energy balance for steady state conditions
  # 0= Qabs -Qemit +Qconv +Qcond
  # Qemit= emissivity*sigma*(A_s*(Tb^4 - Tsky^4)+A_r*(Tb^4 - Ts^4))
  # Qconv =   h_L*A_air*(Ta-Tb)
  # Qcond = A_contact*K*(Ts - Tb)/d
 
  Qfn = function(Tb, Qabs, emissivity, sigma, A_s, Tsky, A_r, Ts, h_L, A_air, Ta, A_contact, K, d) {
    return(Qabs -(emissivity*sigma*(A_s*(Tb^4 - Tsky^4)+A_r*(Tb^4 - Ts^4))) +h_L*A_air*(Ta-Tb)  +A_contact*K*(Ts - Tb)/d)
  }
  
  Te <- uniroot(Qfn, c(273, 323),Qabs=Qabs, emissivity=emissivity, sigma=sigma, A_s=A_s, Tsky=Tsky, A_r=A_r, Ts=Ts, h_L=h_L, A_air=A_air, Ta=Ta, A_contact=A_contact, K=K, d=d, tol = 0.0001)
  
  return(Te$root)
}