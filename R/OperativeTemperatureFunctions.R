#' Predicts body temperatures (operative environmental temperature) of an ectotherm in K. 
#' 
#' 
#' @details Predicts body temperatures (operative environmental temperature) of an ectotherm in K. Uses approximation in Campbell and Norman (1998).
#' 
#' @param T_a is air temperature in K.
#' @param S solar and thermal radiation absorbed (W m^-2)
#' @param epsilon longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param c_p specific heat of air (J mol^-1 K^-1)
#' @param D characteristic dimension of the animal (m)
#' @param V is wind speed in m/s
#' @return operative environmental temperature, T_e (K)
#' @keywords operative environmental temperature
#' @export
#' @examples 
#' \dontrun{
#' Tb_CampbellNorman(T_a=303, S=823, epsilon=0.96, c_p=29.3, D=0.17, V=1)
#'}
#' 
Tb_CampbellNorman=function(T_a, S, epsilon=0.96, c_p=29.3, D, V){
    
  #Stefan-Boltzmann constant
  sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #thermal radiation emitted
  Qemit= epsilon*sigma*T_a^4
  
  #conductance

  g_Ha=1.4*0.135*sqrt(V/D) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976), assumes forced conduction
  g_r= 4*sigma*T_a^3/c_p # (12.7) radiative conductance
  
  # operative environmental temperature
  T_e=T_a+(S-Qemit)/(c_p*(g_r+g_Ha))                       

  return(T_e) 
}

#' Estimates net energy exchange between an animal and the environment in W.
#' 
#' @details Estimates net energy exchange between an animal and the environment in W. Follows Gates (1980, Biophysical ecology) and others.
#' 
#' @param Qabs Solar and thermal radiation absorbed (W)
#' @param Qemit Thermal radiation emitted (W)
#' @param Qconv Energy exchange due to convection; Energy exchange from an animal to its surrounding environment (air or water) (W)
#' @param Qcond Energy exchange due to conduction; Energy exchange from animal to a surface if its in contact  (W)
#' @param Qmet Energy emitted due to metabolism (W)
#' @param Qevap Energy emitted due to evaporative water loss (W)
#' @return net energy exchange (W)
#' @keywords operative environmental temperature
#' @export
#' @examples 
#' \dontrun{
#' Qnet_Gates(Qabs=500, Qemit=10, Qconv=100, Qcond=100, Qmet=10, Qevap=5)
#'}
#' 
Qnet_Gates=function(Qabs, Qemit, Qconv, Qcond, Qmet, Qevap){
  
  Qnet= Qabs -Qemit -Qconv -Qcond -Qmet -Qevap
 
  return(Qnet) 
}

#' Predicts body temperatures (operative environmental temperature) of an ectotherm in K. 
#' 
#' 
#' @details Predicts body temperatures (operative environmental temperature) of an ectotherm in K. Uses approximation in Gates (1980, Biophysical ecology). Omits evaporative and metabolic heat loss.

#' @param A surface area  in m^2
#' @param D characteristic dimension for conduction (m)
#' @param psa_dir proportion surface area exposed to sky (or enclosure)
#' @param psa_ref proportion surface area exposed to ground
#' @param psa_air proportion surface area exposed to air
#' @param psa_g proportion surface area in contact with substrate
#' @param T_g ground surface temperature in K
#' @param T_a ambient air temperature in K
#' @param Qabs Solar and thermal radiation absorbed (W)
#' @param epsilon longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals (Gates 1980)
#' @param H_L Convective heat transfer coefficient (W m^-2 K^-1)
#' @param ef is the enhancement factor, used to adjust H_L to field condictions (using h_L approximation from Mitchell 1976).  Approximated as 1.23 by default, but see Mitchell 1976 for relationship.
#' @param K Thermal conductivity (W K^-1 m^-1 ), K=0.5 W K^-1 m^-1 for naked skin, K=0.15 for insect cuticle ( Galushko et al 2005); conductivity of ground is generally greater than that of animal tissues, so animal thermal conductivity is generally rate limiting step. 
#' @return operative environmental temperature (K)
#' @keywords operative environmental temperature
#' @export
#' @examples 
#' \dontrun{
#' Tb_Gates(A=1, D=0.001, psa_dir=0.6, psa_ref=0.4, psa_air=0.6, psa_g=0.2, T_g=303, T_a=310, Qabs=800, epsilon=0.95, H_L=10, ef=1.23, K=0.5)
#'}
#' 
Tb_Gates=function(A, D, psa_dir, psa_ref, psa_air, psa_g, T_g, T_a, Qabs, epsilon, H_L,ef=1.3, K){

  
    #Stefan-Boltzmann constant
    sigma= 5.673*10^(-8) #W m^(-2) K^(-4)
  
  #Areas
  A_s = A*psa_dir 
  A_r = A*psa_ref 
  # Calculate skin area exposed to air
  A_air = A*psa_air
  # Calculate the area of contact
  A_contact  = A*psa_g
  
  #estimate effective radiant temperature of sky
  #Tsky=0.0552*(T_a)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
  Tsky= (1.22*(T_a-273.15) -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation
  
  #solve energy balance for steady state conditions
  # 0= Qabs -Qemit -Qconv -Qcond

  Qfn = function(Tb, Qabs, epsilon, sigma, A_s, Tsky, A_r, T_g, H_L, A_air, T_a, A_contact, K, D) {

    #Thermal radiaton emitted
    Qemit= epsilon*sigma*(A_s*(Tb^4 - Tsky^4)+A_r*(Tb^4 - T_g^4))
    #Convection
    Qconv= ef*H_L*A_air*(Tb-T_a)
    #Conduction
    Qcond= A_contact*K*(Tb-T_g)/D

    return(Qabs -Qemit -Qconv -Qcond)
  }
  

  Te <- tryCatch(uniroot(Qfn, c(273, 353),Qabs=Qabs, epsilon=epsilon, sigma=sigma, A_s=A_s, Tsky=Tsky, A_r=A_r, T_g=T_g, H_L=H_L, A_air=A_air, T_a=T_a, A_contact=A_contact, K=K, D=D, tol = 0.0001), error = function(e) {print("Unable to balance energy budget. One issue to check is whether absorbed solar radiation exceeds energy potentially lost to thermal radiation, convection, and conduction.")})
  Te.return=NA
  if(length(Te)>1) Te.return=Te$root
  
  return(Te.return)
}