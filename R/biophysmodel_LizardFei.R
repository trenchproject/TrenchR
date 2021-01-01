#' Calculate body temperature(Operative Temperature) of Lizard using model by Fei et al.
#' 
#'
#' @details Predicts body temperature (operative environmental temperature) of a lizard in K.
#' @description Transient model for predicting operative environmental temperature using model in the following publication: T. Fei, A. K. Skidmore, V. Venus, T. Wang, M. Schlerf, B. Toxopeus, 
#' S. van Overjijk, M. Bian, and Y. Liu, “A body temperature model for lizards as estimated from the thermal environment,” J. Therm. Biol., vol. 37, no. 1, pp. 56–64, 2012.
#' @param T_a is air temperature at lizard height in K
#' @param T_g  is surface temperature in K
#' @param H  is total (direct + diffuse) solar radiation flux in W/m^2
#' @param lw Downward flux of near-infrared radiation (W/m^2)
#' @param shade proportion of shade
#' @param m is lizard mass in g
#' @param Acondfact is the proportion of the lizard projected area that is in contact with the ground, Acondfact=0.1 for standing and Acondfact=0.4 for lying on ground
#' @param Agradfact is the proportion of the lizard projected area exposed to radiation from the ground, Agradfact=0.3 for standing and Agradfact=0.0 for lying on ground
#' @return Body temperature of a lizard in K
#' @family biophysical models
#' @author Ofir Levy
#' @export 
#' @examples
#' \dontrun{
#' Tb_Fei(T_a=293, T_g=300, H=1300, lw=60, shade=0.5, m=10.5, Acondfact=0.1, Agradfact=0.3)
#' }
#' 

Tb_Fei <- function(T_a, T_g, H, lw, shade, m, Acondfact, Agradfact){
  
  stopifnot(T_a>200, T_a<400, T_g>200, T_g<400, H>=0, lw>=0, shade>=0, shade<=1, m>=0, Acondfact>=0, Acondfact<=1, Agradfact>=0, Agradfact<=1)
  
  # ---- Start Constants
  alpha_L = 0.965 # thermal absoptivity (proportion), Bartlett & Gates 1967
  h_L = 10.45 #convective heat transfer ceofficient (W m-2 K-1) (Fei et al. 2012, J Ther Biol, 37: 56-64, Porter et al. 1973?)
  epsilon_lizard=0.95 # emissivity of lizard's skin (proportion)
  K_lizard = 0.5 # thermal conductivity (W K-1 m-1)
  lambda = 0.015 # lizard mean thickness in meters (diameter)
  c_lizard = 3762 #specific heat capacity (J kg-1 K-1)
  dt =  120 #time interval in seconds
  
  sigma=5.67*10.0^(-8) # stefan-boltzmann constant (W m^-2 K^-4)
  
  #convert mass to kg
  mass_kg=m/1000. #in kg
  
  #Estimate areas
  A_L= 0.0314 * pi * mass_kg^(2./3.) #surface area m2
  #Estimate projected lizard area for radiation from the ground
  A_down=Agradfact*A_L 
  #Estimate projected lizard area that contacts the ground
  A_contact = Acondfact*A_L 
  
  #-----
  #Initial operative environmental temperature
  T_o = T_a  #initial body temperature in kelvin, assume Ta 
  
  #solar radiation
  A_p = 0.4*A_L #projected lizard area for direct and scattered solar radiation
  
  #check
  dQ_solar = (1-shade)*alpha_L*A_p*H
  
  #net longwave radiation
  A_up = 0.6*A_L #proportion of surface area facing toward the sky
  epsilon_ac= 9.2*10^-6*(T_a)^2 # (10.11) clear sky emissivity
  
  #iterate to calculate steady state
  for (i in c(1:50)) {
    
    #thermal radiation
      dQ_IR = epsilon_lizard*A_down*sigma*(T_g^4. - T_o^4.) + epsilon_lizard*A_up*sigma*(T_a^4. - T_o^4.)
            #Alternative version
           #dQ_IR = epsilon_lizard*A_down*sigma*(T_g^4. - T_o^4.) + epsilon_lizard*A_up*((1-shade)*lw + shade*sigma*T_a^4.) - epsilon_lizard*A_up*sigma*T_o^4.
      
    #conduction
    dQ_cond = A_contact*K_lizard*(T_g - T_o)/(lambda/2)
  
    #convection, assuming no wind
    Aair = 0.9 * A_L # skin area that is exposed to air
    dQ_conv = h_L * Aair * (T_a - T_o)
  
    #Metabolism
    # ew = exp(-10.0+0.51*log(m)+0.115*(T_o-273)) *3 #Buckley 2008
    # dQ_meta = ew/3600. #metabolic rate (j/s)
    
    TinC = T_o - 273.15
    dQ_meta = 0.348 * exp(0.022 * TinC - 0.132) * mass_kg
    
    #Respiratory loss
    if (TinC <20) {
      dQ_resp = 0.272 * mass_kg
    } else if (TinC > 36) {
      dQ_resp = 0.003 * mass_kg * exp(0.1516 * TinC)
    } else {
      dQ_resp = 0.0836 * mass_kg * exp(0.0586 * TinC)
    }
    
    dQe = (dQ_solar + dQ_IR + dQ_meta + dQ_cond + dQ_conv - dQ_resp)
  
    dTe = dQe/((mass_kg)*c_lizard)
    T_o = T_o + dTe*dt
    
  } #end iteration loop
  
  return (T_o)
}
