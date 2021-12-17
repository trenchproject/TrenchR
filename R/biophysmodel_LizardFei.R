#' @title Calculate Operative Temperature of Lizard Using model by Fei et al.
#' 
#' @description Predicts body temperature (operative environmental temperature) of a lizard in K based on \insertCite{Fei2012;textual}{TrenchR}.
#'  \cr \cr
#'
#' @param T_a air temperature at lizard height in K.
#' 
#' @param T_g surface temperature in K.
#' 
#' @param H  total (direct + diffuse) solar radiation flux in W / \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}.
#' 
#' @param lw Downward flux of near-infrared radiation (W / \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @param shade proportion of shade.
#' 
#' @param m lizard mass in g.
#' 
#' @param Acondfact the proportion of the lizard projected area that is in contact with the ground. \code{Acondfact = 0.1} for standing and \code{Acondfact = 0.4} for lying on ground.
#' 
#' @param Agradfact the proportion of the lizard projected area exposed to radiation from the ground. \code{Agradfact = 0.3} for standing and \code{Agradfact = 0.0} for lying on ground.
#' 
#' @return Body temperature of a lizard in K.
#' 
#' @family biophysical models
#' 
#' @author Ofir Levy
#'
#' @details Thermal radiative flux is calculated following \insertCite{Fei2012;textual}{TrenchR} based on \insertCite{Bartlett1967;textual}{TrenchR} and \insertCite{Porter1973;textual}{TrenchR}.
#'
#' 
#' @export
#'
#' @references
#'  \insertAllCited{}
#'  
#' @examples
#'   Tb_Fei(
#'     T_a = 293,
#'     T_g = 300,
#'     H = 1300, 
#'     lw = 60, 
#'     shade = 0.5, 
#'     m = 10.5, 
#'     Acondfact = 0.1, 
#'     Agradfact = 0.3)
#' 
#' 

Tb_Fei <- function(T_a, T_g, H, lw, shade, m, Acondfact, Agradfact){
  
  stopifnot(T_a > 200, T_a < 400, T_g > 200, T_g < 400, H >= 0, lw >= 0, shade >= 0, shade <= 1, m >= 0, Acondfact >= 0, Acondfact <= 1, Agradfact >= 0, Agradfact <= 1)
  
  # Constants

    # thermal absoptivity (proportion), Bartlett & Gates 1967

      alpha_L <- 0.965 

    # convective heat transfer ceofficient (W m-2 K-1) (Fei et al. 2012, J Ther Biol, 37: 56-64, Porter et al. 1973)

      h_L <- 10.45 

    # emissivity of lizard's skin (proportion)

      epsilon_lizard <- 0.95 


    # thermal conductivity (W K-1 m-1)

      K_lizard <- 0.5 

    # lizard mean thickness in meters (diameter)

      lambda <- 0.015

    # specific heat capacity (J kg-1 K-1)

      c_lizard <- 3762

    # time interval in seconds

      dt <- 120

    # stefan-boltzmann constant (W m^-2 K^-4)

      sigma <- 5.67*10.0^(-8) 
  
  # Convert mass to kg

    mass_kg <- m / 1000
  
  # Estimate areas

    # Surface area m2

      A_L <- 0.0314 * pi * mass_kg^(2 / 3) 

    # Estimate projected lizard area for radiation from the ground

      A_down <- Agradfact * A_L 

    # Estimate projected lizard area that contacts the ground
  
      A_contact <- Acondfact * A_L 

  
  # Initial operative environmental temperature

    # Initial body temperature in kelvin, assume Ta 

      T_o <- T_a  
  
  # solar radiation

    # projected lizard area for direct and scattered solar radiation

      A_p <- 0.4 * A_L 
  
  # Check

    dQ_solar <- (1 - shade) * alpha_L * A_p * H
  

  # Net longwave radiation

    # Proportion of surface area facing toward the sky

      A_up <- 0.6*A_L 

    # (10.11) clear sky emissivity

      epsilon_ac <- 9.2 * 10^-6 * (T_a)^2 
  
  # Iterate to calculate steady state

    for (i in c(1:50)) {
    
      # Thermal radiation

        dQ_IR <- epsilon_lizard * A_down * sigma * (T_g^4 - T_o^4) + epsilon_lizard * A_up * sigma * (T_a^4 - T_o^4)

        # alternative version
        #
        #   dQ_IR <- epsilon_lizard * A_down * sigma * (T_g^4 - T_o^4) + epsilon_lizard * A_up * ((1 - shade) * lw + shade * sigma * T_a^4) - epsilon_lizard * A_up * sigma * T_o^4
      

      # Conduction

        dQ_cond = A_contact*K_lizard*(T_g - T_o)/(lambda/2)
  
      # Convection, assuming no wind

        # skin area that is exposed to air

          Aair <- 0.9 * A_L
          dQ_conv <- h_L * Aair * (T_a - T_o)
  
      # Metabolism

        TinC <- T_o - 273.15
        dQ_meta <- 0.348 * exp(0.022 * TinC - 0.132) * mass_kg

        # alternative version
        #
        #   ew <- exp(-10.0+0.51*log(m)+0.115*(T_o-273)) *3 #Buckley 2008
        #   dQ_meta <- ew/3600. #metabolic rate (j/s)

    
      # Respiratory loss

        if (TinC < 20) {

          dQ_resp <- 0.272 * mass_kg

        } else if (TinC > 36) {

          dQ_resp <- 0.003 * mass_kg * exp(0.1516 * TinC)

        } else {

          dQ_resp <- 0.0836 * mass_kg * exp(0.0586 * TinC)

        }
    
      dQe <- (dQ_solar + dQ_IR + dQ_meta + dQ_cond + dQ_conv - dQ_resp)
   
      dTe <- dQe / ((mass_kg) * c_lizard)
      T_o <- T_o + dTe * dt
    
    }
  
  T_o

}