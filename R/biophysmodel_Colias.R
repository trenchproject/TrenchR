#' @title Operative Environmental Temperature of a Butterfly
#'
#' @description Predicts body temperatures (operative environmental temperatures) of a butterfly in C. Based on \insertCite{Kingsolver1983;textual}{TrenchR} and \insertCite{Buckley2012;textual}{TrenchR}.
#' 
#' @param T_a \code{numeric} air temperature in C.
#'
#' @param Tg  \code{numeric} surface temperature in C in the sunlight.
#'
#' @param Tg_sh \code{numeric} surface temperature in C in the shade.
#'
#' @param u \code{numeric} wind speed in m / s.
#'
#' @param H_sdir \code{numeric} direct solar radiation flux in \ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}.
#'
#' @param H_sdif \code{numeric} diffuse solar radiation flux in \ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}.
#'
#' @param z \code{numeric} solar zenith angle in degrees.
#'
#' @param D \code{numeric} thoracic diameter in cm.
#'
#' @param delta \code{numeric} thoracic fur thickness in mm.
#'
#' @param alpha \code{numeric} wing solar absorptivity as a proportion. Range for Colias is 0.4 to 0.7.
#'
#' @param r_g \code{numeric} substrate solar reflectivity (proportion), see \insertCite{Kingsolver1983;textual}{TrenchR}.
#'
#' @param shade \code{logical} indicator whether body temperature should be calculate in sun (\code{FALSE}) or shade (\code{TRUE}).
#'
#' @return \code{numeric} predicted body (operative environmental) temperature (C).
#'
#' @details 
#'  Thermal radiative flux is calculated following \insertCite{Gates1980;textual}{TrenchR} based on \insertCite{Swinbank1960;textual}{TrenchR} and \insertCite{Kingsolver1983;textual}{TrenchR} estimates using the Brunt equation with black body sky temperature from \insertCite{Swinbank1963;textual}{TrenchR}.
#'
#' @family biophysical models
#'
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   Tb_butterfly(T_a    = 25, 
#'                Tg     = 25, 
#'                Tg_sh  = 20, 
#'                u      = 0.4, 
#'                H_sdir = 300, 
#'                H_sdif = 100, 
#'                z      = 30, 
#'                D      = 0.36, 
#'                delta  = 1.46, 
#'                alpha  = 0.6, 
#'                r_g    = 0.3)
#'
Tb_butterfly <- function (T_a, 
                          Tg, 
                          Tg_sh, 
                          u, 
                          H_sdir, 
                          H_sdif, 
                          z, 
                          D, 
                          delta, 
                          alpha, 
                          r_g    = 0.3,
                          shade  = FALSE) {

  stopifnot(u >= 0, 
            H_sdir >= 0, 
            H_sdif >= 0, 
            z >= -90, 
            z <= 90, 
            D > 0, 
            delta >= 0, 
            alpha >= 0, 
            r_g >= 0, 
            r_g <= 1, 
            is.logical(shade))  
  
  # conversions

    # temperatures C to K

      TaK    <- celsius_to_kelvin(T_a)
      TaK_sh <- TaK
      Tg     <- celsius_to_kelvin(Tg)
      Tg_sh  <- celsius_to_kelvin(Tg_sh)

    # wind speed m/s to cm/s

      u <- u *100  

    # solar flux W/m2 to mW/cm2

      H_sdir <- H_sdir / 10 
      H_sdif <- H_sdif / 10

    # thoracic fur thickness mm to cm

      delta <- delta / 10     


  # Total solar radiation

    H_sttl <- H_sdir + H_sdif


  # Butterfly Parameters

    # surface emissivity, ranges from 0.95-1

      epsilon_s <- 0.97 

      sigma <- stefan_boltzmann_constant(units = "mW_cm-2_K-4") # note units

    # butterfly thermal emissivity

      Ep <- 1 

    # k_e- thermal conductivity of the fur, 1.3 mW cm^-1*K^-1

      k_e <- 1.3 

    # r_i- body radius from Kingsolver 1983

      r_i <- 0.15 

    # approximate thermal conductivity of air, mWcm^-1*K^-1

      k_a <- 0.25

    #  kinematic viscosity of air, cm^2/s at 300K 
    #     http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html

      v <- 15.68 * 10^-2  


  # Calculations

    # total surface area cm^2 as area of cylinder without ends

      A_sttl <- pi * D * 2 

    # direct and reflected surface areas For butterflies basking with wings perpendicular to radiation 

      A_sdir <- A_sttl / 2
      A_sref <- A_sdir

    # Radiative Heat Flux, mW
  
      Q_s <- alpha * A_sdir * H_sdir / cos(z * pi / 180) + alpha * A_sref * H_sdif + alpha * r_g * A_sref * H_sttl  

    # Thermal Radiative Flux in K

    # black body sky temperature from Swinbank 1963

      # Tsky <- 0.0552*(TaK)^1.5 

    # Gates 1980 Biophysical ecology based on Swinbank 1960, Kingsolver (1983) estimates using Brunt equation

      Tsky <- (1.22 * T_a - 20.4) + 273.15 
 
      # Q_t <- 0.5* A_sttl * Ep * sigma * (Tb^4 - Tsky^4) +0.5* A_sttl * Ep * sigma * (Tb^4 - Tg^4)


    # Convective Heat Flux

      # Reynolds number- ratio of interval viscous forces

      R_e <- u * D / v

      # Nusselt number- dimensionless conductance

        N_u <- 0.6 * R_e^0.5

      # Kingsolver 1983

        # N_u <- 2.3 


      h_c <- N_u * k_a / D


      # total convective heat transfer coefficient

        h_T <- (1 / h_c + (r_i + delta) * log((r_i + delta) / r_i) / k_e)^-1;  

      # convective heat transfer surface area

        # A_c <- A_sttl
  
      # Q_c <- h_T * A_c * (Tb-T_a)

 
  # Shade Adjustments

    if (shade) {

      # Calculate without basking by dividing areas by two

        A_sttl <- A_sttl / 2

      # Radiative Heat Flux in Shade, mW

        A_sdir <- A_sttl/2
        A_sref <- A_sdir

        # No direct radiation, only diffuse and reflected
  
          H_sdir_sh <- 0
          H_sdif_sh <- H_sdif
          H_sttl <- H_sdif + H_sdif_sh 

        Q_s <- alpha * A_sdir * H_sdir_sh / cos(z * pi / 180) + alpha * A_sref * H_sdif_sh + alpha * r_g * A_sref * H_sttl; 

      # Use shaded surface temperature

        Tg< - Tg_sh

    }
               			
  # Solution 

    a <- A_sttl * Ep * sigma
    b <- h_T * A_sttl
    d <- h_T * A_sttl * TaK +0.5 * A_sttl * Ep * sigma * Tsky^4 + 0.5 * A_sttl * Ep * sigma * (Tg)^4 + Q_s

    # in K

      Te <- 1 / 2 * sqrt((2 * b) / (a * sqrt((sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3) / (2^(1 / 3) * 3^(2 / 3) * a) - (4 * (2 / 3)^(1 / 3) * d) / (sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3))) - (sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3) / (2^(1 / 3) * 3^(2 / 3) * a) + (4 * (2 / 3)^(1 / 3) * d) / (sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3)) - 1 / 2 * sqrt((sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3) / (2^(1 / 3) * 3^(2 / 3) * a) - (4 * (2 / 3)^(1 / 3) * d) / (sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3)) 

    # in C

      kelvin_to_celsius(Te)

} 


