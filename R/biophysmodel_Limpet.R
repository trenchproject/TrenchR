#' @title Operative Environmental Temperature of a Limpet
#'
#' @description Predicts body temperatures (operative environmental temperatures) of a limpet in C.
#'  \cr \cr
#'  Based on \insertCite{Denny2006;textual}{TrenchR}.
#' 
#' @param T_a \code{numeric} air temperature in C.
#'
#' @param T_r \code{numeric} rock surface temperature in C in the sunlight.
#'
#' @param L \code{numeric} limpet length (anterior/posterior axis) (m).
#'
#' @param H \code{numeric} limpet height (dorsal/ventral axis) (m).
#'
#' @param I \code{numeric} solar irradiance W / \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}.
#'
#' @param u \code{numeric} wind speed (m / s).
#'
#' @param psi \code{numeric} solar zenith angle (degrees): can be calculated from zenith_angle function.
#'
#' @param c \code{numeric} fraction of the sky covered by cloud.
#'
#' @param position \code{character} direction of the limpet that is facing upwind. Options are \code{"anterior"}, \code{"posterior"}, and \code{"broadside"}.
#'
#' @return \code{numeric} predicted body (operative environmental) temperature (C).
#'
#' @family biophysical models 
#'
#' @details The original equation uses a finite-difference approach where they divide the rock into series of chunks, and calculate the temperature at each node to derive the conductive heat. For simplification, here it takes the rock temperature as a parameter, and conductive heat is calculated by the product of the area, thermal conductivity of rock and the difference in temperatures of the rock and the body.
#'   \cr \cr
#'   Limpets are simulated as cones following and using solar emissiviity values from \insertCite{Campbell1998;textual}{TrenchR}.
#'   \cr \cr
#'   The area of the limpet's shell (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) is projected in the direction at which sunlight strikes the organism \insertCite{Pennell1989;textual}{TrenchR}.
#'   \cr \cr
#'   Air conductivity values (W / (\ifelse{html}{\out{m<sup>-1</sup>}}{\eqn{m^-1}{ASCII}} \ifelse{html}{\out{K<sup>-1</sup>}}{\eqn{K^-1}{ASCII}})) are calculated following \insertCite{Denny2006;textual}{TrenchR}.
#'
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   Tb_limpet(
#'     T_a = 25, 
#'     T_r = 30, 
#'     L = 0.0176, 
#'     H = 0.0122, 
#'     I = 1300, 
#'     u = 1, 
#'     psi = 30, 
#'     c = 1, 
#'     position = "anterior")
#' 
Tb_limpet <- function (T_a, T_r, L, H, I, u, psi, c, position = "anterior") {
  
  stopifnot(L > 0, H > 0, I > 0, u >= 0, psi >= 0, psi <= 90, c >= 0, c <= 1, position %in% c("anterior", "posterior", "broadside"))
  
  # Conversions

    # temperatures C to K

      T_a <- T_a + 273.15   
      T_r <- T_r + 273.15   

    # dgrees to radians

      psi <- psi * pi / 180 
      r <- L / 2            
  
  # Calculations

    # Short wave heat transfer
  
      # Area of the limpet's shell (m^2) projected in the direction at which sunlight strikes the organism (Pennell and Deignan 1989)

        if (tan(psi) < r / H) {

          Ap <- pi * r^2 * cos(psi) + H * r * sin(psi) - pi * r^2 / 2 * cos(psi)

        } else {

          Ap <- pi * r^2 * cos(psi)

        }

      # short-wave absorptivity of the shell (the fraction of light energy that is absorbed) 

        alpha_sw <- 0.68
  
        q1 = Ap * alpha_sw * I
  
    # Long-wave energy transfer
  
      # View factor. (Campbell and Norman 1998) simulating limpets as a cone.
    
        Vs <- cos(psi) * r / sqrt(r^2 + H^2)
    
      # lateral area of a limpet shell (m^2)

        Al <- pi * r * sqrt(H^2 + r^2) 

      #  long-wave emissivity of the shell 

        eps_ws <- 0.97  

      # stefan-boltzmann constant (W m^-2 K^-4)

        sigma <- 5.67 * 10^-8

      # clear sky emissivity (Campbell and Norman 1998, 10.11)

        eps_ac <- 9.2 * 10^-6 * T_a^2 

      # emissivity of air with clouds (same as above, 10.12)

        eps_wa <- (1 - 0.84 * c) * eps_ac + 0.84 * c  

      q2 <- Vs * Al * eps_ws * sigma * T_a^4 * (eps_wa - 1)
      q3 <- 4 * Vs * Al* eps_ws * sigma * T_a^3
  

    # Convective heat transfer
  
      # conductivity of air (W m^-1 K^-1)

        Ka <- 0.00501 + 7.2 * 10^-5 * T_a       

      # kinematic viscosity of air (m^2 s^-1)

        v <- -1.25 * 10^-5 + 9.2 * 10^-8 * T_a  
  
      # Reynolds number

        Re <- u * L / v
  
      if (position == "anterior") {

        a <- 1.955
        b <- 0.371

      } else if (position == "posterior") {

        a <- 1.881
        b <- 0.376

      } else if (position == "broadside"){

        a <- 1.304
        b <- 0.404

      }
  
      # Nusselt number

        Nu <- a * Re^b    

      # Heat transfer coefficient (W m^-2 K^-1)

        hc <- a*Ka*((u/v)^b)*(L^(b-1)) 
  
      # area of the shell in convective contact with the air (m^2)

        A_cv <- Al  
        q4 <- hc * A_cv
  
  # Conductive heat transfer
  
    # area of conductive contact between the limpet's foot and the rock (m^2)

      A_cd <- pi * r^2  

    # thermal conductivity of rock (W m^-1 K^-1)

      Kr <- 3.06
      q5 <- A_cd * Kr
  
  
  T_b <- (q1 + q2 + (q3 + q4)* T_a + q5 * T_r) / (q3 + q4 + q5)
  
  T_b - 273.15
}
