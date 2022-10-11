#' @title Operative Environmental Temperature of a Limpet Based on a Model by Helmuth  
#'
#' @description The function predicts body temperatures (C, operative environmental temperatures) of a limpet. The function was provided by Brian Helmuth -- although radiation and convection are altered from his original model -- and based on \insertCite{Denny2006;textual}{TrenchR}.
#'
#' @param T_a \code{numeric} air temperature (C).
#'
#' @param T_r \code{numeric} rock surface temperature (C) in the sunlight.
#'
#' @param l \code{numeric} limpet length (anterior/posterior axis) (m).
#'
#' @param h \code{numeric} limpet height (dorsal/ventral axis) (m).
#'
#' @param S \code{numeric} solar irradiance (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#'
#' @param u \code{numeric} wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#'
#' @param s_aspect \code{numeric} solar aspect angle (degree), the angle between the limpet's length dimension and the vector to the Sun. Generally between 70 and 110 degrees.
#'
#' @param s_slope \code{numeric} solar elevation angle (degree), the altitude of the sun, which is the angle between the horizon and the sun.
#'
#' @param c \code{numeric} fraction of the sky covered by clouds.
#'
#' @return \code{numeric} predicted body (operative environmental) temperature (C).
#'
#' @family biophysical models
#'
#' @author Brian Helmuth et al.
#'
#' @details The original equation uses a finite-difference approach where they divide the rock into series of chunks, and calculate the temperature at each node to derive the conductive heat. For simplification, here it takes the rock temperature as a parameter, and conductive heat is calculated by the product of the area, thermal conductivity of rock and the difference in temperatures of the rock and the body.
#'   \cr \cr
#'   Limpets are simulated as cones following and using solar emissivity values from \insertCite{Campbell1998;textual}{TrenchR}.
#'   \cr \cr
#'   The area of the limpet's shell (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}) is projected in the direction at which sunlight strikes the organism \insertCite{Pennell1989;textual}{TrenchR}.
#'   \cr \cr
#'   Air conductivity values (\ifelse{html}{\out{W m<sup>-1</sup> K<sup>-1</sup>}}{\eqn{W m^-1 K^-1}{ASCII}}) are calculated following \insertCite{Denny2006;textual}{TrenchR}.
#'
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   Tb_limpetBH(T_a = 25,
#'               T_r = 30,
#'               l = 0.0176,
#'               h = 0.0122,
#'               S = 1300,
#'               u = 1,
#'               s_aspect = 90,
#'               s_slope = 60,
#'               c = 1)
#'
Tb_limpetBH <- function (T_a,
                         T_r,
                         l,
                         h,
                         S,
                         u,
                         s_aspect,
                         s_slope,
                         c) {

  stopifnot(l        >  0, 
            h        >  0, 
            S        >  0, 
            u        >= 0, 
            s_slope  >= 0, 
            s_slope  <= 90, 
            s_aspect >= 70, 
            s_aspect <= 110, 
            c        >= 0, 
            c        <= 1)

  # Conversions

    T_a <- celsius_to_kelvin(T_a)
    T_r <- celsius_to_kelvin(T_r)

    s_aspect <- degrees_to_radians(s_aspect)
    s_slope  <- degrees_to_radians(s_slope)
    r <- l / 2

  # Calculations

    # Adjust solar radiation for sun angles

      r_aspect <- 257* pi / 180
      r_slope <- 44.5* pi / 180

      delta_i <- cos(r_slope) * cos(s_slope) * cos(s_aspect - r_aspect) + sin(r_slope) * sin(s_slope)
      S <- S*delta_i

    # Short wave heat transfer

      # Area of the limpet's shell (m^2) projected in the direction at which sunlight strikes the organism (Pennell and Deignan 1989)

        Ap <- pi * r^2

      # short-wave absorptivity of the shell (the fraction of light energy that is absorbed)
      # Absorptivity from Luke Miller

        if (l >= 0.037) {

          alpha_sw <- 0.615

        } else if (l <= 0.02225) {

          alpha_sw <-0.689

        } else {

          alpha_sw <- 0.68

        }

        q1 = Ap * alpha_sw * S

    # Long-wave energy transfer

      # View factor simulating limpets as a cone (Campbell and Norman 1998).

        Vs <- 0.7

      # lateral area of a limpet shell (m^2)

        Al <- pi * r * sqrt(h^2 + r^2)

      #  long-wave emissivity of the shell

        eps_ws <- 0.97

        sigma <- stefan_boltzmann_constant()

      # clear sky emissivity (Campbell and Norman 1998, 10.11)

        eps_ac <- 9.2 * 10^-6 * T_a^2

      # emissivity of air with clouds (same as above, 10.12)

        eps_wa <- (1 - 0.84 * c) * eps_ac + 0.84 * c

      q2 <- Vs * Al * eps_ws * sigma * T_a^4 * (eps_wa - 1)
      q3 <- 4 * Vs * Al * eps_ws * sigma * T_a^3


    # Convective heat transfer

      # conductivity of air (W m^-1 K^-1)

        Ka <- 0.00501 + 7.2 * 10^-5 * T_a

      # kinematic viscosity of air (m^2 s^-1)

        v <- -1.25 * 10^-5 + 9.2 * 10^-8 * T_a

      # Reynolds number

        Re <- u * l / v

     # Absorptivity from Luke Miller

      if (l >= 0.037) {

        a <- 0.447
        b <- 0.516

      } else if (l <= 0.02225) {

        a <-0.1515
        b <- 0.6184

      } else  {

        a <- 0.1658
        b <- 0.6206

      }

      # Nusselt number

        Nu <- a * Re^b

      # Heat transfer coefficient (W m^-2 K^-1)

        hc <- a * Ka * ((u / v)^b) * (l^(b - 1))

      # area of the shell in convective contact with the air (m^2)

        A_cv <- Al
        q4 <- hc * A_cv

  # Conductive heat transfer

    # area of conductive contact between the limpet's foot and the rock (m^2)

      A_cd <- pi * r^2

    # thermal conductivity of rock (W m^-1 K^-1)

      Kr <- 3.06
      q5 <- A_cd * Kr


  T_b <- (q1 + q2 + (q3 + q4) * T_a + q5 * T_r) / (q3 + q4 + q5)

  kelvin_to_celsius(T_b)

}
