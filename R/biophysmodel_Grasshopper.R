#' @title Operative Environmental Temperature of a Grasshopper
#'
#' @description Predict body temperatures (operative environmental temperatures) of a grasshopper in C. Based on \insertCite{Swinbank1963;textual}{TrenchR}, using the regression in  \insertCite{Lactin1998;textual}{TrenchR} following \insertCite{Gates1962;textual}{TrenchR} in \insertCite{Kingsolver1983;textual}{TrenchR}.
#' 
#' @param T_a \code{numeric} air temperature in C.
#'
#' @param T_g  \code{numeric} surface temperature in C. \cr 
#'             \insertCite{Kingsolver1983;textual}{TrenchR} assumes \code{T_g - T_a = 8.4}.
#'
#' @param u \code{numeric} wind speed in m / s.
#'
#' @param H \code{numeric} total (direct + diffuse) solar radiation flux in \ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}.
#'
#' @param K_t \code{numeric} clearness index (dimensionless), which is the ratio of the global solar radiation measured at the surface to the total solar radiation at the top of the atmosphere.
#'
#' @param psi \code{numeric} solar zenith angle in degrees.
#'
#' @param l \code{numeric} grasshopper length in m.
#'
#' @param Acondfact \code{numeric} the proportion of the grasshopper surface area that is in contact with the ground.
#'
#' @param z \code{numeric} distance from the ground to the grasshopper in m
#'
#' @param abs \code{numeric} absorptivity of the grasshopper to solar radiation (proportion). See \insertCite{Anderson1979;textual}{TrenchR}.
#'
#' @param r_g \code{numeric} substrate solar reflectivity (proportion), see \insertCite{Kingsolver1983;textual}{TrenchR}.
#'
#' @return \code{numeric} predicted body (operative environmental) temperature (C).
#'
#' @details 
#'   Total radiative flux is calculated as thermal radiative heat flux plus convective heat flux, following \insertCite{Kingsolver1983;textual}{TrenchR}, with the \insertCite{Erbs1982;textual}{TrenchR} model from \insertCite{Wong2001;textual}{TrenchR}.
#'   \cr \cr
#'   Energy balance is based on \insertCite{Kingsolver1983;textual}{TrenchR}.
#'   \cr \cr
#'   W is calculated without area dependence \insertCite{Anderson1979}{TrenchR}.
#'   \cr \cr
#'   The body of a grasshopper female is approximated by a rotational ellipsoid with half the body length as the semi-major axis  \insertCite{Samietz2005}{TrenchR}.
#'   \cr \cr
#'   The diffuse fraction is corrected following \insertCite{Olyphant1984;textual}{TrenchR}.
#'
#' @family biophysical models
#'
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#'   Tb_grasshopper(T_a       = 25, 
#'                  T_g       = 25,      
#'                  u         = 0.4, 
#'                  H         = 400, 
#'                  K_t       = 0.7, 
#'                  psi       = 30, 
#'                  l         = 0.02, 
#'                  Acondfact = 0.25, 
#'                  z         = 0.001, 
#'                  abs       = 0.7, 
#'                  r_g       = 0.3)
#' 
Tb_grasshopper <- function (T_a, 
                            T_g, 
                            u, 
                            H, 
                            K_t, 
                            psi, 
                            l, 
                            Acondfact = 0.25, 
                            z         = 0.001, 
                            abs       = 0.7, 
                            r_g       = 0.3) {

  stopifnot(u >= 0, 
            H >= 0, 
            K_t >= 0, 
            K_t <= 1, 
            psi >= -90, 
            psi <= 90, 
            l >= 0, 
            Acondfact >= 0, 
            Acondfact <= 1, 
            z >= 0, 
            abs >= 0, 
            abs <= 1, 
            r_g >= 0, 
            r_g <= 1)
    
  # conversions

    # temperatures C to K

      T_aK <- T_a + 273.15
      T_g <- T_g + 273.15 

  # Butterfly Parameters

    # Stefan-Boltzmann constant (W m^-2 K^-4)

      omega <- 5.66 * 10^-8 

    # IR emissivity of surface to longwave 

      epsilon <- 1 

    # thermal conductivity of fluid
    #   Kf <- 0.024 + 0.00007 * T_a[k] 

      Kf <- 0.025  

    #  kinematic viscosity of air (m^2/s)
    #    http://users.wpi.edu/~ierardi/PDF/air_nu_plot.PDF
    #  m^2/s, kinematic viscosity of air,  at 300K 
    #    http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html

      v <- 15.68*10^-6  

  # Calculations

    # AREAS
    #   c: semi-major axis
    #   a: semi-minor axis

      c <- l / 2
      a <- (0.365 + 0.241 * l * 1000) / 1000
      e <- sqrt(1 - a^2 / c^2)
      A <- 2 * pi * a^2 + 2 * pi * a * c / e * asin(e)

    # SOLAR RADIATIVE HEAT FLUX   
    #   Separate total radiation into components
    #  Use Erbs et al model from Wong and Chow (2001, Applied Energy 69:1991-224)
    #  Anderson 1979 - calculates radiation as W without area dependence 
    #  kd - diffuse fraction
    #    if(K_t<= 0.22) 
    #    kd = 0.125 Correction from 16.5 for CO from Olyphant 1984

      kd <- 1 - 0.09 * K_t 
      kd[K_t > 0.22 & K_t <= 0.8] <- 0.9511 - 0.1604 * K_t + 4.388 * K_t^2 - 16.638 * K_t^3 + 12.336 * K_t^4
      kd[K_t > 0.8] <- 0.165 

      Httl <- H
      Hdir <- Httl * (1-kd)
      Hdif <- Httl * kd

      # psi in radians

      psi_r <- psi*pi/180 

    # Convection

      #   Reynolds number- ratio of interval viscous forces
      #   L: Characteristic dimension (length)
      #   u = windspeed 
      #   Lactin and Johnson add 1m/s to account for cooling by passive convection

        Re <- u * l / v

      # Nusselt number- dimensionless conductance
      #   Anderson 1979 empirical
      #   h_c: heat transfer coefficient, Wm^{-2}C^{-1} #reported in Lactin and Johnson 1998
      #   hc_s: heat transfer coefficient in turbulent air 

        Nu <- 0.41* Re^0.5 
        h_c <- Nu * Kf / l 
        hc_s <- h_c * (-0.007 * z / l + 1.71) 


    # Conduction 
    #   cuticle thickness (m)
    #   hcut: W m^-1 K^-1
    #   Qcond = hcut *Acond *(Tb- (T_a+273))/Thick
 
     Thick <- 6*10^(-5)
     hcut <- 0.15
     Acond <- A * Acondfact 


    # Energy balance 

      # Thermal radiative flux

      # silhouette area / total area
      # empirical from Anderson 1979, psi in degrees
  
        sa <- 0.19 - 0.00173 * psi 

        Adir <- A*sa
        Aref <- Adir 

    # Qabs as W

      Qdir <- abs * Adir * Hdir / cos(psi_r)
      Qdif <- abs * Aref * Hdif
      Qref <- r_g * Aref *Httl
      Qabs <- Qdir + Qdif + Qref  

   # black body sky temperature in Kelvin
   #   from Swinbank (1963), Kingsolver 1983 estimates using Brunt equation
 
    T_sky <- 0.0552 * (T_a + 273.15)^1.5
               

   #Qt = 0.5* A * epsilon * omega * (Tb^4 - Tsky^4) +0.5 * A * epsilon * omega * (Tb^4 - T_g^4) 
   #Convective heat flux
   #Qc = hc_s * A * (Tb- (T_a+273)) 
   #Qs = Qt+ Qc

  # Solution

    # WITHOUT CONDUCTION

      # a <- A * epsilon *omega
      # b <- hc_s * A
      # d <- hc_s * A * TaK + 0.5 * A * epsilon * omega * Tsky^4 +0.5 * A * epsilon * omega * T_g^4 + Qabs

      # eb <- function(Tb){0.5 * A * epsilon * omega * (Tb^4 - Tsky^4) + 0.5 * A * epsilon * omega * (Tb^4 - T_g^4) + hc_s * A * (Tb - TaK) + hcut * Acond * (Tb - T_g) / Thick - Qabs }
      # r <- uniroot(eb, c(-1, 373), tol = 1e-5)
      # r$root - 273


    # WITH CONDUCTION
    # t solved in wolfram alpha #Solve[a t^4 +b t -d, t]

      a <- A * epsilon * omega
      b <- hc_s * A + hcut * Acond / Thick
      d <- hc_s * A * T_aK +0.5 * A * epsilon * omega * (T_sky^4 + T_g^4)+ hcut * Acond * T_g / Thick + Qabs

   # in K

      tb <- 1 / 2 * sqrt((2 * b) / (a * sqrt((sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3) / (2^(1 / 3) * 3^(2 / 3) * a) - (4 * (2 / 3)^(1 / 3) * d) / (sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3))) - (sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3) / (2^(1 / 3) * 3^(2 / 3) * a) + (4 * (2 / 3)^(1 / 3) * d) / (sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3)) - 1 / 2 * sqrt((sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3) / (2^(1 / 3) * 3^(2 / 3) * a) - (4 * (2 / 3)^(1 / 3) * d) / (sqrt(3) * sqrt(256 * a^3 * d^3 + 27 * a^2 * b^4) + 9 * a * b^2)^(1 / 3)) 
      tb[which(is.na(tb))] <- NA

    # in C

      tb - 273.15

}
