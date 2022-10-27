#' @title Operative Environmental Temperature of an Ectotherm based on Campbell and Norman (1988)
#' 
#' @description The function estimates body temperatures (C, operative environmental temperature) of an ectotherm using an approximation based on \insertCite{Campbell1998;textual}{TrenchR} and \insertCite{Mitchell1976;textual}{TrenchR}.
#' 
#' @param T_a \code{numeric} air temperature (C).
#' 
#' @param T_g \code{numeric} ground temperature (C).
#' 
#' @param S \code{numeric} flux density of solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}), combining direct, diffuse, and reflected radiation accounting for view factors.
#' 
#' @param a_s \code{numeric} organismal solar absorptivity (proportion). 
#' 
#' @param a_l \code{numeric} organismal thermal absorptivity (proportion); 0.965 for lizards \insertCite{Bartlett1967}{TrenchR}.
#' 
#' @param epsilon \code{numeric} longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals \insertCite{Gates1980}{TrenchR}.
#' 
#' @param c_p \code{numeric} specific heat of air (\ifelse{html}{\out{J mol<sup>-1</sup> C<sup>-1</sup>}}{\eqn{J mol^-1 C^-1}{ASCII}}). 
#' 
#' @param D \code{numeric} characteristic dimension of the animal (m).
#' 
#' @param u \code{numeric} wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @details Boundary conductance uses a factor of 1.4 to account for increased convection \insertCite{Mitchell1976}{TrenchR}. The function assumes forced conduction.
#'
#' @return \code{numeric} operative environmental temperature, \code{T_e} (C).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples 
#' Tb_CampbellNorman (T_a     = 30, 
#'                    T_g     = 30, 
#'                    S       = 823, 
#'                    a_s = 0.7, 
#'                    a_l = 0.96, 
#'                    epsilon = 0.96, 
#'                    c_p     = 29.3, 
#'                    D       = 0.17, 
#'                    u       = 1)
#'
Tb_CampbellNorman <- function (T_a, 
                               T_g, 
                               S, 
                               a_s = 0.7, 
                               a_l = 0.96, 
                               epsilon = 0.96, 
                               c_p = 29.3, 
                               D, 
                               u) {
    
  stopifnot(T_a     >  -50, 
            T_a     <  100, 
            epsilon >= 0.5, 
            epsilon <= 1, 
            a_s >= 0, 
            a_l >= 0, 
            c_p     >= 0, 
            D       >  0, 
            u       >= 0)
  
  sigma <- stefan_boltzmann_constant()
  
  #convert temperatures to Kelvin
    T_a = T_a +273.15
    T_g = T_g +273.15
  
  # solar and thermal radiation absorbed
  # (10.7) long wave flux densities from atmosphere, ground 

    L_a <- sigma * T_a^4  
    L_g <- sigma * T_g^4 

  # proportion of organism exposure to air, ground

    F_a <- 0.5 
    F_g <- 0.5

  # (11.14) Absorbed radiation

    R_abs <- a_s * S + a_l * (F_a * L_a + F_g * L_g) 
  
  # thermal radiation emitted

    Qemit <- epsilon * sigma * T_a^4
  
  # conductance: boundary, ground
  #   boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976), assumes forced conduction

    g_Ha <- 1.4 * 0.135 * sqrt(u / D) 
    g_r <- 4 * epsilon * sigma * T_a^3 / c_p 
  
  # operative environmental temperature in C

    T_a + (R_abs - Qemit) / (c_p * (g_r + g_Ha)) -273.15
  
}

#' @title Net Energy Exchange Between an Animal and the Environment
#' 
#' @description The function estimates the net energy exchange (W) between an animal and the environment. The function follows \insertCite{Gates1980;textual}{TrenchR} and others.
#' 
#' @param Qabs \code{numeric} solar radiation absorbed (W).
#' 
#' @param Qemit \code{numeric} thermal radiation emitted (W).
#' 
#' @param Qconv \code{numeric} energy exchange due to convection; Energy exchange from an animal to its surrounding environment (air or water) (W).
#' 
#' @param Qcond \code{numeric} energy exchange due to conduction; Energy exchange from animal to a surface if they are in contact  (W).
#' 
#' @param Qmet \code{numeric} energy emitted due to metabolism (W).
#' 
#' @param Qevap \code{numeric} energy emitted due to evaporative water loss (W).
#' 
#' @return \code{numeric} net energy exchange (W).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples 
#' Qnet_Gates(Qabs  = 500,
#'            Qemit = 10, 
#'            Qconv = 100, 
#'            Qcond = 100, 
#'            Qmet  = 10, 
#'            Qevap = 5)
#' 
Qnet_Gates <- function (Qabs, 
                        Qemit, 
                        Qconv, 
                        Qcond, 
                        Qmet, 
                        Qevap) {
  
  stopifnot(Qabs  >= 0, 
            Qmet  >= 0)
  
  Qabs - Qemit - Qconv - Qcond - Qmet - Qevap
  
}


#' @title Operative Environmental Temperature of an Ectotherm Based on Gates (1980) 
#' 
#' @description The function predicts body temperatures (C, operative environmental temperature) of an ectotherm using the approximation in \insertCite{Gates1980;textual}{TrenchR}. The functions omits evaporative and metabolic heat loss \insertCite{Mitchell1976,Kingsolver1983}{TrenchR}.
#' 
#' @param A \code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @param D \code{numeric} characteristic dimension for conduction (meters).
#' 
#' @param psa_dir \code{numeric} view factor for the proportion surface area exposed to direct radiation from the sky (or enclosure) (0-1).
#' 
#' @param psa_ref \code{numeric} view factor for proportion surface area exposed to reflected radiation from the ground (0-1).
#' 
#' @param psa_air \code{numeric} proportion surface area exposed to air (0-1).
#' 
#' @param psa_g \code{numeric} proportion surface area in contact with substrate (0-1).
#' 
#' @param T_g \code{numeric} ground surface temperature (C).
#' 
#' @param T_a \code{numeric} ambient air temperature (C).
#' 
#' @param Qabs \code{numeric} Solar radiation absorbed (W).
#' 
#' @param epsilon \code{numeric} longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals \insertCite{Gates1980}{TrenchR}.
#' 
#' @param H_L \code{numeric} Convective heat transfer coefficient (\ifelse{html}{\out{W m<sup>-2</sup> K<sup>-1</sup>}}{\eqn{W m^-2 K^-1}{ASCII}}).
#' 
#' @param ef \code{numeric} enhancement factor used to adjust H_L to field conditions using h_L approximation from \insertCite{Mitchell1976;textual}{TrenchR}.  Approximated as 1.23 by default, but see \insertCite{Mitchell1976;textual}{TrenchR} for relationship.
#' 
#' @param K \code{numeric} Thermal conductivity (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}}), K = 0.5 \ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}} for naked skin, K = 0.15 \ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}}for insect cuticle \insertCite{Galushko2005;textual}{TrenchR}; conductivity of the ground is generally greater than that of animal tissues, so animal thermal conductivity is generally the rate limiting step. 
#' 
#' @return \code{numeric} operative environmental temperature, \code{T_e} (C).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples 
#'  Tb_Gates (A       = 0.1, 
#'            D       = 0.025, 
#'            psa_dir = 0.6, 
#'            psa_ref = 0.4, 
#'            psa_air = 0.5, 
#'            psa_g   = 0.1, 
#'            T_g     = 30, 
#'            T_a     = 37, 
#'            Qabs    = 2, 
#'            epsilon = 0.95, 
#'            H_L     = 10, 
#'            ef      = 1.23, 
#'            K       = 0.5)
#' 
Tb_Gates <- function (A, 
                      D, 
                      psa_dir, 
                      psa_ref, 
                      psa_air, 
                      psa_g, 
                      T_g, 
                      T_a, 
                      Qabs, 
                      epsilon, 
                      H_L, 
                      ef = 1.3, 
                      K) {

  stopifnot(A       >  0, 
            D       >  0, 
            psa_dir >= 0, 
            psa_dir <= 1, 
            psa_ref >= 0, 
            psa_ref <= 1, 
            psa_air >= 0, 
            psa_air <= 1, 
            psa_g   >= 0, 
            psa_g   <= 1, 
            T_g     >  -50, 
            T_g     <  100, 
            T_a     >  -50, 
            T_a     <  100, 
            Qabs    >= 0, 
            epsilon >  0.5, 
            epsilon <= 1, 
            H_L     >  0, 
            K       >  0)
  
  sigma <- stefan_boltzmann_constant()
  
  # Areas

    A_s <- A * psa_dir 
    A_r <- A * psa_ref 
  
    # skin area exposed to air

      A_air <- A * psa_air 

    # area of contact

      A_contact <- A * psa_g 
  
  # effective radiant temperature of sky
  #  K, Gates 1980 Biophysical ecology based on Swinback 1960, Kingsolver (1983) estimates using Brunt equation

    T_sky <- celsius_to_kelvin(1.22 * T_a - 20.4)
  
  #convert to Kelvin
    T_a= celsius_to_kelvin(T_a)
    T_g= celsius_to_kelvin(T_g)
    
  # energy balance function for steady state conditions
  #  0 = Qabs - Qemit - Qconv - Qcond

    Qfn <- function(T_b, Qabs, epsilon, sigma, A_s, T_sky, A_r, T_g, H_L, A_air, T_a, A_contact, K, D) {

      Qemit <- epsilon * sigma * (A_s * (T_b^4 - T_sky^4) + A_r * (T_b^4 - T_g^4))
      Qconv <- ef * H_L * A_air * (T_b - T_a) 
      Qcond <- A_contact * K * (T_b - T_g) / D 

      Qabs - Qemit - Qconv - Qcond
    
    }
  
  T_e <- tryCatch(expr = uniroot(f         = Qfn, 
                                 interval  = c(273, 350), 
                                 Qabs      = Qabs, 
                                 epsilon   = epsilon, 
                                 sigma     = sigma, 
                                 A_s       = A_s, 
                                 T_sky     = T_sky, 
                                 A_r       = A_r, 
                                 T_g       = T_g, 
                                 H_L       = H_L, 
                                 A_air     = A_air, 
                                 T_a       = T_a, 
                                 A_contact = A_contact, 
                                 K         = K, 
                                 D         = D, 
                                 tol       = 0.0001)$root, 
                 
                  error = function(e) {
                            message("Unable to balance energy budget. One issue to check is whether absorbed solar radiation exceeds energy potentially lost to thermal radiation, convection, and conduction.")
                            NA})
  
  kelvin_to_celsius(T_e)

}

#' @title Operative Environmental Temperature of an Ectotherm Based on a Variant of Gates (1980) 
#' 
#' @description The function predicts body temperatures (C, operative environmental temperature) of an ectotherm using the approximation in \insertCite{Gates1980;textual}{TrenchR}. The function omits evaporative and metabolic heat loss.
#' 
#' @param A \code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @param D \code{numeric} characteristic dimension for conduction (meters).
#' 
#' @param T_g \code{numeric} ground surface temperature (C).
#' 
#' @param T_a \code{numeric} ambient air temperature (C).
#' 
#' @param Qabs \code{numeric} Solar radiation absorbed (W).
#' 
#' @param u \code{numeric} Wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param epsilon \code{numeric} longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals \insertCite{Gates1980}{TrenchR}.
#' 
#' @return \code{numeric} operative environmental temperature (C). 
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples 
#'   Tb_Gates2(A       = 1, 
#'             D       = 0.001, 
#'             T_g     = 27, 
#'             T_a     = 37, 
#'             Qabs    = 2, 
#'             u       = 0.1, 
#'             epsilon = 1) 
#' 
Tb_Gates2 <- function (A, 
                       D, 
                       T_g, 
                       T_a, 
                       Qabs, 
                       u, 
                       epsilon) {
  
  sigma <- stefan_boltzmann_constant()

  stopifnot(A       >  0, 
            D       >  0, 
            T_g     >  -50, 
            T_g     <  100, 
            T_a     >  -50, 
            T_a     <  100, 
            Qabs    >= 0, 
            epsilon >  0.5, 
            epsilon <= 1, 
            u       >  0)


  # skin area exposed to air

    A_air <- A
  
  # Convection coefficient from Gates (1980)

    H_L <- 3.49 * (u^0.5 / D^0.5)
  
  # effective radiant temperature of sky
  #  K, Gates 1980 Biophysical ecology based on Swinback 1960, Kingsolver (1983) estimates using Brunt equation

    T_sky <- celsius_to_kelvin(1.22 * (T_a - 20.4))
    
    #convert to Kelvin
    T_a= celsius_to_kelvin(T_a)
    T_g= celsius_to_kelvin(T_g)
    
  # Thermal radiation absorbed 

    QIR <- (A_air / 2) * (sigma * epsilon * T_g^4 + sigma * epsilon * T_sky^4)
  
  # energy balance function for steady state conditions
  #   0 = Qabs - Qemit - Qconv - Qcond

    Qfn <- function(T_b, Qabs, QIR, epsilon, sigma, T_sky, H_L, A_air, T_a) {
    
      Qemit <- A_air * sigma * epsilon * T_b^4
      Qconv <- H_L * A_air * (T_b - T_a)
    
      Qabs + QIR - Qemit - Qconv

    }
  
  T_e <- tryCatch(expr = uniroot(f        = Qfn,
                                 interval = c(273, 353),
                                 Qabs     = Qabs, 
                                 QIR      = QIR, 
                                 epsilon  = epsilon, 
                                 sigma    = sigma, 
                                 T_sky    = T_sky, 
                                 H_L      = H_L, 
                                 A_air    = A_air, 
                                 T_a      = T_a, 
                                 tol      = 0.0001)$root, 
                  error = function(e) {
                            message("Unable to balance energy budget. One issue to check is whether absorbed solar radiation exceeds energy potentially lost to thermal radiation, convection, and conduction.")
                            NA})
  
  kelvin_to_celsius(T_e)
  
}

