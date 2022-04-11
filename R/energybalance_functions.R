#' @title Conductance Assuming Animal Thermal Conductivity is Rate Limiting
#' 
#' @description Calculate conductance (W) of an ectothermic animal to its substrate. Method assumes the major resistance to conduction is within surface layers of the animal and that the interior of the animal is equal in temperature to its surface (thermally well mixed) \insertCite{Spotila1992}{TrenchR}.
#' 
#' @param T_g \code{numeric} Ground surface temperature (Kelvin).
#' 
#' @param T_b \code{numeric} Body temperature (Kelvin).
#' 
#' @param d \code{numeric} Mean thickness of the animal skin (surface) in (meters), assumes well mixed interior.
#' 
#' @param K \code{numeric} Thermal conductivity (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}}), \code{K = 0.5} for naked skin, \code{K = 0.15} for insect cuticle \insertCite{Galushko2005}{TrenchR}; conductivity of ground is generally greater than that of animal tissues, so animal thermal conductivity is generally rate limiting step.
#' 
#' @param A \code{numeric} Surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @param proportion \code{numeric} proportion of body in contact with the surface (0-1).
#' 
#' @return \code{numeric} conductance (W).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Qconduction_animal(T_g        = 293,
#'                      T_b        = 303, 
#'                      d          = 10^-6, 
#'                      K          = 0.5, 
#'                      A          = 10^-3, 
#'                      proportion = 0.2)
#' 

Qconduction_animal <- function (T_g, 
                                T_b, 
                                d, 
                                K = 0.5, 
                                A,
                                proportion) {
 
  stopifnot(T_g > 200, T_g < 400, T_b > 200, T_b < 400, d >= 0, K > 0, A > 0, proportion >= 0, proportion <= 1)
  
  # Calculate the area of contact
  A_contact <- A * proportion
  
  # Conduction
  # Calculating the heat loss (Difference between animal temperature and its environment)
  # m^2 * W K^-1 m^-1 * K / m
  A_contact * K * (T_b - T_g) / d

}

#' @title Conductance Assuming Substrate Thermal Conductivity is Rate Limiting
#' 
#' @description Calculate conductance (W) of an ectothermic animal to its substrate. Method assumes the major resistance to conduction is the substrate and that the interior of the animal is equal in temperature to its surface (thermally well mixed) \insertCite{Spotila1992}{TrenchR}.
#' 
#' @param T_g \code{numeric} surface temperature (K).
#' 
#' @param T_b \code{numeric} body temperature (K).
#' 
#' @param D \code{numeric} characteristic dimension of the animal (m).
#' 
#' @param K_g \code{numeric} thermal conductivity of substrate (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}}).
#' 
#' @param A \code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @param proportion \code{numeric} proportion in contact to the surface.
#' 
#' @return \code{numeric} conductance (W).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Qconduction_substrate(T_g        = 293, 
#'                         T_b        = 303, 
#'                         D          = 0.01,
#'                         K_g        = 0.3, 
#'                         A          = 10^-2, 
#'                         proportion = 0.2)
#' 
#' 
Qconduction_substrate <- function (T_g, 
                                   T_b, 
                                   D, 
                                   K_g = 0.5, 
                                   A, 
                                   proportion) {
  
  stopifnot(T_g > 200, T_g < 400, T_b > 200, T_b < 400, D >= 0, K_g > 0, A > 0, proportion >= 0, proportion <= 1)
  
  # Calculate the area of contact
  A_contact <- A * proportion
  
  #Thermal conductance between animal and substrate
  H_g <- 2.0 * K_g / D
  
  # Conduction,  m^2 * W K^-1 m^-1 * K / m
  A_contact * H_g * (T_b - T_g)

}


#' @title Calculate Convection
#' 
#' @description Calculate convection from an organism to its environment as in \insertCite{Mitchell1976}{TrenchR}. Includes an enhancement factor associated with outdoor environments. 
#' 
#' @param T_a \code{numeric} air temperature (K).
#' 
#' @param T_b \code{numeric} initial body temperature (K).
#' 
#' @param H_L \code{numeric} convective heat transfer coefficient (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-2</sup>}}{\eqn{W K^-1 m^-2}{ASCII}}).
#' 
#' @param A \code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @param proportion \code{numeric} proportion of surface area exposed to air.
#' 
#' @param ef \code{numeric} is the enhancement factor, used to adjust H to field conditions.  Approximated as mean value of 1.23 by default, but see \insertCite{Mitchell1976}{TrenchR} for further information.
#' 
#' @return \code{numeric} convection (W).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Qconvection(T_a        = 293, 
#'               T_b        = 303, 
#'               H_L        = 10.45, 
#'               A          = 0.0025, 
#'               proportion = 0.85)
#' 
#' 
Qconvection <- function (T_a, 
                         T_b, 
                         A, 
                         proportion, 
                         H_L         = 10.45,
                         ef          = 1.23) {
  
  stopifnot(T_a > 200, T_a < 400, T_b > 200, T_b < 400, H_L > 0, A > 0, proportion >= 0, proportion <= 1, ef >= 1)
  
  # Calculate skin area exposed to air
  A_air <- A * proportion
 
  ef * H_L * A_air * (T_b - T_a)

}


#' @title Heat Transfer Coefficient
#' 
#' @description Estimate the heat transfer coefficient for various taxa based on empirical measurements \insertCite{Mitchell1976}{TrenchR}.
#' 
#' @param V \code{numeric} air velocity (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param D \code{numeric} Characteristic dimension (e.g., diameter or snout-vent length) (meters).
#' 
#' @param K \code{numeric} Thermal conductivity of air (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}}), can calculate using \code{\link{DRYAIR}} or \code{\link{WETAIR}}.
#' 
#' @param nu \code{numeric} Kinematic viscosity of air (\ifelse{html}{\out{m<sup>2</sup> s<sup>-1</sup>}}{\eqn{m^2 s^-1}{ASCII}}), can calculate using \code{\link{DRYAIR}} or \code{\link{WETAIR}}.
#' 
#' @param taxa \code{character} Which class of organism, current choices: sphere, cylinder, frog, lizard_surface, lizard_elevated, flyinginsect, spider. Notes: Cylinder assumes 40<Re<4000. lizard assumes prostrate on or elevated above surface, average for parallel and perpendicular to air flow.
#' 
#' @return \code{numeric} heat transfer coefficient, \code{H_L} (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-2</sup>}}{\eqn{W K^-1 m^-2}{ASCII}}).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   heat_transfer_coefficient(V    = 0.5, 
#'                             D    = 0.05, 
#'                             K    = 25.7 * 10^(-3), 
#'                             nu   = 15.3 * 10^(-6), 
#'                             taxa = "cylinder")
#' 
heat_transfer_coefficient <- function (V, 
                                       D, 
                                       K, 
                                       nu, 
                                       taxa = "cylinder") {
  
  stopifnot(V >= 0, D >= 0, K >= 0, nu >= 0, taxa %in% c("sphere", "cylinder", "frog", "lizard_surface", "lizard_elevated", "flyinginsect", "spider"))
  
  taxas <- c("sphere", "cylinder", "frog", "lizard_surface", "lizard_elevated", "flyinginsect", "spider")
  
  # Dimensionless constant (Cl)
  Cls <- c(0.37, 0.615, 0.258, 1.36, 1.91, 0.0749, 0.47)
  ns <- c(0.6, 0.466, 0.667, 0.39, 0.45, 0.78, 0.5) 
  
  # Find index  
  ind <- match(taxa, taxas)
  
  Re <- V * D / nu # Reynolds number 
  Nu <- Cls[ind] * Re^ns[ind]  # Nusselt number
  
  Nu * K / D

}


#' @title Calculate Heat Transfer Coefficient Using a Spherical Approximation
#' 
#' @description  estimate the heat transfer coefficient for various taxa.  Approximates forced convective heat transfer for animal shapes using the convective relationship for a sphere \insertCite{Mitchell1976}{TrenchR}.
#' 
#' @param V \code{numeric} air velocity (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param D \code{numeric} characteristic dimension (e.g., diameter or snout-vent length) (meters).
#' 
#' @param K \code{numeric} thermal conductivity of air (\ifelse{html}{\out{W m<sup>-2</sup> K<sup>-1</sup>}}{\eqn{W m^-2 K^-1}{ASCII}}), can calculate using \code{\link{DRYAIR}} or \code{\link{WETAIR}}.
#' 
#' @param nu \code{numeric} kinematic Viscosity of air (\ifelse{html}{\out{m<sup>2</sup> s<sup>-1</sup>}}{\eqn{m^2 s^-1}{ASCII}}), can calculate using \code{\link{DRYAIR}} or \code{\link{WETAIR}}.
#' 
#' @param taxa \code{character} which class of organism, current choices: sphere, frog, lizard, flyinginsect, spider
#' 
#' @return \code{numeric} heat transfer coefficient, \code{H_L} (\ifelse{html}{\out{W m<sup>-2</sup> K<sup>-1</sup>}}{\eqn{W m^-2 K^-1}{ASCII}}).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'  heat_transfer_coefficient_approximation(V    = 3, 
#'                                          D    = 0.05, 
#'                                          K    = 25.7 * 10^(-3), 
#'                                          nu   = 15.3 * 10^(-6), 
#'                                          taxa = "sphere")
#' 
heat_transfer_coefficient_approximation <- function (V, 
                                                     D, 
                                                     K, 
                                                     nu, 
                                                     taxa = "sphere") {
  
  stopifnot(V >= 0, D >= 0, K >= 0, nu >= 0, taxa %in% c("sphere", "frog", "lizard", "flyinginsect", "spider"))
  
  taxas <- c("sphere", "frog", "lizard", "flyinginsect", "spider")
 
  # Dimensionless constant (Cl)
  Cls <- c(0.34, 0.196, 0.56, 0.0714, 0.52)
  ns <- c(0.6, 0.667, 0.6, 0.78, 0.5) 
  
  # Find index  
  ind <- match(taxa, taxas)
  
  Re <- V * D / nu # Reynolds number 
  Nu <- Cls[ind] * Re^ns[ind]  # Nusselt number
  
  Nu * K / D

}


#' @title Calculate Heat Transfer Coefficient
#' 
#' @description estimate the heat transfer coefficient \insertCite{Mitchell1976}{TrenchR} using either the relationship in \insertCite{Spotila1992}{TrenchR} or that in \insertCite{Gates1980}{TrenchR}.
#' 
#' @param V \code{numeric} air velocity (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param D \code{numeric} characteristic dimension (e.g., diameter or snout-vent length) (m).
#' 
#' @param type \code{character} choice between "Spotila" and "Gates" for equation to use.
#' 
#' @return \code{numeric} heat transfer coefficient, H_L (\ifelse{html}{\out{W m<sup>-2</sup> K<sup>-1</sup>}}{\eqn{W m^-2 K^-1}{ASCII}}).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   heat_transfer_coefficient_simple(V    = 0.5, 
#'                                    D    = 0.05, 
#'                                    type = "Gates")
#' 
heat_transfer_coefficient_simple <- function(V, 
                                             D, 
                                             type) {
  
  stopifnot(V >= 0, D >= 0)
  
  if(type == "Spotila") {
    
    H_L <- 6.77 * V^0.6 * D^(-0.4)
    
  }

  if(type == "Gates") {
    
    H_L <- 3.49 * V^0.5 * D^(-0.5)
    
  }

  H_L
  
}

#' @title Calculate Absorbed Solar and Thermal Radiation
#' 
#' @description  estimate solar and thermal radiation (W) absorbed by the surface of an animal. Follows \insertCite{Gates1980}{TrenchR} and \insertCite{Spotila1992}{TrenchR}.
#' 
#' @param a \code{numeric} solar absorptivity of animal surface (proportion), default value is for reptiles (0-1).
#' 
#' @param A \code{numeric} surface area ((\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @param psa_dir \code{numeric} proportion surface area exposed to solar radiation (0-1).
#' 
#' @param psa_ref \code{numeric} proportion surface area exposed to reflected solar radiation (0-1).
#' 
#' @param S_dir \code{numeric} direct solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#' 
#' @param S_dif \code{numeric} diffuse solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}).
#' 
#' @param S_ref \code{numeric} reflected solar radiation (\ifelse{html}{\out{W m<sup>-2</sup>}}{\eqn{W m^-2}{ASCII}}), either provided or estimated if surface albedo is provided instead
#' 
#' @param a_s \code{numeric} is surface albedo (proportion), optional (not used) if reflected radiation is provided. Values available in \insertCite{Gates1980}{TrenchR} Table 8.2.
#' 
#' @return \code{numeric} solar radiation absorbed (W)
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Qradiation_absorbed(a       = 0.9, 
#'                       A       = 1, 
#'                       psa_dir = 0.4, 
#'                       psa_ref = 0.4, 
#'                       S_dir   = 1000, 
#'                       S_dif   = 200, 
#'                       a_s     = 0.5)
#'
Qradiation_absorbed <- function (a = 0.9,
                                 A, 
                                 psa_dir, 
                                 psa_ref, 
                                 S_dir, 
                                 S_dif, 
                                 S_ref = NA, 
                                 a_s   = NA) {

  stopifnot(a >= 0, a <= 1, A > 0, psa_dir >= 0, psa_dir <= 1, psa_ref >= 0, psa_ref <= 1, S_dir > 0, S_dif > 0)  
  
  #Calculate S_ref if not provided
  if(is.na(S_ref)) {
    
    S_ref <- a_s * S_dir
    
  }
  
  # Areas
  A_dir <- A * psa_dir
  A_dif <- A_dir
  A_ref <- A * psa_ref
  
  # Solar radiation
  a * A_dir * S_dir + a * A_dif * S_dif + a * A_ref * S_ref

}


#' @title Calculate Emitted Thermal Radiation
#' 
#' @description estimate thermal radiation (W) emitted by the surface of an animal \insertCite{Gates1980}{TrenchR} and \insertCite{Spotila1992}{TrenchR}.
#' 
#' @param epsilon \code{numeric} longwave infrared emissivity of skin (proportion), 0.95 to 1 for most animals \insertCite{Gates1980}{TrenchR}.
#' 
#' @param A \code{numeric} surface area ((\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @param psa_dir \code{numeric} proportion surface area exposed to sky (or enclosure) (0-1)
#' 
#' @param psa_ref \code{numeric} proportion surface area exposed to ground (0-1).
#' 
#' @param T_b \code{numeric} body surface temperature (K).
#' 
#' @param T_g \code{numeric} ground surface temperature (K).
#' 
#' @param T_a \code{numeric} ambient air temperature (K), only required if animal is in enclosed environment.
#' 
#' @param enclosed \code{logical} if the animal is an enclosed environment or not.
#' 
#' @return \code{numeric} emitted thermal radiation, Qemit (W)
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Qemitted_thermal_radiation(epsilon  = 0.96, 
#'                              A        = 1, 
#'                              psa_dir  = 0.4, 
#'                              psa_ref  = 0.6, 
#'                              T_b      = 303, 
#'                              T_g      = 293, 
#'                              T_a      = 298, 
#'                              enclosed = FALSE)
#' 
Qemitted_thermal_radiation <- function (epsilon  = 0.96, 
                                        A, 
                                        psa_dir, 
                                        psa_ref, 
                                        T_b, 
                                        T_g, 
                                        T_a, 
                                        enclosed = FALSE){
  
  stopifnot(epsilon >= 0, epsilon <= 1, A > 0, psa_dir >= 0, psa_dir <= 1, psa_ref >= 0, psa_ref <= 1, T_b > 200, T_b < 400, T_g > 200, T_g < 400, T_a > 200, T_a < 400, enclosed %in% c(TRUE, FALSE))
  
  # Stefan-Boltzmann constant
  sigma <- 5.673 * 10^(-8) # W m^(-2) K^(-4)
  
  # Areas
  A_s <- A * psa_dir
  A_r <- A * psa_ref
  
  # Estimate effective radiant temperature of sky
  Tsky <- (1.22 * (T_a - 273.15) - 20.4) + 273.15 # K
  
  if(enclosed){
    
    Qemit <- A_r * epsilon * sigma * (T_b^4 - T_a^4)
    
  } else {
    
    Qemit <- epsilon * sigma * (A_s * (T_b^4 - Tsky^4) + A_r * (T_b^4 - T_g^4))
    
  }

  Qemit
  
}


#' @title Calculate Heat Loss Associated with Evaporative Water Loss
#' 
#' @description  estimate heat loss associated with evaporative water loss by an amphibian \insertCite{Spotila1992}{TrenchR} or lizard (based on empirical measurements in \insertCite{Porter1973}{TrenchR}).
#' 
#' @param A \code{numeric} surface area (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}).
#' 
#' @param T_b \code{numeric} body temperature (K).
#' 
#' @param taxa \code{character} taxa current choices: lizard, amphibian_wetskin (fully wet skin), amphibian (not fully wet skin).
#' 
#' @param rho_s \code{numeric} saturation water vapor density at skin surface (\ifelse{html}{\out{kg m<sup>-3</sup>}}{\eqn{kg m^-3}{ASCII}}) (needed if amphibian).
#' 
#' @param rho_a \code{numeric} saturation water vapor density in ambient air (\ifelse{html}{\out{kg m<sup>-3</sup>}}{\eqn{kg m^-3}{ASCII}}) (needed if amphibian).
#' 
#' @param h \code{numeric} relative humidity (0-1) (needed if amphibian).
#' 
#' @param H \code{numeric} convective heat transfer coefficient (\ifelse{html}{\out{W m<sup>-2</sup> K<sup>-1</sup>}}{\eqn{W m^-2 K^-1}{ASCII}}) (needed if amphibian).
#' 
#' @param r_i \code{numeric} internal (cutaneous) resistance to vapor transport (\ifelse{html}{\out{s m<sup>-1</sup>}}{\eqn{s m^-1}{ASCII}}) (needed if amphibian).
#' 
#' @return \code{numeric} evaporative heat loss (W).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Qevaporation(A     = 0.1, 
#'                T_b   = 293, 
#'                taxa  = "amphibian", 
#'                rho_s = 0.003, 
#'                rho_a = 0.002, 
#'                h     = 0.5, 
#'                H     = 20, 
#'                r_i   = 50)
#'   Qevaporation(A    = 0.1,
#'                T_b  = 293, 
#'                taxa = "lizard")
#' 
Qevaporation <- function (A, 
                          T_b, 
                          taxa, 
                          rho_s = NA, 
                          rho_a = NA, 
                          h     = NA, 
                          H     = NA, 
                          r_i   = NA) {
  
  stopifnot(A > 0, T_b > 200, T_b < 400, taxa %in% c("lizard", "amphibian_wetskin", "amphibian"))

  if(taxa %in% c("amphibian_wetskin", "amphibian")) {
    
    stopifnot(rho_s > 0, rho_a > 0, h >= 0, h <= 1, H > 0, r_i > 0) 
    
  }
                                                            
  # Porter et al. 1973 in Gates Biophysical ecology
  if(taxa == "lizard") { 
    if(T_b < 293) {
      
      E_kg <- 0.27
      
    }
    
    if(T_b >= 293 & T_b <= 309) {
      
      E_kg <- 0.08 * exp(0.586) * (T_b - 273.5)
      
    }
    
    if(T_b > 309) {
      
      E_kg <- 2.97 * 10^(-3) * exp(0.1516) * (T_b - 273.5) 
      
    }
    
    # convert from W/kg to W/m2
    E <- E_kg * 0.067 / 0.018 #for 0.067kg lizard with 0.018m^2 surface area
    
    # multiply by surface area
    Qevap <- E * A
    
  }

  # Spotila et al. 1992
  evap_heat <- 2.44 * 10^(6) # J/kg at most temperatures
  
  rhocp <- 1200 # J*m^(-3)*K^(-1)  
  # external (convective) resistance to water vapor transport (s/m), Lewis rule
  r_e <- 0.93 * rhocp / H
  
  if(taxa == "amphibian_wetskin") { 
  
    Ec <- A * (rho_s - h * rho_a) / r_e # rate of water transport (kg/s)
    Qevap <- Ec * evap_heat #to W
    
  }
  
  if(taxa == "amphibian") { 
    
    Ec= A * (rho_s-h*rho_a)/(r_i+r_e) # rate of water transport (kg/s)
    Qevap= Ec*evap_heat # to W
    
  }  
    
  Qevap
  
}


#' @title Approximate Saturation Water Vapor Pressure
#' 
#' @description Approximate saturation water vapor pressure as a function of ambient temperature for temperatures from 0 to 40C using Rosenberg 1974 in \insertCite{Spotila1992}{TrenchR}. See also NichMapR WETAIR and DRYAIR functions \insertCite{Kearney2020}{TrenchR}.
#' 
#' @param T_a \code{numeric} air temperature (C).
#' 
#' @return \code{numeric} Saturation water vapor pressure, \code{e_s} (Pa).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   saturation_water_vapor_pressure(T_a = 20)
#' 
saturation_water_vapor_pressure <- function (T_a) {
  
  10^(0.02604 * T_a + 2.82488)

}


#' @title Calculate External Resistance to Water Vapor Transfer
#'
#' @description  estimate external resistance to water vapor transfer using the Lewis rule relating heat and mass transport \insertCite{Spotila1992}{TrenchR}
#' 
#' @param H \code{numeric} heat transfer (convection) coefficient (\ifelse{html}{\out{W m<sup>-2</sup> C<sup>-1</sup>}}{\eqn{W m^-2 C^-1}{ASCII}}).
#' 
#' @param rhocp \code{numeric} aggregate parameter (\ifelse{html}{\out{J m<sup>-3</sup> C<sup>-1</sup>}}{\eqn{J m^-3 C^-1}{ASCII}}) that is the product of the density of air (\ifelse{html}{\out{kg m<sup>-3</sup>}}{\eqn{kg m^-3}{ASCII}}) and the specific heat of air at constant pressure (\ifelse{html}{\out{J kg<sup>-1</sup> C<sup>-1</sup>}}{\eqn{J kg^-1 C^-1}{ASCII}}). Default of 12000 (\ifelse{html}{\out{J m<sup>-3</sup> C<sup>-1</sup>}}{\eqn{J m^-3 C^-1}{ASCII}}) commonly assumed.
#' 
#' @return \code{numeric} external resistance to water vapor transfer (\ifelse{html}{\out{s m<sup>-1</sup>}}{\eqn{s m^-1}{ASCII}}).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   external_resistance_to_water_vapor_transfer(H = 20)
#' 
external_resistance_to_water_vapor_transfer <- function (H, 
                                                         rhocp = 12000) {
 
  stopifnot(H > 0)
  
  0.93 * rhocp / H

}


#' @title Calculate Metabolism as a Function of Mass
#' 
#' @description Estimate field metabolic rate (W) of various taxa as a function of mass(g). Does not account for temperature. Uses empirical relationships from \insertCite{Nagy2005}{TrenchR}.
#' 
#' @param m \code{numeric} mass (grams).
#' 
#' @param taxa \code{character} taxa to use in calculate. Options: reptile, bird, mammal.
#' 
#' @return \code{numeric} metabolism (W).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Qmetabolism_from_mass(m    = 12,
#'                         taxa = "reptile")
#' 
Qmetabolism_from_mass <- function(m, 
                                  taxa = "reptile") {
  
  stopifnot(m > 0, taxa %in% c("reptile", "bird", "mammal"))
  
  # FMR in W, M is mass in grams
  # Convert 1 kJ/day = 0.0115741 W
  
  # Reptile
  if(taxa == "reptile") {
    
    Qmet <- 0.196 * m^0.889 * 0.0115741
    
  }
  
  # Mammal
  if(taxa == "mammal") {
    
    Qmet <- 4.82 * m^0.734 * 0.0115741
    
  }
  
  # Bird
  if(taxa == "bird") {
    
    Qmet <- 10.5 * m^0.681 * 0.0115741
    
  }
  
  Qmet
  
}


#' @title Calculate Basal (Resting) Metabolism as a Function of Mass and Body Temperature
#' 
#' @description  Estimate basal (or resting) metabolic rate (W) as a function of mass (g) and temperature (K). Based on empirical data and the metabolic theory of ecology (3/4 scaling exponent) \insertCite{Gillooly2001}{TrenchR}.
#' 
#' @param m \code{numeric} mass (grams).
#' 
#' @param T_b \code{numeric} body temperature (K).
#' 
#' @param taxa \code{character} taxa to use. Options: bird, mammal, reptile, amphibian, invertebrate.
#' 
#' @return \code{numeric} basal metabolism (W).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples 
#'   Qmetabolism_from_mass_temp(m    = 100, 
#'                              T_b  = 303, 
#'                              taxa = "reptile")
#' 
Qmetabolism_from_mass_temp <- function (m, 
                                        T_b, 
                                        taxa) {
  
  stopifnot(m > 0, T_b > 200, T_b < 400, taxa %in% c("bird", "mammal", "reptile", "amphibian", "invertebrate"))
  
  if(taxa=="bird" | taxa=="mammal") {
    
    Qmet <- exp(-9100 / T_b + 29.49) * m^0.75 / 60
    
  }
  
  if(taxa=="reptile") {
    
    Qmet <- exp(-8780 / T_b + 26.85) * m^0.75 / 60
    
  }
  
  if(taxa=="amphibian") {
    
    Qmet <- exp(-5760 / T_b + 16.68) * m^0.75 / 60
    
  }
  
  if(taxa=="invertebrate") {
    
    Qmet <- exp(-9150 / T_b + 27.62) * m^0.75 / 60
    
  }
  
  Qmet
  
}


#' @title Calculate Actual Vapor Pressure from Dewpoint Temperature
#'
#' @description Calculate actual vapor pressure from dewpoint temperature based on \insertCite{Stull2000,Riddell2018;textual}{TrenchR}.
#' 
#' @param Tdewpoint \code{numeric} dewpoint temperature (C).
#' 
#' @return \code{numeric} actual vapor pressure (kPa).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Eric Riddell
#' 
#' @examples
#'   actual_vapor_pressure(Tdewpoint = 20)
#' 
actual_vapor_pressure <- function (Tdewpoint) {
  
  0.611 * (2.71828182845904^(((1.0 / 273.0) - (1.0 / (Tdewpoint + 273.15))) * 5422.9939))

}


#' @title Calculate Saturation Vapor Pressure
#'
#' @description Calculate saturation vapor pressure (kPa) based on the Clausius-Clapeyron equation \insertCite{Stull2000,Riddell2018}{TrenchR}.
#' 
#' @param T_a \code{numeric} air temperature (K).
#' 
#' @return \code{numeric} saturation vapor pressure, \code{e_s} (kPa).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Eric Riddell
#' 
#' @examples
#'   saturation_vapor_pressure(T_a = 293)
#' 
saturation_vapor_pressure <- function (T_a) {
  
  stopifnot(T_a > 200, T_a < 400)
  
  Rv <- 461.5     # J*K^-1*kg^-1, ideal gas constant for water vapor
  L <- 2.5 * 10^6 # J per kg, latent heat of vaporization
  e_o <- 0.611    # kPa
  
  e_o * exp((L / Rv) * ((1. / 273.15) - (1. / T_a))) 

}


#' @title Estimate the Boundary Layer Resistance
#' 
#' @description Estimate boundary layer resistance under free convection. Based on the function in \insertCite{Riddell2018}{TrenchR}.
#' 
#' @param T_a \code{numeric} air temperature (K).
#' 
#' @param e_s \code{numeric} saturation vapor pressure (kPa).
#' 
#' @param e_a \code{numeric} actual vapor pressure (kPa).
#' 
#' @param elev \code{numeric} elevation (m).
#' 
#' @param D \code{numeric} characteristic dimension (e.g., body diameter) (m).
#' 
#' @param u \code{numeric} wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}), if not provided assume free convection; if provided, use forced convection if appropriate.
#' 
#' @return \code{numeric} boundary layer resistance (\ifelse{html}{\out{s cm<sup>-1</sup>}}{\eqn{s cm^-1}{ASCII}}). 
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Eric Riddell
#' 
#' @examples
#'   boundary_layer_resistance(T_a  = 293, 
#'                             e_s  = 2.5, 
#'                             e_a  = 2.0, 
#'                             elev = 500, 
#'                             D    = 0.007, 
#'                             u    = 2)
#' 
boundary_layer_resistance <- function (T_a, 
                                       e_s, 
                                       e_a, 
                                       elev, 
                                       D, 
                                       u = NA) {
  
  stopifnot(T_a > 200, T_a < 400, e_s > 0, e_a > 0, elev > 0, D > 0)
  
  if(e_s < e_a) {
    
    stop("Actual vapor pressure, e_a, must be lower that saturation vapor pressure, e_s.")
    
  }
  
  gravity = 9.8 # m/s
  
  air_pressure <- 101325. * (1. - (2.2569 * 10^-5) * elev)^5.2553
  air_density <- air_pressure / (287.04 * T_a)
  dynamic_viscosity <- (1.8325 * 10^-5) * ((296.16 + 120.) / (T_a + 120.)) * ((T_a / 296.16)^1.5) #Tracy et al. 2010
  kinematic_viscosity <- dynamic_viscosity / air_density
  T_surface <- (T_a) * (1. + 0.38 * ((e_s * 1000.) / air_pressure)) #organism soil temperature in steady state heat balance
  T_air <- T_a * (1. + 0.38 * ((e_a * 1000.) / air_pressure)) #air temperature in steady state heat balance
  coef_thermal_expansion <- 1.0 / T_a
  
  # Grashof and Nusselt numbers
  Grashof <- (coef_thermal_expansion * gravity * (D^3) * (abs(T_surface - T_air))) / (kinematic_viscosity^2)
  Nusselt <- 0.48 * (Grashof)^0.25
  
  thermal_conductivity <- (2.4525*10^-2) + ((7.038 * 10^-5) * (T_a - 273.15))
  mixing_ratio <- (0.6257 * (e_a * 1000)) / (air_pressure - (1.006 * (e_a * 1000)))
 
  # free convection
  hc <- (Nusselt * thermal_conductivity) / D #free convective heat transfer coefficient
  
  if(!is.na(u)){ #check if wind speed is provided
    
    # estimate Reynolds number- ratio of interval viscous forces
    Re <- u * D / kinematic_viscosity
    
    # forced convection
    # use if Gr< 16 * Re^2
    if(Grashof <= 16 * Re^2) {
      
      hc <- 0.923 * (u^0.333 * D^(-0.666))
      
    }
    
  }
    
  # calculate boundary layer resistance 
  specific_heat <- (1004.84 + (1846.4 * mixing_ratio)) / (1 + mixing_ratio)
  
  0.93 * ((specific_heat * air_density) / hc) / 100

}


#' @title Humid Operative Temperature
#'
#' @description Calculate humid operative temperature, using adaptation of \insertCite{Campbell1998;textual}{TrenchR} \insertCite{Riddell2018}{TrenchR}.
#' 
#' @param r_i \code{numeric} internal (skin) resistance (\ifelse{html}{\out{s cm<sup>-1</sup>}}{\eqn{s cm^-1}{ASCII}}).
#' 
#' @param r_b \code{numeric} boundary layer resistance (\ifelse{html}{\out{s cm<sup>-1</sup>}}{\eqn{s cm^-1}{ASCII}}).
#' 
#' @param D \code{numeric} body diameter (meters), (diameter = 0.0016*log(mass) + 0.0061 for mass(g)).
#' 
#' @param T_a \code{numeric} ambient temperature (C).
#' 
#' @param elev \code{numeric} elevation (m).
#' 
#' @param e_s \code{numeric} saturation vapor pressure (kPa).
#' 
#' @param e_a \code{numeric} actual vapor pressure (kPa).
#' 
#' @param Qabs \code{numeric} Solar and thermal radiation absorbed (W).
#' 
#' @param epsilon \code{numeric} emissivity of salamander skin.
#' 
#' @return \code{numeric} humid operative temperature (C).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Eric Riddell
#' 
#' @examples
#'   Tb_salamander_humid(r_i     = 4, 
#'                       r_b     = 1, 
#'                       D       = 0.01, 
#'                       T_a     = 20, 
#'                       elev    = 500, 
#'                       e_a     = 2.0, 
#'                       e_s     = 2.5, 
#'                       Qabs    = 400, 
#'                       epsilon = 0.96)
#' 
Tb_salamander_humid <- function (r_i, 
                                 r_b, 
                                 D, 
                                 T_a, 
                                 elev, 
                                 e_a, 
                                 e_s, 
                                 Qabs, 
                                 epsilon = 0.96) {
  
  stopifnot(r_i > 0, r_b > 0, D > 0, elev > 0, e_s > 0, e_a > 0, epsilon > 0.5, epsilon <= 1)
  
  if(e_s < e_a) {
    
    stop("Actual vapor pressure, e_a, must be lower that saturation vapor pressure, e_s.")
    
  }
  
  # Stefan-Boltzmann constant
  sigma <- 5.673 * 10^(-8) # W m^(-2) K^(-4)
  
  vpd <- e_s - e_a # vapor pressure deficit
  
  #radiative conductance function, Campbell and Norman 1998
  radiative_conductance <- (4 * (5.670373 * 10^-8) * (T_a + 273.15)^3) / 29.3
 
  gamma <- 0.000666
  gvs <- 1 / ((r_i * 100.0) / 41.4)
  gva <- 1 / ((r_b * 100) / 41.4)
  gHa <- 1.4 * 0.135 * sqrt(.1 / D)
  gamma_naut <- gamma * ((radiative_conductance + gHa) / ((gvs * gva) / (gvs + gva)))
  s <- ((((17.502 * 240.97)) * 0.611 * exp((17.502 * T_a) / (T_a + 240.97))) / (240.97 + T_a)^2) / (101.3 * exp(-elev / 8200))
  
  T_a + (gamma_naut / (gamma_naut + s)) * (((Qabs - (epsilon * sigma * ((T_a + 273.15)^4))) / (29.3 * (radiative_conductance + gHa))) - (vpd / (gamma_naut * (101.3 * exp(-elev / 8200)))))

}

 
#' @title Absorbed Thermal Radiation
#' 
#' @description Estimate longwave (thermal) radiation (W) absorbed from the sky and the ground \insertCite{Campbell1998,Riddell2018}{TrenchR}.
#' 
#' @param T_a \code{numeric} air temperature (C).
#' 
#' @param T_g \code{numeric} ground temperature (C).
#' 
#' @param epsilon_ground \code{numeric} emissitivity (proportion) for more soil types \insertCite{Campbell1998}{TrenchR}.
#' 
#' @param a_longwave \code{numeric} absorptance (proportion) of organism to longwave radiation (\insertCite{Bartlett1967}{TrenchR} and Buckley 2008).
#' 
#' @return \code{numeric} thermal radiation absorbed (W).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Eric Riddell
#' 
#' @examples
#'   Qthermal_radiation_absorbed(T_a            = 20, 
#'                               T_g            = 25, 
#'                               epsilon_ground = 0.97, 
#'                               a_longwave     = 0.965)
#' 
Qthermal_radiation_absorbed <- function (T_a, 
                                         T_g, 
                                         epsilon_ground = 0.97, 
                                         a_longwave = 0.965) {
  
  stopifnot(epsilon_ground >= 0, epsilon_ground <= 1, a_longwave >= 0, a_longwave <= 1)
  
  # Stefan-Boltzmann constant
  sigma <- 5.673 * 10^(-8) # W m^(-2) K^(-4)
  
  # longwave radiation from sky function, Campbell and Norman 1998
  Slongwave_sky <- 53.1 * 10^-14 * (T_a + 273.15)^6.
  
  # longwave radiation from ground function, Campbell and Norman 1998
  Slongwave_ground <- epsilon_ground * sigma * (T_g + 273.15)^4.
  
  # radiation absorbed function, adapted from Campbell and Norman 1998
  0.5 * a_longwave * (Slongwave_sky + Slongwave_ground)

}


#' @title Approximate Soil Temperature
#'
#' @description Estimate soil temperature at a given depth and hour approximating diurnal variation as sinusoidal; adapted from \insertCite{Campbell1998;textual}{TrenchR} \insertCite{Riddell2018}{TrenchR}.
#' 
#' @param Tg_max \code{numeric} daily maximum soil surface temperature (C).
#' 
#' @param Tg_min \code{numeric} daily minimum soil surface temperature (C).
#' 
#' @param hour \code{numeric} hour of the day.
#' 
#' @param depth \code{numeric} depth (cm).
#' 
#' @return \code{numeric} soil temperature (C).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @author Eric Riddell
#' 
#' @examples
#'   Tsoil(Tg_max = 30,
#'         Tg_min = 15, 
#'         hour   = 12, 
#'         depth  = 5)
#' 
Tsoil <- function (Tg_max,
                   Tg_min, 
                   hour, 
                   depth) {
 
  stopifnot(Tg_max > Tg_min, depth >= 0)
  
  offset <- ifelse(hour %in% c(0, 1, 2, 3), -13, 11)
 
  ((Tg_max + Tg_min) / 2.0) + ((Tg_max - Tg_min) / 2.0) * (2.71**(-depth / 5.238)) * sin((3.14 / 12.) * (hour - offset) - depth / 5.238)

}


#' @title Nusselt Number
#'
#' @description Estimate the Nusselt Number, which describes dimensionless conductance \insertCite{Gates1980}{TrenchR}.
#' 
#' @param H_L \code{numeric} convective heat transfer coefficient (\ifelse{html}{\out{W m<sup>-2</sup> K<sup>-1</sup>}}{\eqn{W m^-2 K^-1}{ASCII}}).
#' 
#' @param D \code{numeric} characteristic dimension (e.g., body diameter) (m).
#' 
#' @param K \code{numeric} thermal conductivity (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}}).
#' 
#' @return \code{numeric} Nusselt number.
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Nusselt_number(H_L = 20, 
#'                  D   = 0.01,
#'                  K   = 0.5)
#' 
Nusselt_number <- function(H_L, 
                           D, 
                           K) {
  
  stopifnot(H_L > 0, D > 0, K > 0)
  
  H_L * D / K #eq 9.24

}


#' @title Prandtl Number
#'
#' @description Estimate the Prandtl Number, which describes the ratio of kinematic viscosity to thermal diffusivity \insertCite{Gates1980}{TrenchR}.
#' 
#' @param c_p \code{numeric} specific heat at constant pressure (\ifelse{html}{\out{J mol<sup>-1</sup> K<sup>-1</sup>}}{\eqn{J mol^-1 K^-1}{ASCII}}).
#' 
#' @param mu \code{numeric} dynamic viscosity (\ifelse{html}{\out{mol s<sup>-1</sup> m<sup>-1</sup>}}{\eqn{mol s^-1 m^-1}{ASCII}}).
#' 
#' @param K \code{numeric} thermal conductivity (\ifelse{html}{\out{W K<sup>-1</sup> m<sup>-1</sup>}}{\eqn{W K^-1 m^-1}{ASCII}}).
#' 
#' @return \code{numeric} Prandtl number.
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Prandtl_number(c_p = 29.3, 
#'                  mu  = 0.00001, 
#'                  K   = 0.5)
#' 
Prandtl_number <- function (c_p, 
                            mu, 
                            K) {
  
  stopifnot(c_p > 0, mu > 0, K > 0)
  
  c_p * mu / K #eq 9.26

}


#' @title Reynolds Number
#'
#' @description Estimate the Reynolds Number, which describes the dynamic properties of the fluid surrounding the animal as the ratio of internal viscous forces \insertCite{Gates1980}{TrenchR}.
#' 
#' @param D \code{numeric} characteristic dimension (e.g., body diameter) (m)
#' 
#' @param u \code{numeric} wind speed (\ifelse{html}{\out{m s<sup>-1</sup>}}{\eqn{m s^-1}{ASCII}}).
#' 
#' @param nu \code{numeric} the kinematic viscosity, ratio of dynamic viscosity to density of the fluid (\ifelse{html}{\out{m<sup>2</sup> s<sup>-1</sup>}}{\eqn{m^2 s^-1}{ASCII}}); can calculate from \code{\link{DRYAIR}} or \code{\link{WETAIR}}.
#' 
#' @return \code{numeric} Reynolds number.
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Reynolds_number(u  = 1, 
#'                   D  = 0.001, 
#'                   nu = 1.2)
#' 
Reynolds_number <- function(u, 
                            D, 
                            nu) {
  
  stopifnot(D >= 0, u >= 0, nu >= 0)
  
  u * D / nu #eq 9.25

}


#' @title Grashof Number
#'
#' @description Estimate the Grashof Number, which describes the ability of a parcel of fluid warmer or colder than the surrounding fluid to rise against or fall with the attractive force of gravity. Ratio of a buoyant force times an inertial force to the square of a viscous force \insertCite{Campbell1998}{TrenchR}.
#' 
#' @param Ta \code{numeric} Air temperature (C).
#' 
#' @param Tg \code{numeric} ground (surface) temperature (C).
#' 
#' @param D \code{numeric} characteristic dimension (e.g., body diameter) (meters).
#' 
#' @param nu \code{numeric} the kinematic viscosity, ratio of dynamic viscosity to density of the fluid (\ifelse{html}{\out{m<sup>2</sup> s<sup>-1</sup>}}{\eqn{m^2 s^-1}{ASCII}}); can calculate from \code{\link{DRYAIR}} or \code{\link{WETAIR}}.
#' 
#' @return \code{numeric} Grashof number.
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Grashof_number(Ta = 30, 
#'                  Tg = 35, 
#'                  D  = 0.001, 
#'                  nu = 1.2)
#' 
Grashof_number <- function (Ta, 
                            Tg, 
                            D, 
                            nu) {
  
  stopifnot(D >= 0, nu >= 0)
  
  # constant
  gravity <- 9.8 # meters per second
  
  gravity * D^3 * abs(Tg - Ta) / (Ta * nu^2)

}


#' @title Grashof Number in Gates
#'
#' @description Estimate the Grashof Number, which describes the ability of a parcel of fluid warmer or colder than the surrounding fluid to rise against or fall with the attractive force of gravity \insertCite{Gates1980}{TrenchR}. Ratio of a buoyant force times an inertial force to the square of a viscous force.
#' 
#' @param Ta \code{numeric} Air temperature (C).
#' 
#' @param Tg \code{numeric} Ground (surface) temperature (C).
#' 
#' @param beta \code{numeric} coefficient of volumetric thermal expansion, \code{beta} = 3.67 x \ifelse{html}{\out{10<sup>-3</sup> C<sup>-1</sup>}}{\eqn{10^-3 C^-1}{ASCII}}  in air and 41.9 x \ifelse{html}{\out{10<sup>-4</sup> C<sup>-1</sup>}}{\eqn{10^-4 C^-1}{ASCII}} in water.
#' 
#' @param D \code{numeric} is characteristic dimension (e.g., body diameter) (m)
#' 
#' @param nu \code{numeric} is the kinematic viscosity, ratio of dynamic viscosity to density of the fluid (\ifelse{html}{\out{m<sup>2</sup> s<sup>-1</sup>}}{\eqn{m^2 s^-1}{ASCII}}); can calculate from \code{\link{DRYAIR}} or \code{\link{WETAIR}}.
#' 
#' @return \code{numeric} Grashof number
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Grashof_number_Gates(Ta   = 30, 
#'                        Tg   = 35, 
#'                        beta = 0.00367,
#'                        D    = 0.001, 
#'                        nu   = 1.2)
#' 
Grashof_number_Gates <- function (Ta, 
                                  Tg, 
                                  beta, 
                                  D, 
                                  nu) {

  stopifnot(beta > 0, D >= 0, nu > 0)
  
  gravity <- 9.8 # m/s
  
  gravity * beta * D^3 * abs(Tg - Ta) / nu^2
  
}


#' @title Estimate the Nusselt Number from the Reynolds Number
#' 
#' @description Estimate the Nusselt number from the Reynolds number for various taxa \insertCite{Mitchell1976;textual}{TrenchR} Table 1 (Convective Heat Transfer Relations for Animal Shapes).  
#' 
#' @param Re \code{numeric} Reynolds Number (dimensionless).
#' 
#' @param taxa \code{character} which class of organism, current choices: sphere, cylinder, frog, lizard_traverse_to_air_flow, lizard_parallel_to_air_flow, lizard_surface, lizard_elevated, flyinginsect, spider.
#' 
#' @return \code{numeric} Nusselt number (dimensionless).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Nusselt_from_Reynolds(Re   = 5, 
#'                         taxa = "cylinder")
#' 
Nusselt_from_Reynolds <- function (Re, 
                                   taxa = "cylinder") {
  
  taxas <- c("sphere", "cylinder", "frog", "lizard_traverse_to_air_flow", "lizard_parallel_to_air_flow", "lizard_surface", "lizard_elevated", "flyinginsect", "spider")
  stopifnot(taxa %in% taxas)
  
  # Dimensionless constant (Cl)
  Cls <- c(0.37, 0.615, 0.258, 0.35, 0.1, 1.36, 1.91, 0.0749, 0.47)
  ns <- c(0.6, 0.466, 0.667, 0.6, 0.74, 0.39, 0.45, 0.78, 0.5) 
  
  # find index  
  ind <- match(taxa, taxas)
  
  Cls[ind] * Re^(ns[ind])
  
}


#' @title Estimate the Nusselt Number from the Grashof Number 
#' 
#' @description Estimate the Nusselt number from the Grashof Number \insertCite{Gates1980}{TrenchR}.
#' 
#' @param Gr \code{numeric} Grashof Number (dimensionless).
#' 
#' @return \code{numeric} Nusselt number (dimensionless).
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   Nusselt_from_Grashof(Gr = 5)
#' 
Nusselt_from_Grashof <- function (Gr) {
  
  0.48 * Gr^0.25
  
}


#' @title Determine If Convection is Free or Forced 
#' 
#' @description Compare the Grashof and Reyolds numbers to determine whether convection is free or forced \insertCite{Gates1980}{TrenchR}.
#' 
#' @param Gr \code{numeric} is the Grashof Number (dimensionless).
#' 
#' @param Re \code{numeric} is the Reynolds Number (dimensionless).
#' 
#' @return \code{character} \code{"free"}, \code{"forced"}, or \code{"intermediate"}.
#' 
#' @family biophysical models
#' 
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#'   free_or_forced_convection(Gr = 100, 
#'                             Re = 5)
#' 
free_or_forced_convection <- function (Gr, 
                                       Re) {
  
  conv <- "intermediate condition, mixed convection based on Nusselt numbers is appropriate"
  
  if(Gr < 0.1 * Re^2) {
    
    conv <- "forced convection" #P284
    
  }
  
  if(Gr > 16 * Re^2) {
    
    conv <- "free convection"
    
  }
  
  conv
  
}
